#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WDCM Engine (S)itelinks
### --- Version 1.0.0
### --- Script: WDCM_Engine_Sitelinks.R
### --- June 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: The script represents the update engine 
### --- for the WDCM (S)itelinks dashboard.
### --- NOTE: the execution of this WDCM script is always dependent upon the
### --- previous WDCM_Sqoop_Clients.R run
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
### ---
### --- WDCM is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDCM is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDCM. If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- Section 1. ETL from wdcm_clients_wb_entity_usage
### --- Map-Reduce w. HiveQL
### --- Goal: Extract entity-project matrices
### --- Constraints:
### --- 1. All 14 WDCM semantic categories
### --- 2. Only Wikipedia projects that make use of all 14 categories
### --- 3. (S)itelinks (S entity usage aspects) only
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- FULL WDCM Engine (S)itelinks update STARTED ON:", Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- setup
library(data.table)
library(tidyverse)
library(RColorBrewer)
library(plotrix)
library(maptpx)
library(snowfall)
library(httr)
library(jsonlite)
library(htmltab)
library(XML)
library(parallelDist)
library(mclust)
library(networkD3)

### --- directories
### --- Read WDCM paramereters
# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = T)
fPath <- unlist(strsplit(fPath, split = "/", fixed = T))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")
params <- xmlParse(paste0(fPath, "wdcmConfig.xml"))
params <- xmlToList(params)

### --- Directories
ontologyDir <- params$sitelinks$ontologyDir
logDir <- params$sitelinks$logDir
itemsDir <- params$sitelinks$itemsDir
structureDir <- params$general$structureDir
publicDir <- params$sitelinks$publicDataDir
localDataDir <- params$sitelinks$localDataDir

### --- Set proxy
Sys.setenv(
  http_proxy = params$general$http_proxy,
  https_proxy = params$general$https_proxy)

### --- utilities
wdcm_batchIndexes <- function(x, batchSize) {
  if (class(batchSize) != "integer") {
    message("Error: batchSize not integer.")
    break()
  } else if (!is.vector(x) | length(x) < 100) {
    message("Error: x not vector or length(x) < 100.")
    break()
  } else {
    batchNum <- ceiling(length(x)/batchSize)
    startBatchIx <- c(1:batchNum) * batchSize - batchSize + 1
    stopBatchIx <- c(1:batchNum) * batchSize
    stopBatchIx[batchNum] <- length(x)
    bIx <- vector(mode = "list", length = length(startBatchIx))
    for (i in 1:length(bIx)) {
      bIx[[i]]$start <- startBatchIx[i]
      bIx[[i]]$stop <- stopBatchIx[i]
    }
    return(bIx)
  }
}

# - projectType() to determine project type
projectType <- function(projectName) {
  unname(sapply(projectName, function(x) {
    if (grepl("commons", x, fixed = T)) {"Commons"
    } else if (grepl("mediawiki|meta|species|wikidata", x)) {"Other"
    } else if (grepl("wiki$", x)) {"Wikipedia"
    } else if (grepl("quote$", x)) {"Wikiquote"
    } else if (grepl("voyage$", x)) {"Wikivoyage"
    } else if (grepl("news$", x)) {"Wikinews"
    } else if (grepl("source$", x)) {"Wikisource"
    } else if (grepl("wiktionary$", x)) {"Wiktionary"
    } else if (grepl("versity$", x)) {"Wikiversity"
    } else if (grepl("books$", x)) {"Wikibooks"
    } else {"Other"}
  }))
}

# - topicCoherence_tdm() - compute topic coherence 
# - for a full topic model
topicCoherence_tdm <- function(tdm, theta, M, normalized = T) {
  
  # - tdm: a term-document matrix (columns = terms, rows = documents)
  # - theta: The num(terms) by num(topics) matrix of estimated topic-phrase probabilities
  # - M: number of top topic terms to use to compute coherence
  
  # - constant to add to joint probabilities
  # - (avoid log(0))
  epsilon <- 1e-12
  
  # - select top term subsets from each topic
  topTerms <- apply(theta, 2, function(x) {
    names(sort(x, decreasing = T)[1:M])
  })
  
  # - compute topic coherences
  nmpi <- apply(topTerms, 2, function(x) {
    
    # - compute Normalized Pairwise Mutual Information (NMPI)
    wT <- which(colnames(tdm) %in% x)
    # - term probabilities
    pT <- colSums(tdm[, wT])/sum(tdm)
    # - term joint probabilities
    terms <- colnames(tdm)[wT]
    bigS <- sum(tdm)
    jpT <- lapply(terms, function(y) {
      cmpTerms <- setdiff(terms, y)
      p <- lapply(cmpTerms, function(z) {
        mp <- sum(apply(tdm[, c(y, z)], 1, min))/bigS
        names(mp) <- z
        return(mp)
      })
      p <- unlist(p)
      return(p)
    })
    names(jpT) <- terms
    # - produce all pairs from terms
    pairTerms <- combn(terms, 2)
    # - compute topic NMPI
    n_mpi <- vector(mode = "numeric", length = dim(pairTerms)[2])
    for (i in 1:dim(pairTerms)[2]) {
      p1 <- pT[which(names(pT) %in% pairTerms[1, i])]
      p2 <- pT[which(names(pT) %in% pairTerms[2, i])]
      p12 <- jpT[[which(names(jpT) %in% names(p1))]][names(p2)]
      p12 <- p12 + epsilon
      # - if NMPI is required (default; normalized = T)
      if (normalized == T) {
        n_mpi[i] <- log2(p12/(p1*p2))/(-log2(p12)) 
      } else {
        # - if PMI is required (normalized = F)
        n_mpi[i] <- log2((p1*p2)/p12)
      }
    }
    # - aggregate in topic
    n_mpi <- mean(n_mpi)
    return(n_mpi)
  })
  
  # - aggregate across topics:
  # - full topic model coherence
  return(mean(nmpi))
}

### ---------------------------------------------------------------------------
### --- Section 0. Collect WDCM Ontology Wikidata items
### ---------------------------------------------------------------------------

### --- collect WDCM Wikidata items:
source(paste0(fPath, 'wdcmModule_CollectItems_AuxSystems.R'))

### --- everything from localDataDir
setwd(localDataDir)

### ---------------------------------------------------------------------------
### --- Section 1. Produce project-entity matrices
### ---------------------------------------------------------------------------

### --- ETL from goransm.wdcm_clients_wb_entity_usage
### --- w. HiveQL from Beeline
filename <- "siteLinksData.tsv"
queryFile <- "siteLinksData_HiveQL_Query.hql"
kerberosPrefix <- 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata '
hiveQLquery <- 'SET hive.mapred.mode=unstrict; 
  SELECT wiki_db, eu_entity_id, COUNT(*) AS eu_usage FROM goransm.wdcm_clients_wb_entity_usage 
  WHERE ((eu_aspect = \'S\') AND (wiki_db RLIKE \'wiki$\') AND (wiki_db != \'commonswiki\')) 
  GROUP BY wiki_db, eu_entity_id ORDER BY wiki_db, eu_usage DESC;'
# - write hql
write(hiveQLquery, queryFile)
# - to Report
print("Fetching (S)itelinks data from wdcm_clients_wb_entity_usage now.")
# - Kerberos init
system(command = paste0(kerberosPrefix, ' hdfs dfs -ls'), 
       wait = T)
# - Run query
query <- system(command = paste(kerberosPrefix, 
                                '/usr/local/bin/beeline --incremental=true --silent -f "',
                                paste0(localDataDir, queryFile),
                                '" > ', localDataDir, filename,
                                sep = ""),
                wait = TRUE)
# - to Report
print("DONE w. ETL from Hadoop: wdcm_clients_wb_entity_usage.")
print("DONE w. siteLinksData production.")

### --- clear localDataDir
lfs <- list.files()
lfs <- setdiff(lfs, 'siteLinksData.tsv')
file.remove(lfs)

### --- load and annotate siteLinksData
siteLinksData <- fread('siteLinksData.tsv', 
                       sep = "\t",
                       quote = "")

### --- filter siteLinksData for projects
### --- from List of Wikipedias Wiki page:
### --- scrape: https://en.wikipedia.org/wiki/List_of_Wikipedias
url <- "https://en.wikipedia.org/wiki/List_of_Wikipedias"
listWiki <- htmltab(url, 3)
# - extract all languages
allLanguages <- listWiki$Wiki
# - filter siteLinksData for these languages:
siteLinksLanguages <- siteLinksData$wiki_db
wL <- which(siteLinksLanguages %in% paste0(allLanguages, "wiki"))
if (length(wL) > 0) {
  siteLinksData <- siteLinksData[wL, ]
}

# - filter according to Wikidata usage
wdUsage <- siteLinksData %>% 
  group_by(wiki_db) %>% 
  summarise(usage = sum(eu_usage)) %>% 
  arrange(desc(usage))
cutOff <- median(wdUsage$usage)
wWDmed <- wdUsage$wiki_db[which(wdUsage$usage >= cutOff)]
siteLinksData <- siteLinksData %>% 
  filter(wiki_db %in% wWDmed)

### --- anotate siteLinksData by WDCM semantic categories
siteLinksData$category <- integer(dim(siteLinksData)[1])
# - to itemsDir
setwd(itemsDir)
lFitems <- list.files()
# - iterate, annotate
for (i in 1:length(lFitems)) {
  # to Report:
  print(paste0("Annotating siteLinksData from: ", i, ": ", lFitems[i], "."))
  items <- tryCatch({
    fread(lFitems[i], header = T)
    },
    error = function(condition) {
      print("Ooops - can't read this category. Skipping.")
      break
      }
  )
  items$V1 <- NULL
  colnames(items) <- c('items', 'category')
  items <- items$item
  # to Report:
  print(paste0("Annotating ", length(items), " items."))
  siteLinksData$category[which(siteLinksData$eu_entity_id %in% items)] <- i
  # to Report:
  print(paste0("Annotated ", sum(siteLinksData$category == i), " rows in siteLinksData."))
  rm(items); gc()
}
# - to Report
print("DONE w. siteLinksData annotation.")
### --- back to localDataDir
setwd(localDataDir)

### --- drop non-WDCM terms
# - to Report
print("Drop non-WDCM entities.")
siteLinksData <- siteLinksData[siteLinksData$category != 0, ]
print("DONE w. drop non-WDCM items.")

### --- Remove WDCM semantic class: Wikimedia Internals
# - to Report
print("Remove WDCM semantic class: Wikimedia Internals")
wWMInternals <- which(grepl("Wikimedia_Internal", lFitems))
siteLinksData <- siteLinksData %>% 
  dplyr::filter(category != wWMInternals)
if (length(wWMInternals) > 0) {
  lFitems <- lFitems[-wWMInternals]
}
print("DONE w. Remove WDCM semantic class: Wikimedia Internals.")

### --- what Wikipedia projects are present
### --- in at least 10 out of 12 (13 minus Wikimedia Internals) 
### --- WDCM semantic categories: a selection.
wikiSelection <- table(siteLinksData$wiki_db, siteLinksData$category)
wikiSelectionIx <- apply(wikiSelection, 1, function(x) {sum(x > 0, na.rm = T)})
selectedWikies <- names(which(wikiSelectionIx >= 10))
colnames(wikiSelection) <- unname(sapply(lFitems, function(x) {
  strsplit(x, split = "_")[[1]][1]
}))
wikiSelection <- wikiSelection[which(rownames(wikiSelection) %in% selectedWikies), ]
# - store wikiSelection: frequency of 'S' item usage
# - across all Wikipedia projects
write.csv(wikiSelection, 'Wikipedia_wdcmSUsage_CategoryOverview.csv')
# - filter siteLinksData for selectedWikies
siteLinksData <- siteLinksData %>% 
  filter(wiki_db %in% selectedWikies)

### --- produce item frequency matrices
# - keep track of siteLinks category size:
lFitems <- gsub("-", " ", lFitems)
siteLinksCategoryNum <- numeric()
categoryCode <- sort(unique(siteLinksData$category))
for (i in 1:length(lFitems)) {
  # - to Report
  print(paste0("Producing freqMatrix for: ", i, ": ", lFitems[i]))
  # - select category
  fMatrix <- data.table(
    siteLinksData[siteLinksData$category == categoryCode[i], c('wiki_db', 'eu_entity_id', 'eu_usage')]
  )
  # - aggregate per item and arrange descending by frequency
  fMatrix <- fMatrix[, .(eu_usage = sum(eu_usage)), by = eu_entity_id]
  fMatrix <- fMatrix[order(-eu_usage)]
  siteLinksCategoryNum <- append(siteLinksCategoryNum, dim(fMatrix)[1])
  # - to Report
  print(paste0("There are ", dim(fMatrix)[1], " unique items. Saving..."))
  # - save
  filename <- paste0(gsub("_ItemIDs.csv", "", lFitems[i]), "_freqMatrix.csv")
  write.csv(as.data.frame(fMatrix), filename)
  # - to Report
  print("DONE.")
  # - clear:
  rm(fMatrix)
}
print("DONE w. frequency matrices.")

### --- produce project-entity matrices
### --- NOTE: AN ARBITRARY DECISION TO SELECT TOP 10,000 MOST FREQUENTLY USED ITEMS
### --- CRITERIA:
### --- (0) filter out any Wikipedia that does not
### --- make use of at least 10 out of 13 (14 minus Wikimedia Internals)
### --- WDCM semantic categories;
### --- (1) for each matrix, we pick the top 10,000 most
### --- frequently used items;
### --- (2) if a WDCM semantic category has less than
### --- 10,000 entities, we pick them all;
### --- (3) if a WDCM semantic category has less than
### --- 100 entities, drop the category from further analyses;
### --- (4) remove from the project-entity matrix
### --- any item that is not used in at least 50% of projects

# - iterate over WDCM semantic categories
# - and apply criteria (1), (2), and (3)
lFcategories <- list.files()
lFcategories <- lFcategories[grepl("_freqMatrix", lFcategories)]
for (i in 1:length(lFcategories)) {
  # - read semantic category frequency matrix
  frMat <- fread(lFcategories[i])
  # - to Report:
  print(paste0("Project-entity matrix for: ", lFcategories[i]))
  # - discard if there are less than 100 items
  if (dim(frMat)[1] >= 100) {
    # - produce project-entity matrix
    if (dim(frMat)[1] <= 10000) {
      # - case 1: there are <= 10,000 items
      # - to Report:
      print("Found <= 10,000 items; selecting all.")
      # - select semantic category from siteLinksData
      categoryFile <- siteLinksData %>% 
        dplyr::filter(eu_entity_id %in% frMat$eu_entity_id)
      # - drop category column
      categoryFile$category <- NULL
      # - reshape categoryFile
      # - to Report:
      print("Reshaping now.")
      categoryFile <- spread(categoryFile,
                             key = eu_entity_id,
                             value = eu_usage,
                             fill = 0)
      rownames(categoryFile) <- categoryFile$wiki_db
      categoryFile$wiki_db <- NULL
      # - to Report:
      print("Checking now for empty projects or entities.")
      # - remove entities that are not used at all
      w <- which(colSums(categoryFile) == 0)
      if (length(w) > 0) {
        categoryFile <- categoryFile[, -w]
      }
      # - remove wikies that are not mentioned, if any at all
      w <- which(rowSums(categoryFile) == 0)
      if (length(w) > 0) {
        categoryFile <- categoryFile[-w, ]
      }
      # - to Report:
      print("Removing all items that are not mentioned in at least 10% projects.")
      # - remove all items that are not used in at least 10% of projects
      itemUse <- apply(categoryFile, 2, function(x) {
        sum(x > 0, na.rm = T)
      })
      wItemUse <- which(itemUse >= round(dim(categoryFile)[1]/10))
      categoryFile <- categoryFile[, wItemUse]
      # - to Report: Final matrix dimesionality
      print(paste0("Final matrix dimesionality: ", 
                   dim(categoryFile)[1], 
                   " projects, ", 
                   dim(categoryFile)[2], 
                   " entities."))
      print("---------")
      # - save project-entity matrix
      if (dim(categoryFile)[2] > 0) {
        fileName <- paste0(
          strsplit(lFcategories[i], split = "_")[[1]][1],
          "_sitelinks_tfMatrix.csv")
        write.csv(categoryFile, fileName)
      }
      # - clear
      rm(categoryFile)
    } else {
      # - case 2: there are >= 10,000 items
      # - to Report:
      print("Found > 10,000 items; selecting top 10,000 most frequent.")
      # - select semantic category from siteLinksData
      categoryFile <- siteLinksData %>% 
        dplyr::filter(eu_entity_id %in% frMat$eu_entity_id[1:10000])
      # - drop category column
      categoryFile$category <- NULL
      # - reshape categoryFile
      # - to Report:
      print("Reshaping now.")
      categoryFile <- spread(categoryFile,
                             key = eu_entity_id,
                             value = eu_usage,
                             fill = 0)
      rownames(categoryFile) <- categoryFile$wiki_db
      categoryFile$wiki_db <- NULL
      # - to Report:
      print("Checking now for empty projects or entities.")
      # - remove entities that are not used at all
      w <- which(colSums(categoryFile) == 0)
      if (length(w) > 0) {
        categoryFile <- categoryFile[, -w]
      }
      # - remove wikies that are not mentioned, if any at all
      w <- which(rowSums(categoryFile) == 0)
      if (length(w) > 0) {
        categoryFile <- categoryFile[-w, ]
      }
      # - to Report:
      print("Removing all items that are not mentioned in at least 10% projects.")
      # - remove all items that are not used in at least 10% of projects
      itemUse <- apply(categoryFile, 2, function(x) {
        sum(x > 0, na.rm = T)
      })
      wItemUse <- which(itemUse >= round(dim(categoryFile)[1]/10))
      categoryFile <- categoryFile[, wItemUse]
      # - to Report: Final matrix dimesionality
      print(paste0("Final matrix dimesionality: ", 
                   dim(categoryFile)[1], 
                   " projects, ", 
                   dim(categoryFile)[2], 
                   " entities."))
      print("---------")
      # - save project-entity matrix
      if (dim(categoryFile)[2] > 0) {
        fileName <- paste0(
          strsplit(lFcategories[i], split = "_")[[1]][1],
          "_sitelinks_tfMatrix.csv")
        write.csv(categoryFile, fileName)
      }
      # - clear
      rm(categoryFile)
    }
  } else {
    print("Found less than 100 items; discarding semantic category.")
  }
}

# - store siteLinksData as processed:
write.csv(siteLinksData, "siteLinksData_Processed.csv")
# - clear: siteLinksData
rm(siteLinksData); gc()

### ---------------------------------------------------------------------------
### --- Section 2. Latent Dirichlet Allocation w. project-entity matrices
### ---------------------------------------------------------------------------

### --- Working w. top 1,000 frequently mentioned items SitelinksData only

# - list project-entity matrices
setwd(localDataDir)
lFtmat <- list.files()
lFtmat <- lFtmat[grepl("tfMatrix", lFtmat)]

# - iterate across project-entity matrices
# - optimal {mattpx} topic model for each matrix
# - store output matrices
t1General <- Sys.time()
for (i in 1:length(lFtmat)) {
  
  # - to Report
  t1Category <- Sys.time()
  
  # - prepare project-entity matrix
  itemCat <- fread(lFtmat[i])
  # - top 1,000 frequently mentioned items Sitelinks only:
  if (dim(itemCat)[2] > 1001) {
    iF <- colSums(itemCat %>% 
                    dplyr::select(dplyr::starts_with('Q')))
    iF <- names(sort(iF, decreasing = T)[1:1000])
    itemCat <- as.data.frame(itemCat)[, c(1, which(colnames(itemCat) %in% iF))]
  }
  
  # - check if there are less than 100 items to model:
  if (dim(itemCat)[2] >= 101) {
  
    categoryName <- strsplit(lFtmat[i], split = ".", fixed = T)[[1]][1]
    categoryName <- strsplit(categoryName, split = "_", fixed = T)[[1]][1]
    
    wikis <- itemCat$V1
    itemCat$V1 <- NULL
    w <- which(rowSums(itemCat) == 0)
    if (length(w) > 0) {
      itemCat <- itemCat[-w, ]
      wikis <- wikis[-w]
    }
    itemCat <- as.matrix(itemCat)
    
    # - to Report:
    print(paste("----------------------- {maptpx} LDA model: category ", i, ". ", categoryName, sep = ""))
    
    ####### ----------- PARALLEL w. {snowfall} STARTS
    
    # - start cluster and export data + package
    sfInit(parallel = T, cpus = 20)
    sfExport("itemCat")
    sfLibrary(maptpx)

    ## -- run 20 k = 10 topic models
    numTopics <- rep(c(2:20), times = 5)
    topicModels <- sfClusterApplyLB(numTopics,
                                    function(x) {
                                      maptpx::topics(counts = itemCat,
                                                     K = x, bf = T,
                                                     shape = NULL, initopics = NULL,
                                                     tol = .01, kill = 0,
                                                     ord = TRUE, verb = 0)
                                      }
                                    )

    # - stop cluster
    sfStop()

    ####### ----------- PARALLEL w. {snowfall} ENDS
    
    # - determine model from topic coherence
    # - toReport
    print("Topic coherence based model selection... ")
    # - define M = max. top terms for coherence computation
    M <- 15
    thetas <- lapply(topicModels, function(x) {x$theta})
    tC <- sapply(thetas, function(x) {
      topicCoherence_tdm(itemCat, x, M, normalized = T)
    })
    wModel <- which.max(tC)[1]
    topicModel <- topicModels[[wModel]]
    
    # - clear:
    rm(itemCat); gc()

    # - collect matrices
    # - entities x topics
    wdcm_itemtopic <- as.data.frame(topicModel$theta)
    colnames(wdcm_itemtopic) <- paste("topic", seq(1, dim(wdcm_itemtopic)[2]), sep = "")
    itemTopicFileName <- paste0(categoryName, "_sitelinks_itemtopic.csv")
    write.csv(wdcm_itemtopic, itemTopicFileName)
    # - wikis x topics
    wdcm_projecttopic <- as.data.frame(topicModel$omega)
    colnames(wdcm_projecttopic) <- paste("topic", seq(1, dim(wdcm_projecttopic)[2]), sep = "")
    rownames(wdcm_projecttopic) <- wikis
    projectTopicFileName <- paste0(categoryName, "_sitelinks_wikitopic.csv")
    write.csv(wdcm_projecttopic, projectTopicFileName)
    
    # - toReport
    print(paste0("Selected model has: ", topicModel$K, " topics."))
    
    # - clear:
    rm(topicModel); rm(wdcm_projecttopic); rm(wdcm_itemtopic); gc()
    
    # - to Report:
    print("DONE.")
    # - to Report
    print("--- Category LDA completed --- ")
    print(paste0("Category LDA time: ", Sys.time() - t1Category))
  
  }
  
}

# - to Report
print("--- LDA completed --- ")
print(paste0("Total LDA time: ", Sys.time() - t1General))

### ---------------------------------------------------------------------------
### --- Section 3. Annotate topic models and project important properties
### ---------------------------------------------------------------------------

### --- Annotate topic models
lFtmod <- list.files()
lFtmod <- lFtmod[grepl("itemtopic", lFtmod)]
# - Wikidata MediaWiki API prefix
APIprefix <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'
# - get item labels for item-topic matrixes
for (i in 1:length(lFtmod)) {
  # load item-topic matrix
  itMatrix <- fread(lFtmod[i])
  # - compose API call
  items <- itMatrix$V1
  # - perform API calls for item labels
  # - fetch item labels in batches (max values = 50, MediaWiki API constraint)
  c <- 0
  ixStart <- 1
  iLabs <- list()
  # - # - to Report
  print(paste0("Fetching from API item labels for: ", lFtmod[i]))
  repeat {
    ixEnd <- ixStart + 50 - 1
    searchItems <- items[ixStart:ixEnd]
    w <- which(is.na(searchItems))
    if (length(w) > 0) {searchItems <- searchItems[-w]}
    ids <- paste(searchItems, collapse = "|")
    query <- paste0(APIprefix, 
                    'ids=', ids, '&',
                    'props=labels&languages=en&sitefilter=wikidatawiki&languagefallback=true&format=json')
    res <- tryCatch(
      {
        GET(url = URLencode(query))
      },
      error = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      },
      warning = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      }
    )
    rc <- rawToChar(res$content)
    rc <- fromJSON(rc)
    itemLabels <- unlist(lapply(rc$entities, function(x) {
      x$labels$en$value
    }))
    itemLabels <- data.frame(eu_entity_id = names(itemLabels), 
                             eu_label = itemLabels, 
                             stringsAsFactors = F, 
                             row.names = c())
    c <- c + 1
    iLabs[[c]] <- itemLabels
    if (length(searchItems) < 50) {
      break
    } else {
      ixStart <- ixStart + 50
      # - pause here 2 secs
      Sys.sleep(1)
    }
  }
  rm(res); rm(rc); gc()
  iLabs <- rbindlist(iLabs)
  # - enter item labels to itMatrix
  itMatrix <- dplyr::left_join(itMatrix, iLabs, 
                               by = c("V1" = "eu_entity_id"))
  colnames(itMatrix)[1] <- "eu_entity_id"
  wNALabel <- which(is.na(itMatrix$eu_label))
  if (length(wNALabel) > 0) {
    itMatrix$eu_label[wNALabel] <- 
      itMatrix$eu_entity_id[wNALabel]
    }
  # - store itMatrix
  write.csv(itMatrix, lFtmod[i])
  # - to Report
  print("DONE.")
  print("Sleep for 10 secs.")
  Sys.sleep(10)
}

### --- Project important properties onto item x topics matrices
# - fetch only immediate P31 classes
# - for the top M most important items in each topic.

lFtmod <- list.files()
lFtmod <- lFtmod[grepl("itemtopic", lFtmod)]

# - Wikidata MediaWiki API prefix
APIprefix <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'

### --- (P31) onto topic models
for (i in 1:length(lFtmod)) {
  
  # - load item-topic matrix
  itMatrix <- fread(lFtmod[i])
  itMatrix$V1 <- NULL

  # - extract top M most imporant items
  # - NOTE: use here the same M as used for topic coherence computations
  # - define M = max. top terms for coherence computation
  M <- 15
  # - from each topic in the itemTopic matrix
  cutOffItems <- 
    select(itMatrix, dplyr::starts_with('topic')) %>% 
    apply(2, function(x) {
      x <- as.numeric(x)
      itMatrix$eu_entity_id[which(x %in% sort(x, decreasing = T)[1:M])]
    })
  if (class(cutOffItems) == "matrix") {
    items <- unique(as.character(cutOffItems))
  } else {
    items <- unique(unlist(cutOffItems))
  }
  
  # - compose API call
  # - perform API calls for item P31 properties
  # - fetch item P31 properties in batches (max values = 50, MediaWiki API constraint)
  c <- 0
  ixStart <- 1
  iProps <- list()
  # - to Report
  print(paste0("Fetching from API item P31 properties for: ", lFtmod[i]))
  repeat {
    ixEnd <- ixStart + 20 - 1
    searchItems <- items[ixStart:ixEnd]
    print(paste0("Processing now: ", ixStart, "-", ixEnd, " of: ", length(items), " items in: ", lFtmod[i]))
    w <- which(is.na(searchItems))
    if (length(w) > 0) {searchItems <- searchItems[-w]}
    ids <- paste(searchItems, collapse = "|")
    query <- paste0(APIprefix, 
                    'ids=', ids, '&',
                    '&props=claims&languages=en&sitefilter=wikidatawiki&languagefallback=true&format=json')
    repeat {
      res <- tryCatch(
        {
          GET(url = URLencode(query))
        },
        error = function(condition) {
          Sys.sleep(10)
          GET(url = URLencode(query))
        },
        warning = function(condition) {
          Sys.sleep(10)
          GET(url = URLencode(query))
        }
      )
      if (res$status_code == 200) {
        break
      } else {
        print(paste0("Repeating API call; current server status response is: ", res$status_code, "."))
      }
    }
     
    rc <- rawToChar(res$content)
    rc <- fromJSON(rc)
    rc <- rc$entities
    propsP31 <- lapply(rc, function(x) {
      
      tryCatch({
        unique(
          c(x$claims$P31$mainsnak$datavalue$value$id)
        )
      },
      error = function(condition) {NULL},
      warning = function(condition) {NULL}
      )
    })
    wNull <- which(sapply(propsP31, is.null))
    if (length(wNull) > 0) {
      propsP31[wNull] <- "NoP31Class"
    }
    p31list <- vector(mode = "list", length = length(propsP31))
    for (j in 1:length(propsP31)) {
      p31list[[j]] <- data.frame(eu_entity_id = rep(names(propsP31[j]), length(propsP31[[j]])),
                                 eu_p31 = propsP31[[j]],
                                 stringsAsFactors = F)
    }
    p31list <- rbindlist(p31list)
    c <- c + 1
    iProps[[c]] <- p31list
    # - exit condition
    ixStart <- ixStart + 20
    if (ixStart > length(items)) {
      break
    } else {
      # - pause here 3 secs
      Sys.sleep(3)
    }
  }
  rm(res); rm(rc); gc()
  iProps <- rbindlist(iProps)
  # - reshape:
  iProps <- table(iProps)
  # - remove "NoP31Class" tag if any:
  wNoP31Class <- which(colnames(iProps) == "NoP31Class")
  if (length(wNoP31Class) > 0) {iProps <- iProps[, -wNoP31Class]}
  iProps <- as.data.frame(iProps)
  iProps <- iProps %>% 
    tidyr::spread(key = eu_p31,
                  value = Freq,
                  fill = 0)
  # - to Report
  print("DONE.")
  
  ### --- perform API calls for p31 labels
  # - get p31 labels
  items <- iProps %>% 
    dplyr::select(dplyr::starts_with('Q')) %>%
                  colnames()
  # - fetch item labels in batches (max values = 50, MediaWiki API constraint)
  c <- 0
  ixStart <- 1
  p31Labs <- list()
  repeat {
    ixEnd <- ixStart + 20 - 1
    searchItems <- items[ixStart:ixEnd]
    # - to Report
    print(paste0("Processing now: ", ixStart, "-", ixEnd, " of: ", length(items), " item labels in: ", lFtmod[i]))  
    w <- which(is.na(searchItems))
    if (length(w) > 0) {searchItems <- searchItems[-w]}
    ids <- paste(searchItems, collapse = "|")
    query <- paste0(APIprefix, 
                    'ids=', ids, '&',
                    'props=labels&languages=en&sitefilter=wikidatawiki&languagefallback=true&format=json')
    res <- tryCatch(
      {
        GET(url = URLencode(query))
      },
      error = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      },
      warning = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      }
    )
    rc <- rawToChar(res$content)
    rc <- fromJSON(rc)
    itemLabels <- lapply(rc$entities, function(x) {
      x$labels
    })
    itemLabels <- sapply(itemLabels, function(x) {
      tryCatch({
        x[[1]][2]$value},
        error = function(condition) {'No label'},
        warning = function(condition) {'No Label'}
        )
      })
    itemLabels <- data.frame(eu_entity_id = names(itemLabels), 
                             eu_label = itemLabels, 
                             stringsAsFactors = F, 
                             row.names = c())
    c <- c + 1
    p31Labs[[c]] <- itemLabels
    ixStart <- ixStart + 20
    if (ixStart > length(items)) {
      break
    } else {
      # - pause here 3 secs
      Sys.sleep(3)
    }
  }
  rm(res); rm(rc); gc()
  p31Labs <- rbindlist(p31Labs)
  p31Labs$eu_label[which(p31Labs$eu_label == 'No Label')] <- 
    p31Labs$eu_entity_id[which(p31Labs$eu_label == 'No Label')]
  # - paste p31 labels into colnames(iProps):
  wQcols <- which(grepl("Q[[:digit:]]+", colnames(iProps)))
  colnames(iProps)[wQcols] <- paste0(p31Labs$eu_label, 
                                     " (", 
                                     colnames(iProps)[wQcols], 
                                     ")"
                                     )
  # - save
  filename <- strsplit(lFtmod[i], split = "_")[[1]][1]
  filename <- paste0(filename, "_topicAnnotationMatrix.csv")
  write.csv(iProps, filename)
  # - to Report
  print("DONE.")
  print("Sleep now for 10 secs.")
  Sys.sleep(10)
}

### --- project properties onto entity x topics matrices
lFitemtopic <- list.files()
lFitemtopic <- lFitemtopic[grepl("itemtopic", lFitemtopic)]
lFannotatetopic <- list.files()
lFannotatetopic <- lFannotatetopic[grepl("_topicAnnotationMatrix", lFannotatetopic)]
lFwikitopic <- list.files()
lFwikitopic <- lFwikitopic[grepl("wikitopic", lFwikitopic)]
for (i in 1:length(lFitemtopic)) {
  # - to Report
  print(paste0("Projecting P31 on item-topic matrix for: ", lFitemtopic[i]))
  # - right matrix
  itemTopic <- fread(lFitemtopic[i])
  itemTopic$V1 <- NULL
  itemTopic$eu_label <- NULL
  # - left matrix
  topicAnnotation <- fread(lFannotatetopic[i])
  topicAnnotation$V1 <- NULL
  # - comfort itemTopic to topicAnnotation
  # - i.e. select only those items from itemTopic
  # - whose classes are used for annotation
  wTAItems <- which(itemTopic$eu_entity_id %in% topicAnnotation$eu_entity_id)
  itemTopic <- itemTopic[wTAItems, ]
  itemTopic$eu_entity_id <- NULL
  # - transpose topicAnnotation
  topicAnnotation$eu_entity_id <- NULL
  topicAnnotation <- t(topicAnnotation)
  # - P31 x topics annotation:
  classTopic <- topicAnnotation %*% as.matrix(itemTopic)
  # - normalize P31 x topics annotation per TOPICS:
  classTopic <- apply(classTopic, 2, function(x) {
    x/sum(x, na.rm = T)
  })
  
  if (class(classTopic) == "numeric") {
    classTopic <- as.matrix(t(classTopic))
    rownames(classTopic) <- rownames(topicAnnotation)
  }
  
  # store P31 x topics annotation:
  filename <- gsub("_.+", "", lFitemtopic[i])
  filename <- paste0(filename, "_P31Topic.csv")
  write.csv(as.data.frame(classTopic), filename)
  # - Annotate wikipedias now:
  # - right matrix:
  topicWiki <- fread(lFwikitopic[i])
  wikis <- topicWiki$V1
  topicWiki$V1 <- NULL
  topicWiki <- t(topicWiki)
  colnames(topicWiki) <- wikis
  # - P31 x wikis annotation:
  classProject <- classTopic %*% topicWiki
  # store P31 x wikis annotation:
  filename <- gsub("_.+", "", lFitemtopic[i])
  filename <- paste0(filename, "_P31Wiki.csv")
  write.csv(classProject, filename)
}

# - to Report
print("Projecting P31 on item-topic and topic-wiki matrices completed. ")

### ---------------------------------------------------------------------------
### --- Section 4. Produce Analytical Tables
### ---------------------------------------------------------------------------

### --- Category View

### --- Theme Description in Category (P31 class topical annotations)
lF <- list.files()
lF <- lF[grepl("P31Topic", lF)]
for (i in 1:length(lF)) {
  
  print(paste0("Producing Theme Description in Category: ", lF[i]))
  
  # - read classTopic
  classTopic <- fread(lF[i])
  # - read itemTopic as themeItems
  themeItems <- fread(lFitemtopic[i])
  # - read topicAnnotation
  topicAnnotation <- fread(lFannotatetopic[i])
  topicAnnotation$V1 <- NULL
  
  # - extract top M most imporant items
  # - NOTE: use here the same M as used for topic coherence computations
  # - define M = max. top terms for coherence computation
  M <- 15
  # - from each topic in the itemTopic matrix
  cutOffItems <- 
    select(themeItems, dplyr::starts_with('topic')) %>% 
    apply(2, function(x) {
      x <- as.numeric(unlist(unclass(x)))
      themeItems$eu_entity_id[order(x, decreasing = T)[1:M]]
    })
  rownames(topicAnnotation) <- topicAnnotation$eu_entity_id
  topicAnnotation$eu_entity_id <- NULL
  
  # - pick the most important classes per topic
  topClassesTopic <- apply(cutOffItems, 2, function(x) {
    aMat <- topicAnnotation[which(rownames(topicAnnotation) %in% x), ]
    aMat <- sort(colSums(aMat), decreasing = T)
    aMat <- aMat[aMat > 0]
    aMat <- round(aMat/sum(aMat)*100, 2)
    as.list(aMat)
  })
  
  topClassesTopic <- lapply(topClassesTopic, function(x) {
    unlist(x)
  })
  
  theme <- paste0("Theme ", 1:length(topClassesTopic))
  # - theme description in terms of distinctive classes
  themeDescriptions <- lapply(topClassesTopic,
                              function(x) {
                                items <- unlist(str_extract_all(names(x), "Q[[:digit:]]+"))
                                links <- paste0("https://www.wikidata.org/wiki/", items)
                                links <- paste0('<a href = "',
                                                links,
                                                '" target = "_blank">',
                                                paste0(names(x), paste0("[", x, "%]")),
                                                "</a>")
                                links <- paste(links, sep = "", collapse = ", ")
                                return(links)
                              })
  themeDescriptions <- unlist(themeDescriptions)
  
  # - theme description in terms of items
  # - extract top M most imporant items
  # - NOTE: use here the same M as used for topic coherence computations
  # - define M = max. top terms for coherence computation
  M <- 15
  themeItems$V1 <- NULL
  themeItems$item <- paste0(themeItems$eu_label, " (", themeItems$eu_entity_id, ")")
  themeItemsDescriptions <- apply(themeItems[, 2:(dim(themeItems)[2] - 2)], 
                                  2, 
                                  function(x) {
                                    items <- themeItems$item[order(-x)][1:M]
                                    links <- unlist(str_extract_all(items, "Q[[:digit:]]+"))
                                    links <- paste0("https://www.wikidata.org/wiki/", links)
                                    links <- paste0('<a href = "', 
                                                    links, 
                                                    '" target = "_blank">', 
                                                    items,
                                                    "</a>")
                                    links <- paste(links, sep = "", collapse = ", ")
                                    return(links)                             
                                  })
  # - theme diveristy
  themeDiversity <- apply(classTopic[, 2:dim(classTopic)[2]], 2, function(x) {
    
    N = length(x)
    if (N == 1) {
      surN <- 1
    } else {
      surN <- log(length(x))
    }
    
    paste0(round(-sum(x*log(x))/surN*100, 2), 
           "%")
  })
  themeDiversityQualification <- sapply(themeDiversity, 
                                        function(x) {
                                          diversity <- as.numeric(gsub("%", "", x))
                                          if (diversity <= 25) {
                                            return("Very focused") 
                                          } else if (diversity > 25 & diversity <= 50) {
                                            return("Focused")
                                          } else if (diversity > 50 & diversity <= 75) {
                                            return("Diversified")
                                          } else {
                                            return("Very diversified")
                                          } 
                                        })
  # - compose themeDescription and store:
  filename <- gsub("_.+", "", lF[i])
  filename <- paste0("wdcmdo_",filename, "_themeDescription.csv")
  themeDescription <- data.frame(Theme = theme, 
                                 `Distinctive Classes` = themeDescriptions, 
                                 Items = themeItemsDescriptions,
                                 Diversity = themeDiversity, 
                                 Qualification = themeDiversityQualification,
                                 stringsAsFactors = F)
  write.csv(themeDescription, filename)
}

### --- Theme Distributions across Wikipedias (P31 class topical annotations)
lF <- list.files()
lF <- lF[grepl("wikitopic", lF)]
for (i in 1:length(lF)) {
  print(paste0("Producing Theme Distributions across Wikipedias for: ", lF[i]))
  # - read classWiki
  topicWiki <- fread(lF[i])
  colnames(topicWiki)[1] <- 'Wikipedia'
  topicWiki$Wikipedia <- gsub("wiki", "", topicWiki$Wikipedia)
  colnames(topicWiki)[2:dim(topicWiki)[2]] <- 
    paste0("Theme ", 
          str_extract(colnames(topicWiki)[2:dim(topicWiki)[2]], 
                      "[[:digit:]]+")
          )
  # - produce {ggplot2} chart tables:
  topicWiki <- tidyr::gather(data = topicWiki, 
                             key = 'Theme', 
                             value = 'Score',
                             starts_with('Theme')) %>% 
    dplyr::group_by(Wikipedia, Theme) %>% 
    dplyr::arrange(Theme, desc(Score)) %>% 
    dplyr::group_by(Theme) %>% 
    dplyr::top_n(50, Score)
  # - store:
  filename <- gsub("_.+", "", lF[i])
  filename <- paste0("wdcmdo_", filename, "_themeWiki.csv")
  write.csv(topicWiki, filename)
}

### --- Item Similarity Graphs per Category
lF <- list.files()
lF <- lF[grepl("itemtopic", lF)]
for (i in 1:length(lF)) {
  itemtopic <- fread(lF[i])
  itemtopic$V1 <- NULL
  category <- gsub("_sitelinks_itemtopic.csv", "", lF[i])
  iFreq <- fread(paste0(category, "_freqMatrix.csv"))
  iFreq$V1 <- NULL
  w100 <- iFreq$eu_entity_id[1:100]
  w100 <- which(itemtopic$eu_entity_id %in% w100)
  w100names <- itemtopic$eu_entity_id[w100]
  rm(iFreq)
  itemSim <- itemtopic %>% 
    dplyr::select(starts_with('topic'))
  itemSim <- as.matrix(parDist(as.matrix(itemSim), method = "euclid"))
  itemSim <- itemSim[w100, w100]
  colnames(itemSim) <- w100names
  rownames(itemSim) <- w100names
  labels <- itemtopic$eu_label[w100]
  colnames(itemSim) <- paste0(labels, " (", colnames(itemSim), ")")
  rownames(itemSim) <- paste0(labels, " (", rownames(itemSim), ")")
  # - create {visNetwork} nodes for items
  nodes <- data.frame(id = 1:length(w100), 
                      label = rownames(itemSim), 
                      shape = 'circle',
                      color = '#00bfff',
                      shadow = T)
  # - create {visNetwork} egdes for items
  edgesFrom1 <- nodes$id
  edgesFrom2 <- nodes$id
  edgesFrom3 <- nodes$id
  edgesTo1 <- unname(apply(itemSim, 2, function(x) {
    nodes$id[which(nodes$label == names(sort(x, decreasing = F)[2]))]
  }))
  edgesTo2 <- unname(apply(itemSim, 2, function(x) {
    nodes$id[which(nodes$label == names(sort(x, decreasing = F)[3]))]
  }))
  edgesTo3 <- unname(apply(itemSim, 2, function(x) {
    nodes$id[which(nodes$label == names(sort(x, decreasing = F)[4]))]
  }))
  edges <- data.frame(
    from = c(edgesFrom1, edgesFrom2, edgesFrom3),
    to = c(edgesTo1, edgesTo2, edgesTo3), 
    width = c(rep(5, length(edgesTo1)), rep(4, length(edgesTo2)), 
              rep(3, length(edgesTo3))),
    shadow = T, 
    smooth = T,
    arrows = "to"
  )
  # - store:
  filename <- gsub("_sitelinks_itemtopic.csv", "", lF[i])
  filename <- paste0(filename, "_itemNodes.csv")
  write.csv(nodes, filename)
  filename <- gsub("_sitelinks_itemtopic.csv", "", lF[i])
  filename <- paste0(filename, "_itemEdges.csv")
  write.csv(edges, filename)
  
  # - {networkD3} radial represenations of hiearchical clusters
  hcItemSim <- hclust(dist(itemSim), method = "ward.D")
  filename <- paste0(category, "_itemClusters.Rds")
  saveRDS(hcItemSim, filename)
}

### --- Project Similarity Graphs/Hiearchies per Category
lF <- list.files()
lF <- lF[grepl("wikitopic", lF)]
for (i in 1:length(lF)) {
  wikitopic <- fread(lF[i])
  rownames(wikitopic) <- wikitopic$V1
  wikitopic$V1 <- NULL
  category <- gsub("_sitelinks_wikitopic.csv", "", lF[i])
  wikiSim <- as.matrix(parDist(as.matrix(wikitopic), method = "hellinger"))
  colnames(wikiSim) <- gsub("wiki", "", rownames(wikitopic))
  rownames(wikiSim) <- gsub("wiki", "", rownames(wikitopic))
  
  # - prepare for {mclust}
  cP <- wikiSim
  rownames(cP) <- rownames(wikiSim)
  colnames(cP) <- rownames(wikiSim)
  # - {mclust}
  bic <- mclustBIC(cP)
  model <- Mclust(cP, x = bic)
  # - clusters membership
  projectMembership <- model$classification
  projectMembership <- data.frame(Wiki = (projectMembership), 
                                  Cluster = projectMembership, 
                                  stringsAsFactors = F)
  nodeColors <- brewer.pal(length(unique(projectMembership$Cluster)), "Set3")
  nodeColors <- unname(sapply(nodeColors, function(x) color.id(x)[1]))
  nodeColors <- sapply(projectMembership$Cluster, function(x) {
    nodeColors[x]
  })
  # - create {visNetwork} nodes for items
  nodes <- data.frame(id = 1:dim(wikiSim)[1], 
                      label = rownames(wikiSim), 
                      shape = 'circle',
                      color = nodeColors,
                      shadow = T)

  # - create {visNetwork} egdes for items
  edgesFrom1 <- nodes$id
  edgesFrom2 <- nodes$id
  edgesFrom3 <- nodes$id
  edgesTo1 <- unname(apply(wikiSim, 2, function(x) {
    nodes$id[which(nodes$label == names(sort(x, decreasing = F)[2]))]
  }))
  edgesTo2 <- unname(apply(wikiSim, 2, function(x) {
    nodes$id[which(nodes$label == names(sort(x, decreasing = F)[3]))]
  }))
  edgesTo3 <- unname(apply(wikiSim, 2, function(x) {
    nodes$id[which(nodes$label == names(sort(x, decreasing = F)[4]))]
  }))
  edges <- data.frame(
    from = c(edgesFrom1, edgesFrom2, edgesFrom3),
    to = c(edgesTo1, edgesTo2, edgesTo3), 
    width = c(rep(5, length(edgesTo1)), rep(4, length(edgesTo2)), 
              rep(3, length(edgesTo3))),
    shadow = T, 
    smooth = T,
    arrows = "to"
  )
  # - store:
  filename <- gsub("_sitelinks_wikitopic.csv", "", lF[i])
  filename <- paste0(filename, "_projectNodes.csv")
  write.csv(nodes, filename)
  filename <- gsub("_sitelinks_wikitopic.csv", "", lF[i])
  filename <- paste0(filename, "_projectEdges.csv")
  write.csv(edges, filename)
  
  # - {networkD3} radial represenations of hiearchical clusters
  hcWikiSim <- hclust(dist(wikiSim), method = "ward.D")
  filename <- paste0(category, "_projectClusters.Rds")
  saveRDS(hcWikiSim, filename)
}

### --- Wiki View

### --- Local similarity structure per Wiki

# - load all tfMatrices and format for processing
lF <- list.files()
lF <- lF[grepl("_tfMatrix", lF)]
tfMatrices <- vector(mode = "list", length = length(lF))
for (i in 1:length(tfMatrices)) {
  tfMatrices[[i]] <- fread(lF[i], data.table = F)
  projects <- tfMatrices[[i]]$V1
  tfMatrices[[i]]$V1 <- NULL
  tfMatrices[[i]] <- t(tfMatrices[[i]])
  colnames(tfMatrices[[i]]) <- projects
}
classNames <- unname(sapply(lF, function(x) { 
  strsplit(x, split = "_")[[1]][1]}))
names(tfMatrices) <- classNames

# - iterate over wikies; for each wiki
# - determine the projects that use the same classes;
# - extract all such projects and all such tfMatrices;
# - find all tfMatrices that encompass all such projects;
# - merge to provide a single items x projects matrix;
# - compute similarity structure.
for (i in 1:length(selectedWikies)) {
  # - toReport:
  print(paste0("Computing local similarity structure for: ", 
               selectedWikies[i], ": ", i, "/", length(selectedWikies), "."))
  # - select all tfMatrices were selectedWikies[i] is found:
  sWIx <- sapply(tfMatrices, function(x) {
    selectedWikies[i] %in% colnames(x)
  })
  # - select all projects from all such matrices:
  sProjects <- lapply(tfMatrices[sWIx], function(x) {
    colnames(x)
  }) 
  # - intersect all:
  sProjects <- Reduce(intersect, sProjects)
  # toReport:
  print(paste0(length(sProjects), " projects considered."))
  # - select tfMatrices that have all of the sProjects:
  sPIx <- which(unname(sapply(tfMatrices, function(x) {
    sum(sProjects %in% colnames(x)) == length(sProjects)
  })))
  simMat <- vector(mode = "list", length = length(sPIx))
  for (j in 1:length(sPIx)) {
    simMat[[j]] <- tfMatrices[[sPIx[j]]]
    simMat[[j]] <- simMat[[j]][, sProjects]
  }
  # - merge:
  simMat <- rbindlist(lapply(simMat, as.data.frame))
  # - compute projects x projects Jaccard similarity matrix:
  simMat <- as.matrix(simMat)
  simMat <- apply(simMat, 1, function(x) {
    ifelse(x > 0, 1, 0)
  })
  prNames <- rownames(simMat)
  simMat <- as.matrix(parDist(simMat, method = "binary"))
  rownames(simMat) <- prNames
  colnames(simMat) <- prNames
  write.csv(simMat, file = paste0(selectedWikies[i], "_localSimilarity.csv"))
}
# - correct wikiSelection: what categories have remained in the analysis?
wikiSelection <- read.csv('Wikipedia_wdcmSUsage_CategoryOverview.csv', 
                          header = T,
                          check.names = F,
                          row.names = 1,
                          stringsAsFactors = F)
wCR <- which(colnames(wikiSelection) %in% names(tfMatrices))
wikiSelection <- wikiSelection[, wCR]
write.csv(wikiSelection, 'Wikipedia_wdcmSUsage_CategoryOverview.csv')

# - produce _wikiSimilarityNetwork.Rds files for {visNetwork}
lF <- list.files()
lF <- lF[grepl("_localSimilarity", lF)]
for (i in 1:length(lF)) {
  wiki <- strsplit(lF[i], split = "_")[[1]][1]
  simData <- read.csv(lF[i], 
                     header = T, 
                     check.names = F, 
                     row.names = 1, 
                     stringsAsFactors = F)
  simData$wiki <- rownames(simData)
  simData <- simData %>% 
    gather(key = neighbour, 
           value = similarity, 
           -wiki) %>% 
    filter(wiki != neighbour) %>% 
    group_by(wiki) %>% 
    arrange(wiki, similarity) %>% 
    top_n(-3)
  # - {visNetwork} objects:
  nodeColors <- ifelse(unique(simData$wiki) %in% wiki, "red", "grey40")
  # - create {visNetwork} nodes for wikies
  nodes <- data.frame(id = 1:length(unique(simData$wiki)), 
                      label = unique(simData$wiki), 
                      shape = 'circle',
                      color = nodeColors,
                      shadow = T)
  # - create {visNetwork} edges for wikies
  edges <- data.frame(
    from = simData$wiki,
    to = simData$neighbour, 
    width = rep(c(5, 4, 3), times = length(simData$wiki)/3),
    shadow = T, 
    smooth = T,
    arrows = "to")
  edges$from <- sapply(edges$from, function(x) {
    nodes$id[which(nodes$label %in% x)]
  })
  edges$to <- sapply(edges$to, function(x) {
    nodes$id[which(nodes$label %in% x)]
  })
    # - store {visNetwork} objects:
  filenameNodes <- paste0(wiki, "_wikiSimNodes.csv")
  write.csv(nodes, filenameNodes)
  filenameEdges <- paste0(wiki, "_wikiSimEdges.csv")
  write.csv(edges, filenameEdges)
}

# - COPY to publicDir
# - toReport
system(command = paste0('cp ', localDataDir, '* ', publicDir), 
       wait = T)

# - update string
write(paste0("Last updated on: ", Sys.time()), "wikipediaSitelinksUpdateString.txt")
# - copy update string to: publicDir
system(command = paste0('cp ', localDataDir, 'wikipediaSitelinksUpdateString.txt ', publicDir), 
       wait = T)

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("FULL WDCM Engine (S)itelinks update DONE IN: ", generalT2 - generalT1, "."))

