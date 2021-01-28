#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdcmModule_ML.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Machine Learning procedures for WDCM
### --- NOTE: the execution of this WDCM script is always dependent upon the
### --- previous WDCM_Sqoop_Clients.R run, as well
### --- as the previous execution of wdcmModule_CollectItems.R and 
### --- wdcmModule_ETL.py (Pyspark ETL)
### ---------------------------------------------------------------------------
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
### --- Script 4: wdcmModule_ML.R
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- wdcmModule_ML.R produces WD items TF matrices for LDA Topic Models and 
### --- applies t-SNE dimensionality reduction across several 
### --- model-based similarity matrices. 
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- wdcmModule_ML.R UPDATE RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Setup
library(httr)
library(XML)
library(jsonlite)
# - wrangling:
library(dplyr)
library(stringr)
library(readr)
library(data.table)
library(tidyr)
# - modeling:
library(snowfall)
library(text2vec)
library(Rtsne)
### NOTE: replace proxy Hellinger Distrance
library(proxy)
library(topicmodels)
# - matrix
library(Matrix)

### --- functions
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

### --- Read WDCM paramereters: wdcmConfig.xml
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
# - fPath: where the scripts is run from?
fPath <- params$general$fPath_R
# - form paths:
ontologyDir <- params$general$ontologyDir
logDir <- params$general$logDir
itemsDir <- params$general$itemsDir
structureDir <- params$general$structureDir
etlDir <- params$general$etlDir
mlDir <- params$general$mlDir
mlInputDir <- params$general$mlInputDir
# - production published-datasets:
dataDir <- params$general$publicDir

### --- ML params: wdcmConfig_Deployment.xml
paramsDeployment <- xmlParse(paste0(fPath, "wdcmConfig_Deployment.xml"))
paramsDeployment <- xmlToList(paramsDeployment)
lda_NItems <- as.numeric(paramsDeployment$ml$lda_NItems)
tSNE_Perplexity <- as.numeric(paramsDeployment$ml$tSNE_Perplexity)
tsne_theta <- as.numeric(paramsDeployment$ml$tSNE_Theta)

### ----------------------------------------------
### --- reshape project-item matrices for LDA
### ----------------------------------------------

# - to etlDir
setwd(etlDir)
# - to runtime Log:
print("STEP: Semantic Modeling Phase: RESHAPING TDF MATRICES")
selItems <- list.files()
selItems <- selItems[grepl("^wdcm_category_item_", selItems)]
itemFiles <- list.files()
itemFiles <- itemFiles[grepl("^tfMatrix_", itemFiles)]
for (i in 1:length(itemFiles)) {
  # - to runtime Log:
  print(paste("----------------------- Reshaping TDF matrix: category ", i, ".", sep = ""))
  # - load the most frequently used items
  selFile <- fread(selItems[i])
  nItems <- selFile$eu_entity_id[1:lda_NItems]
  rm(selFile)
  # - load i-th TFMatrix
  categoryFile <- fread(itemFiles[i])
  categoryFile$V1 <- NULL
  # - filter for nItems in categoryFile (the top lda_NItems frequently used items)
  categoryFile <- categoryFile %>%
    dplyr::select(eu_entity_id, eu_project, eu_count) %>% 
    dplyr::filter(eu_entity_id %in% nItems)
  categoryFile <- spread(categoryFile,
                         key = eu_entity_id,
                         value = eu_count,
                         fill = 0)
  rownames(categoryFile) <- categoryFile$eu_project
  categoryFile$eu_project <- NULL
  # - remove items with zero observations, if any
  w <- which(colSums(categoryFile) == 0)
  if (length(w) > 0) {
    categoryFile <- categoryFile[, -w]
  }
  # - remove projects with zero observations, if any
  w <- which(rowSums(categoryFile) == 0)
  if (length(w) > 0) {
    categoryFile <- categoryFile[-w, ]
  }
  # - save reshaped TF matrix
  write.csv(categoryFile, paste0(mlInputDir, itemFiles[i]))
}

### ----------------------------------------------
### --- LDA topic models for each category
### ----------------------------------------------

# - to runtime Log:
print("STEP: Semantic Modeling Phase: LDA estimation")

for (i in 1:length(itemFiles)) {
  
  # - to mlInputDir
  setwd(mlInputDir)
  
  # - to Report:
  print(paste0("Estimating category: ", itemFiles[i]))
  
  categoryName <- strsplit(itemFiles[i], split = ".", fixed = T)[[1]][1]
  categoryName <- strsplit(categoryName, split = "_", fixed = T)[[1]][2]
  
  # - try to read the TF Matrix:
  itemCat <- tryCatch({
    read.csv(itemFiles[i],
             header = T,
             check.names = F,
             row.names = 1,
             stringsAsFactors = F)
  },
  error = function(condition) {NULL}
  )
  # - convert itemCat to sparse matrix for {text2vec}
  itemCat <- as(as.matrix(itemCat), "sparseMatrix")
  
  if (!is.null(itemCat)) {
    
    # - prepare subsets for 3-fold cross-validation
    foldIx <- sample(1:3, dim(itemCat)[1], replace = T)
    folds <- 1:3
      
    # - store perplexity
    repl_perplexity <- vector(mode = "list", length = length(folds))
    
    # - start CV
    for (j in 1:length(folds)) {
      
      trainIx <- setdiff(folds, j)
      trainTDM <- itemCat[which(foldIx %in% trainIx), ]
      testTDM <- itemCat[which(foldIx %in% j), ]
      
      # - params
      # - topic range:
      nTops <- seq(2, 30, by = 1)
      
      # - initiate cluster:
      sfInit(parallel = TRUE, 
             cpus = 20,
             type = "SOCK")
      # - export
      sfExport("nTops")
      sfExport("trainTDM")
      sfExport("testTDM")
      sfLibrary(text2vec)
      
      # - train in parallel:
      # - train:
      print(paste0("---------------- Running CV step: ", j))
      t1 <- Sys.time()
      print(paste0("Training starts:", t1))
      modelPerplexity <- sfClusterApplyLB(nTops,
                                          function(x) {
                                              # - define model:
                                              # - alpha:
                                              doc_topic_prior = 50/x
                                              # - beta:
                                              topic_word_prior = 1/x
                                              # - lda_model:
                                              lda_model <- text2vec:::LatentDirichletAllocation$new(n_topics = x,
                                                                                                    doc_topic_prior,
                                                                                                    topic_word_prior)
                                              # - train:
                                              doc_topic_distr <- tryCatch({
                                                lda_model$fit_transform(trainTDM,
                                                                        n_iter = 100,
                                                                        convergence_tol = -1, 
                                                                        n_check_convergence = 25,
                                                                        progressbar = FALSE)
                                              }, 
                                              error = function(condition) {
                                                NULL
                                              })
                                              
                                              # - if !is.null(doc_topic_distr), compute perplexity:
                                              if (!is.null(doc_topic_distr)) {
                                                new_doc_topic_distr <- tryCatch({
                                                  lda_model$transform(testTDM)
                                                }, 
                                                error = function(condition) {
                                                  NULL
                                                })
                                                # - if !is.null(new_doc_topic_distr), compute perplexity:
                                                if (!is.null(new_doc_topic_distr)) {
                                                  text2vec:::perplexity(testTDM,
                                                                        topic_word_distribution = lda_model$topic_word_distribution,
                                                                        doc_topic_distribution = new_doc_topic_distr)                                                  
                                                } else {
                                                  return(NULL)
                                                }
                                                
                                                } else {
                                                  return(NULL)
                                                  }
                                          })
      
      # - stop cluster
      sfStop()
      
      print(paste0("Training ends:", Sys.time()))
      print(paste0("Training took: ", Sys.time() - t1))
      
      # - store perplexities from j-th replication:
      modelFrame <- data.frame(topics = nTops,
                               perplexity = unlist(modelPerplexity),
                               fold = j,
                               stringsAsFactors = F)
      repl_perplexity[[j]] <- modelFrame
      print(paste0("---------------- Completed fold: ", j))
      
    }
    
  }
  
  # - mean perplexity from all folds
  modelFrame <- rbindlist(repl_perplexity)
  modelFrame <- modelFrame %>% 
    group_by(topics) %>% 
    summarise(meanPerplexity = mean(perplexity))
  selectedTopics <- modelFrame$topics[which.min(modelFrame$meanPerplexity)]
  # - toReport:
  print(paste0("Selected model has ", selectedTopics, "topics; estimating now."))
  
  # - fit optimal category LDA model 
  # - alpha:
  doc_topic_prior = 50/selectedTopics
  # - beta:
  topic_word_prior = 1/selectedTopics
  t1 <- Sys.time()
  print(paste0("Optimal category model: Training starts: ", t1))
  ldaModel <- text2vec:::LatentDirichletAllocation$new(n_topics = selectedTopics,
                                                       doc_topic_prior,
                                                       topic_word_prior)
  # - train:
  doc_topic_distr <- ldaModel$fit_transform(itemCat,
                                            n_iter = 1000,
                                            convergence_tol = -1,
                                            n_check_convergence = 25,
                                            progressbar = FALSE)
  
  print(paste0("Optimal category model: Training ends :", Sys.time()))
  print(paste0("Training took: ", Sys.time() - t1))
  
  # - collect output matrices:
  # - clear:
  rm(itemCat); gc()
  # - to ml results dir:
  setwd(mlDir)
  # - collect matrices:
  # - wdcm_itemtopic
  wdcm_itemtopic <- as.data.frame(t(ldaModel$topic_word_distribution))
  colnames(wdcm_itemtopic) <- paste0("topic", 1:dim(wdcm_itemtopic)[2])
  itemTopicFileName <- paste('wdcm2_itemtopic',
                             paste(categoryName, ".csv", sep = ""),
                             sep = "_")
  write.csv(wdcm_itemtopic, itemTopicFileName)
  # - wdcm_projecttopic
  wdcm_projecttopic <- as.data.frame(doc_topic_distr)
  colnames(wdcm_projecttopic) <- paste0("topic", 1:dim(wdcm_projecttopic)[2])
  wdcm_projecttopic$project <- rownames(wdcm_projecttopic)
  wdcm_projecttopic$projecttype <- projectType(wdcm_projecttopic$project)
  projectTopicFileName <- paste('wdcm2_projecttopic',
                                paste(categoryName, ".csv", sep = ""),
                                sep = "_")
  write.csv(wdcm_projecttopic, projectTopicFileName)

  # - toReport
  print("=====================================================================")
  
}

### ----------------------------------------------
### --- t-SNE 2D maps from wdcm2_projectttopic 
### --- files: projects similarity structure
### ----------------------------------------------
# - to runtime Log:
print("STEP: Semantic Modeling Phase: t-SNE 2D MAPS")
setwd(mlDir)
projectFiles <- list.files()
projectFiles <- projectFiles[grepl("^wdcm2_projecttopic", projectFiles)]
for (i in 1:length(projectFiles)) {
  # - toReport:
  print(paste0("tSNE reduction for: ", projectFiles[i], " happening now."))
  # filename:
  fileName <- strsplit(projectFiles[i], split = ".", fixed = T)[[1]][1]
  fileName <- strsplit(fileName, split = "_", fixed = T)[[1]][3]
  fileName <- paste("wdcm2_tsne2D_project_", fileName, ".csv", sep = "")
  # load:
  projectTopics <- read.csv(projectFiles[i],
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
  projectTopics$project <- NULL
  projectTopics$projecttype <- NULL
  
  # - Distance space, metric: Hellinger
  projectDist <- topicmodels::distHellinger(as.matrix(projectTopics))
  
  # - t-SNE 2D map
  tSNE_PerplexityInit <- tSNE_Perplexity
  tSNE_flag <- FALSE
  repeat {
    print(paste0("-- Attempt tSNE reduction for: ", projectFiles[i], " happening now."))
    tsneProject <- tryCatch({
      Rtsne(projectDist,
            pca = FALSE,
            theta = tsne_theta,
            is_distance = T,
            tsne_perplexity = tSNE_PerplexityInit)
    }, 
    error = function(condition) {
      return(NULL)
    })
    if (!is.null(tsneProject)) {
      tSNE_flag <- TRUE
      break
    } else {
      tSNE_PerplexityInit <- tSNE_PerplexityInit - 1
      print(paste0("-- Decrease perplexity to: ", tSNE_PerplexityInit, "; retry."))
    }
    if (tSNE_PerplexityInit == 0) {
      break
    }
  }
  
  # - if tSNE worked, store:
  if (tSNE_flag) {
    # - store:
    tsneProject <- as.data.frame(tsneProject$Y)
    colnames(tsneProject) <- paste("D", seq(1:dim(tsneProject)[2]), sep = "")
    tsneProject$project <- rownames(projectTopics)
    tsneProject$projecttype <- projectType(tsneProject$project)
    write.csv(tsneProject, fileName)
    # - clear:
    rm(projectTopics); rm(projectDist); rm(tsneProject)
  } else {
    # - if tSNE did not work, fallback: PCA
    # - toReport:
    print(paste0("tSNA FAILED: fallback to 2D PCA for: ", projectFiles[i]))
    pcaSolution <- prcomp(projectDist, center = TRUE, scale = TRUE)
    pcaSolution <- as.data.frame(pcaSolution$x[, 1:2])
    colnames(pcaSolution) <- paste("D", seq(1:dim(pcaSolution)[2]), sep = "")
    pcaSolution$project <- rownames(projectTopics)
    pcaSolution$projecttype <- projectType(pcaSolution$project)
    write.csv(pcaSolution, fileName)
  }
}

### --- {visNetwork} graphs from wdcm2_projectttopic files: projects similarity structure
# - to runtime Log:
print("STEP: {visNetwork} graphs from wdcm2_projectttopic files")
projectFiles <- list.files()
projectFiles <- projectFiles[grepl("^wdcm2_projecttopic", projectFiles)]
for (i in 1:length(projectFiles)) {
  # - load:
  projectTopics <- read.csv(projectFiles[i],
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
  projectTopics$project <- NULL
  projectTopics$projecttype <- NULL
  
  # - toReport
  print(paste0("{visNetwork} data structrues for: ", projectFiles[i]))
  
  # - Distance space, metric: Hellinger
  projectDist <- topicmodels::distHellinger(as.matrix(projectTopics))
  rownames(projectDist) <- rownames(projectTopics)
  colnames(projectDist) <- rownames(projectTopics)
  
  # - {visNetwork} nodes data.frame:
  indexMinDist <- sapply(rownames(projectDist), function(x) {
    w <- which(rownames(projectDist) %in% x)
    y <- sort(projectDist[w, -w], decreasing = T)
    names(y)[length(y)]
  })
  id <- 1:length(colnames(projectDist))
  label <- colnames(projectDist)
  nodes <- data.frame(id = id,
                      label = label,
                      stringsAsFactors = F)
  # - {visNetwork} edges data.frame:
  edges <- data.frame(from = names(indexMinDist),
                      to = unname(indexMinDist),
                      stringsAsFactors = F)
  edges$from <- sapply(edges$from, function(x) {
    nodes$id[which(nodes$label %in% x)]
  })
  edges$to <- sapply(edges$to, function(x) {
    nodes$id[which(nodes$label %in% x)]
  })
  edges$arrows <- rep("to", length(edges$to))
  # filenames:
  fileName <- strsplit(projectFiles[i], split = ".", fixed = T)[[1]][1]
  fileName <- strsplit(fileName, split = "_", fixed = T)[[1]][3]
  nodesFileName <- paste("wdcm2_visNetworkNodes_project_", fileName, ".csv", sep = "")
  edgesFileName <- paste("wdcm2_visNetworkEdges_project_", fileName, ".csv", sep = "")
  # store:
  write.csv(nodes, nodesFileName)
  write.csv(edges, edgesFileName)
  # - clear:
  rm(projectTopics); rm(projectDist); rm(nodes); rm(edges); gc()
}

### --- wdcm2_project_category_2dmap
### --- used on: Overview Dashboard
# - fetch wdcm2_project_category:
# - wrangle wdcm2_project_category for t-SNE:
# - toReport
print("Produce: wdcm2_project_category_2dmap for the Overview dashboard.")
setwd(etlDir)
# - compose wdcm_project_category.csv
lF <- list.files()
lF <- lF[grepl("^wdcm_project_category_", lF)]
lF <- lF[!grepl("^wdcm_project_category_item", lF)]
wdcm_project_category <- lapply(lF, fread)
wdcm_project_category <- rbindlist(wdcm_project_category)
wdcm_project_category <- wdcm_project_category[, c('eu_project', 'category', 'eu_count')]
wEmptyProject <- 
  which(is.na(wdcm_project_category$eu_project) | 
          (wdcm_project_category$eu_project=="") | 
          (wdcm_project_category$eu_project==" "))
if (length(wEmptyProject) > 0) {
  wdcm_project_category <- wdcm_project_category[-wEmptyProject, ]
}
write.csv(wdcm_project_category, "wdcm_project_category.csv")
# - dimensionality reduction
tsneData <- spread(wdcm_project_category,
                   key = category,
                   value = eu_count,
                   fill = 0)
rownames(tsneData) <- tsneData$eu_project
tsneData$eu_project <- NULL
projects <- rownames(tsneData)
tsneData <- as.matrix(dist(tsneData, method = "euclidean"))
# - t-SNE 2D reduction:
tsneData <- Rtsne(tsneData, 
                  theta = tsne_theta, 
                  tsne_perplexity = tSNE_Perplexity, 
                  is_distance = T)
tsneData <- as.data.frame(tsneData$Y)
tsneData$projects <- projects
tsneData$projecttype <- projectType(tsneData$projects)
colnames(tsneData)[1:2] <- c('D1', 'D2')
setwd(mlDir)
write.csv(tsneData, 'wdcm_project_category_2dmap.csv')

### --- wdcm2_category_project_2dmap
### --- used on: Overview Dashboard
# - fetch wdcm2_project_category:
# - wrangle wdcm2_project_category for PCA:
# - toReport
print("Produce: wdcm2_category_project_2dmap for the Overview dashboard.")
setwd(etlDir)
pcaData <- read.csv('wdcm_project_category.csv',
                    header = T,
                    check.names = F,
                    row.names = 1,
                    stringsAsFactors = F)
pcaData <- spread(pcaData,
                  key = eu_project,
                  value = eu_count,
                  fill = 0)
rownames(pcaData) <- pcaData$category
pcaData$category <- NULL
categories <- rownames(pcaData)
pcaData <- as.matrix(dist(pcaData, method = "euclidean"))
# - PCA 2D reduction:
pcaSolution <- prcomp(pcaData, center = TRUE, scale = TRUE)
pcaSolution <- as.data.frame(pcaSolution$x[, 1:2])
pcaSolution <- as.data.frame(pcaSolution)
pcaSolution$category <- categories
colnames(pcaSolution)[1:2] <- c('D1', 'D2')
setwd(mlDir)
write.csv(pcaSolution, 'wdcm2_category_project_2dmap.csv')

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- wdcmModule_ML.R UPDATE DONE IN: ", 
             generalT2 - generalT1, "."))
