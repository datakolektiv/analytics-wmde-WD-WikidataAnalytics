#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Curious Facts
### --- Version 1.0.0
### --- Script: Qurator_CuriousFacts_M2.R
### --- September 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: Finds M2 type anomalies in Wikidata
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of QURATOR Curious Facts
### ---
### --- QURATOR Curious Facts is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- QURATOR Curious Facts is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with QURATOR Curious Facts If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Setup

# - to runtime Log:
print(paste(
  "--- FULL QURATOR Curious Facts M2 update STARTED ON:", 
  Sys.time(), 
  sep = " ")
)
# - GENERAL TIMING:
generalT1 <- Sys.time()

# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = TRUE)
fPath <- unlist(strsplit(fPath, split = "/", fixed = TRUE))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")

# - renv
renv::load(project = fPath, quiet = FALSE)

# - lib
library(WMDEData)

# - dirTree
dataDir <- paste0(fPath, "_data/")
analyticsDir <- paste0(fPath, "_analytics/")
reportingDir <- paste0(fPath, "_reporting/")

# - pars
params <- XML::xmlParse(paste0(fPath, 
                               "wd_cluster_fetch_items.xml"))
params <- XML::xmlToList(params)
hdfsDir <- params$hdfsDir

# - proxy
WMDEData::set_proxy()

# - functions
source(paste0(fPath, 'Q_CF_Functions.R'))

# - WDQS endpoint
sparqlEndPointURL <- 
  "https://query.wikidata.org/bigdata/namespace/wdq/sparql?format=json&query="

### ----------------------------------------------------------------------------
### --- Module 2: Property constraints/Subject class
### --- ETL/Problem Solver
### ----------------------------------------------------------------------------

### --- determine wmf.wikidata_entity snapshot
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Define query
queryFile <- paste0(dataDir, 
                    "snapshot_Query.hql")
hiveQLquery <- "SHOW PARTITIONS wmf.wikidata_entity;"
write(hiveQLquery, queryFile)
# - Run HiveQL query
filename <- "snapshot.tsv"
WMDEData::kerberos_runHiveQL(kerberosUser = "analytics-privatedata",
                             query = queryFile,
                             localPath = dataDir,
                             localFilename = filename)
wikidataEntitySnapshot <- read.table(paste0(dataDir, filename), 
                                     sep = "\t", 
                                     stringsAsFactors = FALSE)
wikidataEntitySnapshot <- tail(wikidataEntitySnapshot$V1, 1)
wikidataEntitySnapshot <- 
  stringr::str_extract(wikidataEntitySnapshot,
                       "[[:digit:]]+-[[:digit:]]+-[[:digit:]]+") 

### --- obtain property constraints from Wikidata
propConstraints <- 
  wd_fetchPropertyConstraints(sparqlEndPointURL)

### --- load M2 problems
m2_problems <- read.csv(paste0(fPath, 'm2_problems.csv'), 
                        stringsAsFactors = F, 
                        header = T)

# - iterate over problems and solve
failed <- numeric()

for (i in 1:dim(m2_problems)[1]) {
  
  print(paste0("--------------- ", 
               "Solving now problem : ", 
               i, "/", dim(m2_problems)[1], ".",
               " --------------- ")
  )
  
  # - define problem
  targetProperty <- m2_problems$targetProperty[i]
  
  # - define property constraints
  targetClasses <- propConstraints %>% 
    dplyr::filter(grepl(
      paste0(targetProperty, "$"), 
      propConstraints$`Property_.value`))
  targetClasses <- targetClasses[, c('Property_.value',
                                     'Property_Label.value',
                                     'class_.value', 
                                     'class_Label.value',
                                     'relation_.value', 
                                     'relation_Label.value'
  )]
  targetClasses$Property_.value <- gsub('http://www.wikidata.org/entity/', 
                                        '',
                                        targetClasses$Property_.value)
  targetClasses$class_.value <- gsub('http://www.wikidata.org/entity/',
                                     '',
                                     targetClasses$class_.value)
  targetClasses$relation_.value <- gsub('http://www.wikidata.org/entity/',
                                        '',
                                        targetClasses$relation_.value)
  
  # - extract relation
  apiPx <- "https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&ids="
  wdRelation <- lapply(unique(targetClasses$relation_.value), 
                       function(x) {
                       p <- httr::GET(URLencode(paste0(apiPx, x)))
                         p <- rawToChar(p$content)
                         p <- jsonlite::fromJSON(p)
                         p <- p[[1]][[1]]$claims$P1687
                         return(p$mainsnak$datavalue$value$id)  
                       })
  names(wdRelation) <- unique(targetClasses$relation_.value)
  wdRelation <- stack(wdRelation)
  wdRelation <- rbind(wdRelation)
  # - join wdRelation to targetClasses
  targetClasses <- dplyr::left_join(targetClasses, 
                                    wdRelation, 
                                    by = c('relation_.value' = 'ind'))
  targetClasses <- targetClasses %>% 
    dplyr::select('values', 'class_.value')
  colnames(targetClasses) <- c('property', 'class')
  targetClasses <- targetClasses %>% 
    dplyr::group_by(property) %>% 
    dplyr::summarise(classes = paste(class, collapse = ', '))
  
  # - report problem
  print(paste0("This problem is: ", targetProperty, "."))

  # - clean up dataDir
  lF <- list.files(dataDir)
  lF <- lF[grepl('^results_M2', lF)]
  if (length(lF) > 0) {
    file.remove(paste0(dataDir, lF))
  }
  
  # - solve problem
  t1 <- Sys.time()
  fetchCheck <- wd_cluster_fetch_items_M2(targetProperty = targetProperty,
                                          referenceProperty = targetClasses$property,
                                          referenceClasses = targetClasses$classes,
                                          fPath = fPath,
                                          dataDir = dataDir)
  timeTaken <- difftime(Sys.time(), t1, units = "mins")
  print(paste0("This took: ", round(timeTaken, 2), " minutes."))
  
  if (fetchCheck) {
    # - read result from hdfs
    hdfsFPx <- paste0("result_M2_", 
                      wikidataEntitySnapshot, 
                      ".csv")
    dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                        localPath = dataDir,
                                        localFilenamePrefix = "results_M1_",
                                        hdfsDir = hdfsDir,
                                        hdfsFilenamePrefix = hdfsFPx,
                                        fr_header = FALSE)
    
    if (dim(dataSet)[1] > 0) {
      
      # - schema
      colnames(dataSet) <- c('item', 'property')
      
      # - PREPARE OUTPUT: targetClasses$classes, Explanation, timestamp, etc.
      # - add Wikidata JSON Dump snapshot
      dataSet$wdDumpSnapshot <- wikidataEntitySnapshot
      # - add metadata
      dataSet$establishedOn <- as.character(Sys.time())
      dataSet$problemType <- 'M2'
      dataSet$timeTaken <- timeTaken
      
      # - fetch 'en' item labels
      print("--- fetch 'en' item labels")
      items <- unique(dataSet$item)
      itemLabs <- WMDEData::api_fetch_labels(items = items,
                                             language = "en",
                                             fallback = TRUE)
      colnames(itemLabs) <- c("propertyValue", "propertyValueLabel")
      colnames(itemLabs) <- c('item', 'itemLabel')
      dataSet <- dplyr::left_join(dataSet,
                                  itemLabs,
                                  by = 'item')
      
      
      # - fetch 'en' referenceClasses labels
      refClasses <- targetClasses$classes
      refClasses <- strsplit(refClasses, ", ")[[1]]
      refClassesLabs <- WMDEData::api_fetch_labels(items = refClasses,
                                                   language = "en",
                                                   fallback = TRUE)
      # - fix for No label defined
      refClassesLabs$label <- ifelse(refClassesLabs$label == "",
                                     refClassesLabs$title,
                                     refClassesLabs$label)
      tRefClasses <- paste0('<a href="https://www.wikidata.org/wiki/',
                            refClassesLabs$item,
                            '" target = "_blank">',
                            refClassesLabs$label,
                            '</a>')
      tRefClasses <- paste(tRefClasses, collapse = ", ")
      
      # - fetch propertLabel
      propLab <- WMDEData::api_fetch_labels(items = unique(dataSet$property),
                                            language = "en",
                                            fallback = TRUE)
      dataSet$propertyLab <- propLab$label
      
      # - fix for No label defined
      dataSet$itemLabel <- ifelse(dataSet$itemLabel == "", 
                                  dataSet$item, 
                                  dataSet$itemLabel)
      dataSet$propertyLab <- ifelse(dataSet$propertyLab == "",
                                    dataSet$property,
                                    dataSet$propertyLab)
      
      # - add explanation
      tItem <- paste0('<a href="https://www.wikidata.org/wiki/', 
                      dataSet$item, 
                      '" target = "_blank">', 
                      dataSet$itemLabel, 
                      '</a>')
      tProperty <- paste0('<a href="https://www.wikidata.org/wiki/Property:',
                          dataSet$property,
                          '" target = "_blank">',
                          dataSet$propertyLab,
                          '</a>')
      
      dataSet$explanation <- paste0(tItem,
                                    ' uses the Property ',
                                    tProperty, 
                                    ' but it is not found in any of the following classes: ',
                                    tRefClasses, 
                                    '.')
      
      # - prepare dataSet
      dataSet <- dplyr::select(dataSet, 
                               item, 
                               itemLabel, 
                               property,
                               propertyLab, 
                               explanation, 
                               problemType, 
                               wdDumpSnapshot,
                               establishedOn, 
                               timeTaken)
      
      # - store results
      prop <- unique(dataSet$property)
      CSVfilename <- paste0("M2_", propLab$item,
                            "_", propLab$label, 
                            "_problems_solved.csv")
      CSVfilename <- gsub(" ", "_", CSVfilename, fixed = T)
      write.csv(dataSet,
                paste0(analyticsDir, CSVfilename))
      
    } else {
      print("------ ATTENTION: Empty dataset, no anomalies found:")
      print(paste0("--------------- ",
                   "The current is : ",
                   i, "/", dim(m2_problems)[1], ".",
                   " --------------- ")
      )
      failed <- append(failed, i)
    } 
    
  } else {
    print("------ ATTENTION: Data Acquisition failed:")
    print(paste0("--------------- ",
                 "The current is : ",
                 i, "/", dim(m2_problems)[1], ".",
                 " --------------- ")
    )
    failed <- append(failed, i)
  }
  
}

### ----------------------------------------------------------------------------
### --- Module 2: Property constraints/Subject class
### --- Reporter
### ----------------------------------------------------------------------------

### --- Collect solved M2 problems
lF <- list.files(analyticsDir)
lF <- lF[grepl("^M2", lF)]
dataM2 <- lapply(paste0(analyticsDir, lF), 
                 data.table::fread, 
                 header = T)
dataM2 <- data.table::rbindlist(dataM2)
dataM2$V1 <- NULL
infoM2 <- list(problemType = unique(dataM2$problemType),
               wdDumpSnapshot = unique(dataM2$wdDumpSnapshot)
               )
infoM2 <- as.data.frame(infoM2)
write.csv(infoM2, 
          paste0(reportingDir, "infoM2.csv"))
dataM2 <- dplyr::select(
  dataM2, 
  item, itemLabel, 
  property, propertyLab,
  explanation, establishedOn
)
colnames(dataM2)[4] <- 'propertyLabel'
write.csv(dataM2, 
          paste0(reportingDir, "dataM2.csv"))

### --- File info on unsolved
m2_problems$solved <- TRUE
m2_problems$solved[failed] <- FALSE
write.csv(m2_problems, 
          paste0(analyticsDir, "m2_problems_solved.csv"))
