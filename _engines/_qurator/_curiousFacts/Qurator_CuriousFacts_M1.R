#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Curious Facts
### --- Version 1.0.0
### --- Script: Qurator_CuriousFacts_M1.R
### --- September 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: Finds M1 type anomalies in Wikidata
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
  "--- FULL QURATOR Curious Facts M1 update STARTED ON:", 
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

### ----------------------------------------------------------------------------
### --- Module 1: targetProperty, datavalue:item, against referenceClass
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


### --- load M1 problems
m1_problems <- read.csv(paste0(fPath, 'm1_problems.csv'), 
                        stringsAsFactors = F, 
                        header = T)

### --- iterate over problems and solve
for (i in 1:dim(m1_problems)[1]) {
  
  print(paste0("--------------- ", 
               "Solving now problem : ", 
               i, "/", dim(m1_problems)[1], ".",
               " --------------- ")
  )
  
  # - define problem
  class <- m1_problems$class[i]
  targetProperty <- m1_problems$targetProperty[i]
  referenceClass <- m1_problems$referenceClass[i]
  
  # - determine classLabel
  classLabel <- WMDEData::api_fetch_labels(items = class, 
                                           language = "en", 
                                           fallback = TRUE)
  classLabel <- classLabel$label[1]
    
  # - determine targetPropertyLabel
  targetPropertyLabel <- WMDEData::api_fetch_labels(items = targetProperty, 
                                           language = "en", 
                                           fallback = TRUE)
  targetPropertyLabel <- targetPropertyLabel$label[1]
  
  # - determine referenceClassLabel
  referenceClassLabel <- WMDEData::api_fetch_labels(items = referenceClass, 
                                                    language = "en", 
                                                    fallback = TRUE)
  referenceClassLabel <- referenceClassLabel$label[1]
  
  # - report problem
  print(paste0("This problem is: ", 
               paste0(class, "_", classLabel), 
               "; ", 
               paste0(targetProperty, "_", targetPropertyLabel), 
               "; ",
               "; ", 
               paste0(referenceClass, "_", referenceClassLabel), 
               "; ")
        )
  
  # - clean up dataDir
  lF <- list.files(dataDir)
  lF <- lF[grepl('^result_M1', lF)]
  if (length(lF) > 0) {
    file.remove(paste0(dataDir, lF))
  }
  
  # - solve problem
  t1 <- Sys.time()
  wd_cluster_fetch_items_M1(class = class,
                            targetProperty = targetProperty,
                            referenceClass = referenceClass,
                            fPath = fPath,
                            dataDir = dataDir)
  timeTaken <- difftime(Sys.time(), t1, units = "mins")
  print(paste0("This took: ", round(timeTaken, 2), " minutes."))
  
  # - read result from hdfs
  hdfsFPx <- paste0("result_M1_", 
                    wikidataEntitySnapshot, 
                    ".csv")
  dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                      localPath = dataDir,
                                      localFilenamePrefix = "results_M1_",
                                      hdfsDir = hdfsDir,
                                      hdfsFilenamePrefix = hdfsFPx,
                                      fr_header = FALSE)
  # - clean up hdfs files
  kerberosCom <- paste0("sudo -u analytics-privatedata ",
                        "kerberos-run-command analytics-privatedata ",
                        "hdfs dfs -rmr ", 
                        hdfsDir,
                        hdfsFPx)
  system(command = kerberosCom, wait = TRUE)
  
  #  - wrangle dataSet
  colnames(dataSet) <- c('id', 'propertyValue')
  dataSet$property <- targetProperty
  dataSet$propertyLabel <- targetPropertyLabel
  dataSet$referenceClass <- referenceClass
  dataSet$referenceClassLabel <- referenceClassLabel
  
  # - fetch 'en' item labels
  items <- unique(dataSet$id)
  itemLabs <- WMDEData::api_fetch_labels(items = items,
                                         language = "en",
                                         fallback = TRUE)
  colnames(itemLabs) <- c("id", "itemLabel")
  dataSet <- dplyr::left_join(dataSet,
                              itemLabs,
                              by = "id")
  
  # - fetch 'en' propValue labels
  items <- unique(dataSet$propertyValue)
  itemLabs <- WMDEData::api_fetch_labels(items = items,
                                         language = "en",
                                         fallback = TRUE)
  colnames(itemLabs) <- c("propertyValue", "propertyValueLabel")
  dataSet <- dplyr::left_join(dataSet,
                              itemLabs,
                              by = "propertyValue")
  
  # - fix for No label defined
  dataSet$itemLabel <- ifelse(dataSet$itemLabel == "", 
                              dataSet$id, 
                              dataSet$itemLabel)
  dataSet$propertyValueLabel <- ifelse(dataSet$propertyValueLabel == "",
                                        dataSet$propertyValue,
                                        dataSet$propertyValueLabel)

  # - add explanation
  tItem <- paste0('<a href="https://www.wikidata.org/wiki/', 
                  dataSet$id, 
                  '" target = "_blank">', 
                  dataSet$itemLabel, 
                  '</a>')
  tProperty1 <- paste0('<a href="https://www.wikidata.org/wiki/Property:',
                       targetProperty,
                       '" target = "_blank">',
                       targetPropertyLabel,
                       '</a>')
  tProperty2 <- paste0('<a href="https://www.wikidata.org/wiki/',
                       dataSet$propertyValue,
                       '" target = "_blank">',
                       dataSet$propertyValueLabel,
                       '</a>')
  tReferenceClass <- paste0('<a href="https://www.wikidata.org/wiki/',
                            referenceClass,
                            '" target = "_blank">',
                            referenceClassLabel,
                            '</a>')
  dataSet$explanation <- paste0(tItem, 
                                ' has a statement "',
                                tProperty1,
                                ': ',
                                tProperty2, 
                                '" but ', 
                                dataSet$propertyValueLabel, 
                                ' is not found in the class "', 
                                tReferenceClass, 
                                '".')

  
  # - add metadata
  dataSet$wdDumpSnapshot <- wikidataEntitySnapshot
  dataSet$establishedOn <- as.character(Sys.time())
  dataSet$problemType <- 'M1'
  dataSet$timeTaken <- timeTaken
  
  # - store results
  CSVfilename <- paste0("M1_", classLabel, 
                        "_", targetPropertyLabel, 
                        "_", referenceClassLabel, 
                        ".csv")
  CSVfilename <- gsub(" ", "_", CSVfilename, fixed = T)
  write.csv(dataSet, 
            paste0(analyticsDir, CSVfilename)) 
  
}

### ----------------------------------------------------------------------------
### --- Module 1: targetProperty, datavalue:item, against referenceClass
### --- Reporter
### ----------------------------------------------------------------------------

### --- Collect solved M1 problems
lF <- list.files(analyticsDir)
lF <- lF[grepl("^M1", lF)]
dataM1 <- lapply(paste0(analyticsDir, lF), 
                 data.table::fread, 
                 header = TRUE)
dataM1 <- data.table::rbindlist(dataM1)
dataM1$V1 <- NULL
infoM1 <- list(problemType = unique(dataM1$problemType),
               wdDumpSnapshot = unique(dataM1$wdDumpSnapshot)
               )
infoM1 <- as.data.frame(infoM1)
write.csv(infoM1, 
          paste0(reportingDir, "infoM1.csv"))
dataM1 <- dplyr::select(
  dataM1, 
  id, itemLabel, 
  property, propertyLabel,
  propertyValue, propertyValueLabel,
  referenceClass, referenceClassLabel,
  explanation, establishedOn
)
colnames(dataM1)[1] <- 'item'
write.csv(dataM1, 
          paste0(reportingDir, "dataM1.csv"))

