#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Curious Facts
### --- Version 1.0.0
### --- Script: Qurator_CuriousFacts_M3.R
### --- September 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: Finds M3 type anomalies in Wikidata
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
  "--- FULL QURATOR Curious Facts M3 update STARTED ON:", 
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
### --- Module 3: Items that should have one value for a property but have many
### --- Single Value Property Constraint ETL/Problem Solver
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

### --- WDQS: obtain properties with single value constraints
qr <- 'SELECT DISTINCT ?property WHERE {?property wdt:P2302 wd:Q19474404 .}'
# - run query:
res <- WMDEData::wdqs_send_query(query = qr, 
                                 SPARQL_Endpoint = sparqlEndPointURL,
                                 max_retry = 10)
# - parse res:
print("--- M3: parse WDQS result.")
res <- jsonlite::fromJSON(res, simplifyDataFrame = T)
res <- res$results$bindings
res <- data.frame(property = res$property$value,
                  stringsAsFactors = F)
res$property <- gsub("http://www.wikidata.org/entity/", "", res$property)

# - move to hdfs directory:
print("--- M3: move to hdfs directory.")
write.csv(res, 
          paste0(fPath, "singleValueConstraintProperties.csv"))
WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata", 
                       localPath = fPath,
                       localFilename = "singleValueConstraintProperties.csv", 
                       hdfsDir = hdfsDir)

### --- WDQS: obtain all separators, for all properties w. single value constraints
qr <- 'SELECT ?property ?propertyLabel ?propertyConstraint ?propertyConstraintLabel
  WHERE {
    ?property p:P2302 ?statement .
    ?statement pq:P4155 ?propertyConstraint .
    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
  }'
# - run query:
res <- WMDEData::wdqs_send_query(query = qr, 
                                 SPARQL_Endpoint = sparqlEndPointURL,
                                 max_retry = 10)
# - parse res:
print("--- M3: parse WDQS result.")
res <- jsonlite::fromJSON(res, simplifyDataFrame = T)
res <- res$results$bindings
res <- data.frame(property = res$property$value,
                  separator = res$propertyConstraint$value,
                  stringsAsFactors = F)
res$property <- gsub("http://www.wikidata.org/entity/", "", res$property)
res$separator <- gsub("http://www.wikidata.org/entity/", "", res$separator)
res <- res[!duplicated(res), ]

# - move to hdfs directory:
print("--- M3: move to hdfs directory.")
write.csv(res, 
          paste0(fPath, "separators.csv"))
WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata", 
                       localPath = fPath,
                       localFilename = "separators.csv", 
                       hdfsDir = hdfsDir)

### --- Apache Spark ETL

# - Spark deployment parameters:
paramsDeployment <- XML::xmlParse(paste0(
  fPath, "wd_cluster_fetch_items_Deployment.xml"))
paramsDeployment <- XML::xmlToList(paramsDeployment)
# - spark2-submit parameters:
sparkMaster <- paramsDeployment$spark$master
sparkDeployMode <- paramsDeployment$spark$deploy_mode
sparkNumExecutors <- paramsDeployment$spark$num_executors
sparkDriverMemory <- paramsDeployment$spark$driver_memory
sparkExecutorMemory <- paramsDeployment$spark$executor_memory
sparkConfigDynamic <- paramsDeployment$spark$config

# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Run Spark ETL
print(
  "--- wd_cluster_fetch_items_M2: Run PySpark ETL (wd_cluster_fetch_items_M3.py)"
  )
WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                            pysparkPath = paste0(fPath, "wd_cluster_fetch_items_M3.py"),
                            sparkMaster = sparkMaster,
                            sparkDeployMode = sparkDeployMode,
                            sparkNumExecutors = sparkNumExecutors,
                            sparkDriverMemory = sparkDriverMemory,
                            sparkExecutorMemory = sparkExecutorMemory,
                            sparkConfigDynamic = sparkConfigDynamic)
# - exit
print("--- wd_cluster_fetch_items_M3: DONE; Exit.)")

### --- read and process result
hdfsFPx <- paste0("result_M3_", 
                  wikidataEntitySnapshot, 
                  ".csv")
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = dataDir,
                                    localFilenamePrefix = "results_M1_",
                                    hdfsDir = hdfsDir,
                                    hdfsFilenamePrefix = hdfsFPx,
                                    fr_header = FALSE)
colnames(dataSet) <- c("item", 
                       "itemLabel", 
                       "property", 
                       "propertyLabel", 
                       "num_values")
# - filter for count > 1
dataSet <- dplyr::filter(dataSet, 
                         num_values > 1)
# - sanitize dataSet
dataSet$itemLabel <- gsub('^\\\\"', "", 
                          dataSet$itemLabel)
dataSet$itemLabel <- gsub('\\\\"$', "", 
                          dataSet$itemLabel)
dataSet$propertyLabel <- gsub('^\\\\"', "", 
                              dataSet$propertyLabel)
dataSet$propertyLabel <- gsub('\\\\"$', "", 
                              dataSet$propertyLabel)

### ----------------------------------------------------------------------------
### --- Module 3: Items that should have one value for a property but have many
### --- Reporter
### ----------------------------------------------------------------------------

# - add explanation
tItem <- paste0('<a href="https://www.wikidata.org/wiki/', 
                dataSet$item, 
                '" target = "_blank">', 
                dataSet$itemLabel, 
                '</a>')
tProperty <- paste0('<a href="https://www.wikidata.org/wiki/Property:',
                    dataSet$property,
                    '" target = "_blank">',
                    dataSet$propertyLabel,
                    '</a>')
dataSet$explanation <- paste0(tItem,
                              " has multiple values for Property ",
                              tProperty, 
                              ", but is generally expected to have only one.")

dataSet$establishedOn <- as.character(Sys.time())
write.csv(dataSet, 
          paste0(reportingDir, "dataM3.csv"))
infoM3 <- list(problemType = 'M3',
               wdDumpSnapshot = wikidataEntitySnapshot)
infoM3 <- as.data.frame(infoM3)
write.csv(infoM3, 
          paste0(reportingDir, "infoM3.csv"))
