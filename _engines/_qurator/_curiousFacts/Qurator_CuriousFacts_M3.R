#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Current Events
### --- Script: Q_CE_Functions.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: functions to support the Qurator Project(s) 
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of QURATOR Current Events
### ---
### --- QURATOR Current Events is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- QURATOR Current Events is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with QURATOR Current Events If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Setup
library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)
library(WikidataR)

### --- dirTree
fPath <- paste0(getwd(), "/")
dataDir <- paste0(fPath, "_data/")
analyticsDir <- paste0(fPath, "_analytics/")
reportingDir <- paste0(fPath, "_reporting/")
hdfsDir <- 'hdfs:///tmp/wmde/analytics/qurator/'

### --- functions
source(paste0(fPath, 'Q_CF_Functions.R'))

### --- Set proxy for stat100*
Sys.setenv(
  http_proxy = 'http://webproxy.eqiad.wmnet:8080',
  https_proxy = 'http://webproxy.eqiad.wmnet:8080')

### --- WDQS endpoint
sparqlEndPointURL <- 
  "https://query.wikidata.org/bigdata/namespace/wdq/sparql?format=json&query="

### ----------------------------------------------------------------------------
### --- Module 3: Items that should have one value for a property but have many
### --- Single Value Property Constraint ETL/Problem Solver
### ----------------------------------------------------------------------------

### --- WDQS: obtain properties with single value constraints
query <- 'select ?property where {?property wdt:P2302 wd:Q19474404 .}'
# - run query:
repeat {
  res <- tryCatch({
    GET(url = paste0(sparqlEndPointURL, URLencode(query)))
  },
  error = function(condition) {
    print("Something's wrong on WDQS: wait 10 secs, try again.")
    Sys.sleep(10)
    GET(url = paste0(sparqlEndPointURL, URLencode(query)))
  },
  warning = function(condition) {
    print("Something's wrong on WDQS: wait 10 secs, try again.")
    Sys.sleep(10)
    GET(url = paste0(sparqlEndPointURL, URLencode(query)))
  }
  )  
  if (res$status_code == 200) {
    print(": success.")
    break
  } else {
    print(": failed.")
  }
}
# - parse res:
print("--- M3: parse WDQS result.")
res <- rawToChar(res$content)
res <- fromJSON(res, simplifyDataFrame = T)
res <- res$results$bindings
res <- data.frame(property = res$property$value,
                  stringsAsFactors = F)
res$property <- gsub("http://www.wikidata.org/entity/", "", res$property)

# - move to hdfs directory:
print("--- M3: move to hdfs directory.")
write.csv(res, 
          paste0(fPath, "singleValueConstraintProperties.csv"))
system(command = paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
  paste0(fPath, "singleValueConstraintProperties.csv"), " ",
  hdfsDir),
  wait = T)


### --- WDQS: obtain all separators, for all properties w. single value constraints
query <- 'select ?property ?propertyLabel ?propertyConstraint ?propertyConstraintLabel
  where {
    ?property p:P2302 ?statement .
    ?statement pq:P4155 ?propertyConstraint .
    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
  }'
# - run query:
repeat {
  res <- tryCatch({
    GET(url = paste0(sparqlEndPointURL, URLencode(query)))
  },
  error = function(condition) {
    print("Something's wrong on WDQS: wait 10 secs, try again.")
    Sys.sleep(10)
    GET(url = paste0(sparqlEndPointURL, URLencode(query)))
  },
  warning = function(condition) {
    print("Something's wrong on WDQS: wait 10 secs, try again.")
    Sys.sleep(10)
    GET(url = paste0(sparqlEndPointURL, URLencode(query)))
  }
  )  
  if (res$status_code == 200) {
    print(": success.")
    break
  } else {
    print(": failed.")
  }
}
# - parse res:
print("--- M3: parse WDQS result.")
res <- rawToChar(res$content)
res <- fromJSON(res, simplifyDataFrame = T)
res <- res$results$bindings
res <- data.frame(separator = res$propertyConstraint$value,
                  stringsAsFactors = F)
res$separator <- gsub("http://www.wikidata.org/entity/", "", res$separator)

# - move to hdfs directory:
print("--- M3: move to hdfs directory.")
write.csv(res, 
          paste0(fPath, "separators.csv"))
system(command = paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
  paste0(fPath, "separators.csv"), " ",
  hdfsDir),
  wait = T)

### --- Apache Spark ETL

# - Spark deployment parameters:
paramsDeployment <- xmlParse(paste0(fPath, "wd_cluster_fetch_items_Deployment.xml"))
paramsDeployment <- xmlToList(paramsDeployment)
# - spark2-submit parameters:
sparkMaster <- paramsDeployment$spark$master
sparkDeployMode <- paramsDeployment$spark$deploy_mode
sparkNumExecutors <- paramsDeployment$spark$num_executors
sparkDriverMemory <- paramsDeployment$spark$driver_memory
sparkExecutorMemory <- paramsDeployment$spark$executor_memory
sparkConfigDynamic <- paramsDeployment$spark$config

# - Kerberos init
print("--- wd_cluster_fetch_items_M3: Kerberos init.")
system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
       wait = T)

# - Run PySpark ETL
print("--- wd_cluster_fetch_items_M3: Run PySpark ETL (wd_cluster_fetch_items_M3.py)")
system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                        sparkMaster, ' ',
                        sparkDeployMode, ' ',
                        sparkNumExecutors, ' ',
                        sparkDriverMemory, ' ',
                        sparkExecutorMemory, ' ',
                        sparkConfigDynamic, ' ',
                        paste0(fPath, 'wd_cluster_fetch_items_M3.py')
                        ),
       wait = T)

# - exit
print("--- wd_cluster_fetch_items_M3: DONE; Exit.)")

# - read result - and process it, if there is any
system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ', 
              hdfsDir, ' > ', 
              dataDir, 'files.txt'), 
       wait = T)
files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
files <- as.character(files$V8[grepl("result_M3", files$V8)])
# - collect wdDumpSnapshot
wdDumpSnapshot <- strsplit(files, "/")[[1]][8]
wdDumpSnapshot <- gsub("result_M3_", "", wdDumpSnapshot)
wdDumpSnapshot <- gsub(".csv", "", wdDumpSnapshot, fixed = T)
system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
              files, ' > ', 
              dataDir, 'files.txt'), 
       wait = T)
files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
files <- as.character(files$V8)[2:length(as.character(files$V8))]
file.remove(paste0(dataDir, 'files.txt'))
for (j in 1:length(files)) {
  system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ', 
                files[j], ' > ',  
                paste0(dataDir, "results_M3_", j, ".csv")), wait = T)
}
# - read splits: dataSet
# - load
lF <- list.files(dataDir)
lF <- lF[grepl("results_M3_", lF)]
dataSet <- lapply(paste0(dataDir, lF), 
                  function(x) {fread(x, header = F)})
# - collect
dataSet <- rbindlist(dataSet)
colnames(dataSet) <- c('item', 'itemLabel', 'property', 'propertyLabel', 'num_values')
dataSet$itemLabel <- gsub('^\\\\"', '', dataSet$itemLabel)
dataSet$itemLabel <- gsub('\\\\"$', '', dataSet$itemLabel)
dataSet$propertyLabel <- gsub('^\\\\"', '', dataSet$propertyLabel)
dataSet$propertyLabel <- gsub('\\\\"$', '', dataSet$propertyLabel)
# - filter for count > 1
dataSet <- dplyr::filter(dataSet, 
                         num_values > 1)

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
                              ' has ', dataSet$num_values, 
                              ' values for Property ',
                              tProperty, 
                              ' but it should often be unique.')

dataSet$establishedOn <- as.character(Sys.time())
write.csv(dataSet, 
          paste0(reportingDir, "dataM3.csv"))
infoM3 <- list(problemType = 'M3',
               wdDumpSnapshot = wdDumpSnapshot)
infoM3 <- as.data.frame(infoM3)
write.csv(infoM3, 
          paste0(reportingDir, "infoM3.csv"))


