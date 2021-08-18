#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WDCM Engine Statements
### --- Version 1.0.0
### --- Script: WDCM Engine Statements.R
### --- January 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- WDCM_Engine Statements provides the ETL procedures for the 
### --- WDCM Statements systems.

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
### --- WDCM is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### --- WDCM is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### --- You should have received a copy of the GNU General Public License
### --- along with WDCM. If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

# - toLog
print(paste0("WDCM Statements updated started at: ", 
             as.character(Sys.time())
             )
      )

### --- Setup
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(WikidataR)
library(ggplot2)
library(scales)
library(XML)
library(httr)
library(jsonlite)

### --- parameters
### --- Read WDCM paramereters: wdcmConfig.xml
# - toLog
print(paste0("Read WDCM params: ", Sys.time()))
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
# - toLog
print(paste0("Set directory params: ", Sys.time()))
# - log paths
logDir <- params$statements$logDir
# - hdfs directory
hdfsDir <- params$statements$statements_hdfsDir
# - temporary ETL dir
etlDir <- params$statements$statements_etlDir
# - published-datasets dir, maps onto
# - https://analytics.wikimedia.org/datasets/wdcm/
pubDataDir <- params$statements$publicDir

# - spark2-submit parameters: wdcmConfig_Deployment.xml
paramsDeployment <- xmlParse(paste0(fPath, "wdcmConfig_Deployment.xml"))
paramsDeployment <- xmlToList(paramsDeployment)
# - toLog
print(paste0("Set Spark params: ", Sys.time()))
sparkMaster <- paramsDeployment$biases$spark$biases_master
sparkDeployMode <- paramsDeployment$biases$spark$biases_deploy_mode
sparkNumExecutors <- paramsDeployment$biases$spark$biases_num_executors
sparkDriverMemory <- paramsDeployment$biases$spark$biases_driver_memory
sparkExecutorMemory <- paramsDeployment$biases$spark$biases_executor_memory
sparkConfigDynamic <- paramsDeployment$biases$spark$biases_config

### --- Set proxy
# - toLog
print(paste0("Set proxy params: ", Sys.time()))
Sys.setenv(
  http_proxy = params$general$http_proxy,
  https_proxy = params$general$http_proxy)

# - clear tempDataDir
# - toLog
print(paste0("WDCM Statements:clear etlDir", 
             as.character(Sys.time()))
      )
lF <- list.files(etlDir)
if (length(lF) > 0) {
  lapply(paste0(etlDir, lF), file.remove)
}

### --- Functions
### --- Function: wd_api_fetch_labels()
# - fetch item labels in batches 
# - (max values = 50, MediaWiki API constraint)
wd_api_fetch_labels <- function(items, language, fallback) {
  
  # - params:
  # - items - character vector of Wikidata identifiers
  # - language - character, ISO 639-1 two-letter language code
  # - fallback - to use or not to use the Wikidata language fallback 
  
  # - API prefix    
  APIprefix <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'
  
  # - enforce item uniqueness
  items <- unique(items)
  # - iLabs: store batches
  iLabs <- list()
  
  # fetch items
  # - counter
  c <- 0
  # - batch start
  ixStart <- 1
  repeat {
    ixEnd <- ixStart + 50 - 1
    searchItems <- items[ixStart:ixEnd]
    w <- which(is.na(searchItems))
    if (length(w) > 0) {searchItems <- searchItems[-w]}
    ids <- paste(searchItems, collapse = "|")
    if (fallback == T) {
      query <- paste0(APIprefix, 
                      'ids=', ids, '&',
                      'props=labels&languages=', 
                      language, 
                      '&languagefallback=&sitefilter=wikidatawiki&format=json')
    } else {
      query <- paste0(APIprefix, 
                      'ids=', ids, '&',
                      'props=labels&languages=', 
                      language, 
                      '&sitefilter=wikidatawiki&format=json')
    }
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
    rclabs <- rawToChar(res$content)
    rclabs <- fromJSON(rclabs)
    itemLabels <- unlist(lapply(rclabs$entities, function(x) {
      if (length(x$labels) > 0) {
        return(x$labels[[1]]$value) 
      } else {
        return("")
      }
    }))
    itemLabels <- data.frame(title = names(itemLabels), 
                             en_label = itemLabels, 
                             stringsAsFactors = F, 
                             row.names = c())
    c <- c + 1
    iLabs[[c]] <- itemLabels
    if (length(searchItems) < 50) {
      break
    } else {
      ixStart <- ixStart + 50
      # - pause here 1 sec
      Sys.sleep(1)
    }
  }
  iLabs <- rbindlist(iLabs)
  iLabs <- as.data.frame(iLabs)
  iLabs$en_label[nchar(iLabs$en_label) == 0] <- 'No label defined'
  return(iLabs)
}

### --- statistics
statistics <- list()

### --- determine current wmf.wikidata_entity snapshot
# - toLog
print(paste0("WDCM Statements: determine current WD json dump snapshot.", 
             as.character(Sys.time()))
)
# - Kerberos init
system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
       wait = T)
query <- 'SHOW PARTITIONS wmf.wikidata_entity;'
write(query, paste0(etlDir, 'snapshot_query.hql'))
system(command = paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata /usr/local/bin/beeline --incremental=true --silent -f "', 
  paste0(etlDir, 'snapshot_query.hql'), '" > ',
  paste0(etlDir, "wdsnaps.csv")),
  wait = T)
snaps <- read.csv(paste0(etlDir, 'wdsnaps.csv'), 
                  stringsAsFactors = F)
currentSnap <- tail(snaps$partition, 1)
currentSnap <- substr(currentSnap, 10, 19)
write.csv(currentSnap, paste0(etlDir, 'currentSnap.csv'))

### ---------------------------------------------------------------------------
### --- Apache Spark ETL 
### ---------------------------------------------------------------------------
# - toLog
print(paste0("Run Apache Spark ETL: ", Sys.time()))
# - Kerberos init
system(command = 
         'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
       wait = T)
# -  delete hdfsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rm -r ',
           hdfsDir),
       wait = T)
# -  make hdfsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -mkdir ',
           hdfsDir),
       wait = T)
# - run Pyspark
system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                        sparkMaster, ' ',
                        sparkDeployMode, ' ', 
                        sparkNumExecutors, ' ',
                        sparkDriverMemory, ' ',
                        sparkExecutorMemory, ' ',
                        sparkConfigDynamic, ' ',
                        paste0(fPath, 'wdcmModule_Statements_ETL.py')
                        ),
       wait = T)
print(paste0("Run Apache Spark ETL (DONE): ", Sys.time()))

### ---------------------------------------------------------------------------
### --- Load Spark ETL results
### ---------------------------------------------------------------------------

# - toLog
print(paste0("Read Apache Spark ETL data: ", Sys.time()))

### --- Copy splits from hdfs to local dataDir

### --- Item usage in Wikidata properties
system(paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
  paste0(hdfsDir, 'wd_statements_item_usage'), ' > ', etlDir, 'files.txt'),
  wait = T)
files <- read.table(
  paste0(etlDir, 'files.txt'), 
  skip = 1)
files <- as.character(files$V8)[2:length(files$V8)]
file.remove(paste0(etlDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
    files[i], ' > ',
    paste0(etlDir, "wd_statements_item_usage", i, ".csv")), wait = T)
}
lF <- list.files(etlDir)
lF <- lF[grepl("wd_statements_item_usage", lF)]
dataSet <- lapply(paste0(etlDir, lF), fread)
dataSet <- rbindlist(dataSet)
colnames(dataSet) <- c('item', 'propertyCount')

# - statistics
statistics$items_used_in_wd_properties <- dim(dataSet)[1]

# - remove temporary files
lapply(paste0(etlDir, lF), file.remove)

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_item_usage.csv"))
rm(dataSet); gc()

### --- Property usage in Wikidata: from claims
system(paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
  paste0(hdfsDir, 'wd_statements_property_usage'), ' > ', etlDir, 'files.txt'),
  wait = T)
files <- read.table(
  paste0(etlDir, 'files.txt'), 
  skip = 1)
files <- as.character(files$V8)[2:length(files$V8)]
file.remove(paste0(etlDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
    files[i], ' > ',
    paste0(etlDir, "wd_statements_property_usage", i, ".csv")), wait = T)
}
lF <- list.files(etlDir)
lF <- lF[grepl("wd_statements_property_usage", lF)]
dataSet <- lapply(paste0(etlDir, lF), fread)
dataSet <- rbindlist(dataSet)
colnames(dataSet) <- c('property', 'usage')

# - statistics
statistics$property_use_in_wd_properties <- dim(dataSet)[1]

# - remove temporary files
lapply(paste0(etlDir, lF), file.remove)

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_property_usage.csv"))
rm(dataSet); gc()

### --- Property usage in Wikidata: references
system(paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
  paste0(hdfsDir, 'wd_statements_properties_used_in_references'), ' > ', etlDir, 'files.txt'),
  wait = T)
files <- read.table(
  paste0(etlDir, 'files.txt'), 
  skip = 1)
files <- as.character(files$V8)[2:length(files$V8)]
file.remove(paste0(etlDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
    files[i], ' > ',
    paste0(etlDir, "wd_statements_properties_used_in_references", i, ".csv")), wait = T)
}
lF <- list.files(etlDir)
lF <- lF[grepl("wd_statements_properties_used_in_references", lF)]
dataSet <- lapply(paste0(etlDir, lF), fread)
dataSet <- rbindlist(dataSet)
colnames(dataSet) <- c('property', 'used_in_references')

# - remove temporary files
lapply(paste0(etlDir, lF), file.remove)

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_properties_used_in_references.csv"))
rm(dataSet); gc()

### --- Property usage in Wikidata: qualifiers
system(paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
  paste0(hdfsDir, 'wd_statements_properties_used_in_qualifiers'), ' > ', etlDir, 'files.txt'),
  wait = T)
files <- read.table(
  paste0(etlDir, 'files.txt'), 
  skip = 1)
files <- as.character(files$V8)[2:length(files$V8)]
file.remove(paste0(etlDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
    files[i], ' > ',
    paste0(etlDir, "wd_statements_properties_used_in_qualifiers", i, ".csv")), wait = T)
}
lF <- list.files(etlDir)
lF <- lF[grepl("wd_statements_properties_used_in_qualifiers", lF)]
dataSet <- lapply(paste0(etlDir, lF), fread)
dataSet <- rbindlist(dataSet)
colnames(dataSet) <- c('property', 'used_in_qualifiers')

# - remove temporary files
lapply(paste0(etlDir, lF), file.remove)

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_properties_used_in_qualifiers.csv"))
rm(dataSet); gc()

### --- Number of references per Wikidata property
system(paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
  paste0(hdfsDir, 'wd_statements_num_ref_per_property'), ' > ', etlDir, 'files.txt'),
  wait = T)
files <- read.table(
  paste0(etlDir, 'files.txt'), 
  skip = 1)
files <- as.character(files$V8)[2:length(files$V8)]
file.remove(paste0(etlDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
    files[i], ' > ',
    paste0(etlDir, "wd_statements_num_ref_per_property", i, ".csv")), wait = T)
}
lF <- list.files(etlDir)
lF <- lF[grepl("wd_statements_num_ref_per_property", lF)]
dataSet <- lapply(paste0(etlDir, lF), fread)
dataSet <- rbindlist(dataSet)
colnames(dataSet) <- c('property', 'num_references')

# - remove temporary files
lapply(paste0(etlDir, lF), file.remove)

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_num_ref_per_property.csv"))
rm(dataSet); gc()

### ---------------------------------------------------------------------------
### --- Map-Reduce ETL from Hadoop: C.xx reuse aspects 
### ---------------------------------------------------------------------------

### --- Items whose properties are reused in C.xx usage aspects

### --- ETL from goransm.wdcm_clients_wb_entity_usage
### --- w. HiveQL from Beeline
filename <- "wd_statements_C_aspect_reuse_items.tsv"
queryFile <- "wd_statements_HiveQL_Query.hql"
kerberosPrefix <- 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata '
hiveQLquery <- 'SET hive.mapred.mode=unstrict; 
  SELECT eu_entity_id AS entity, COUNT(*) AS c_reuse FROM goransm.wdcm_clients_wb_entity_usage 
  WHERE eu_aspect RLIKE \'C\' GROUP BY eu_entity_id ORDER BY c_reuse DESC;'
# - write hql
write(hiveQLquery, paste0(fPath, queryFile))
# - to Report
print("Fetching C aspect reuse data from wdcm_clients_wb_entity_usage now: items.")
# - Kerberos init
system(command = paste0(kerberosPrefix, ' hdfs dfs -ls'), 
       wait = T)
# - Run query
query <- system(command = paste(kerberosPrefix, 
                                '/usr/local/bin/beeline --incremental=true --silent -f "',
                                paste0(fPath, queryFile),
                                '" > ', etlDir, filename,
                                sep = ""),
                wait = TRUE)
# - remove query file
file.remove(paste0(fPath, queryFile))
# - to Report
print("DONE w. ETL from Hadoop: wdcm_clients_wb_entity_usage.")
print("DONE w. Statements C aspect reuse for items in the C.xx usage aspect.")

### --- Properties as reused in C.xx usage aspects

### --- ETL from goransm.wdcm_clients_wb_entity_usage
### --- w. HiveQL from Beeline
filename <- "wd_statements_C_aspect_reuse_properties.tsv"
queryFile <- "wd_statements_HiveQL_Query.hql"
kerberosPrefix <- 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata '
hiveQLquery <- 'SET hive.mapred.mode=unstrict; 
  SELECT eu_aspect AS aspect, COUNT(*) AS c_reuse FROM goransm.wdcm_clients_wb_entity_usage 
  WHERE eu_aspect RLIKE \'C\' GROUP BY eu_aspect ORDER BY c_reuse DESC;'
# - write hql
write(hiveQLquery, paste0(fPath, queryFile))
# - to Report
print("Fetching C aspect reuse data from wdcm_clients_wb_entity_usage now: properties.")
# - Kerberos init
system(command = paste0(kerberosPrefix, ' hdfs dfs -ls'), 
       wait = T)
# - Run query
query <- system(command = paste(kerberosPrefix, 
                                '/usr/local/bin/beeline --incremental=true --silent -f "',
                                paste0(fPath, queryFile),
                                '" > ', etlDir, filename,
                                sep = ""),
                wait = TRUE)
# - remove query file
file.remove(paste0(fPath, queryFile))
# - to Report
print("DONE w. ETL from Hadoop: wdcm_clients_wb_entity_usage.")
print("DONE w. Statements C aspect reuse for properties in the C.xx usage aspect.")

### --- Fix/Wrangle C aspect reuse datasets:
dataSet <- fread(paste0(
  etlDir, "wd_statements_C_aspect_reuse_properties.tsv"),
  header = T)
dataSet$aspect <- gsub("C.", "", dataSet$aspect, fixed = T)
w <- which(dataSet$aspect == "C")
dataSet$aspect[w] <- 'Other'
colnames(dataSet) <- c('property', 'C_reuse')
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_C_aspect_reuse_properties.csv"))
file.remove(paste0(etlDir, "wd_statements_C_aspect_reuse_properties.tsv"))
dataSet <- fread(paste0(
  etlDir, "wd_statements_C_aspect_reuse_items.tsv"),
  header = T)
dataSet_10000 <- head(dataSet, 10000)
colnames(dataSet_10000) <- c('item', 'C_reuse')
write.csv(dataSet_10000, 
          paste0(etlDir, "wd_statements_C_aspect_reuse_items_top10000.csv"))
colnames(dataSet) <- c('item', 'C_reuse')
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_C_aspect_reuse_items.csv"))
file.remove(paste0(etlDir, "wd_statements_C_aspect_reuse_items.tsv"))
rm(dataSet); rm(dataSet_10000); gc()

### ---------------------------------------------------------------------------
### --- Analytics datasets 
### ---------------------------------------------------------------------------

### --- properties
prop_usage <- read.csv(paste0(etlDir, "wd_statements_property_usage.csv"),
                       header = T, 
                       row.names = 1)
prop_in_references <- read.csv(paste0(etlDir, "wd_statements_properties_used_in_references.csv"), 
                               header = T,
                               row.names = 1)
prop_in_qualifiers <- read.csv(paste0(etlDir, "wd_statements_properties_used_in_qualifiers.csv"),
                               header = T,
                               row.names = 1)
num_ref_in_properties <- read.csv(paste0(etlDir, "wd_statements_num_ref_per_property.csv"),
                                  header = T,
                                  row.names = 1)
propertiesSet <- dplyr::full_join(prop_usage, 
                                  prop_in_references, 
                                  by = "property")
propertiesSet <- dplyr::full_join(propertiesSet, 
                                  prop_in_qualifiers, 
                                  by = "property")
propertiesSet <- dplyr::full_join(propertiesSet, 
                                  num_ref_in_properties, 
                                  by = "property")
prop_labs <- wd_api_fetch_labels(propertiesSet$property, 
                                 language = "en", 
                                 fallback  = T)
propertiesSet <- dplyr::left_join(propertiesSet, 
                                  prop_labs, 
                                  by = c("property" = "title"))
propertiesSet <- dplyr::select(propertiesSet, 
                               property, en_label, 
                               usage, used_in_references, 
                               used_in_qualifiers,
                               num_references)
propertiesSet[is.na(propertiesSet)] <- 0
colnames(propertiesSet) <- c('property', 
                             'en_label',
                             'used_in_claims', 
                             'used_in_references',
                             'used_in_qualifiers',
                             'num_references')
write.csv(propertiesSet, 
          paste0(etlDir, "wd_statements_propertiesSet.csv"))

### --- items
items_usage <- fread(paste0(etlDir, 'wd_statements_item_usage.csv'),
                     header = T)
items_usage$V1 <- NULL
items_usage <- head(items_usage, 10000)
item_labs <- wd_api_fetch_labels(items_usage$item,
                                 language = "en",
                                 fallback  = T)
items_usage <- dplyr::left_join(items_usage,
                                item_labs,
                                by = c("item" = "title"))
colnames(items_usage) <- c('item',
                           'used_in_properties', 
                           'en_label')
write.csv(items_usage, 
          paste0(etlDir, "wd_statements_items_usage_top10000.csv"))
rm(items_usage); gc()

### --- items C aspect reuse
items_reuse <- 
  read.csv(paste0(etlDir, 
                  "wd_statements_C_aspect_reuse_items_top10000.csv"), 
           header = T, 
           row.names = 1)
item_labs <- wd_api_fetch_labels(items_reuse$item,
                                 language = "en",
                                 fallback  = T)
items_reuse <- dplyr::left_join(items_reuse,
                                item_labs,
                                by = c("item" = "title"))
items_reuse <- items_reuse[, c('item', 'en_label', 'C_reuse')]
write.csv(items_reuse, 
          paste0(etlDir, "wd_statements_C_aspect_reuse_items_top10000.csv"))

### --- update timestamp
timestamp <- as.character(Sys.time())
timestamp <- paste0(timestamp, " UTC")
timestamp <- data.frame(timestamp = timestamp, 
                        stringsAsFactors = F)
write.csv(timestamp, 
          paste0(etlDir, "updateTimestamp.csv"))

### -----------------------------------
### --- copy to pubDataDir
### -----------------------------------

# - toLog
print(paste0("copy to pubDataDir: ", Sys.time()))
# - archive:
lF <- c('wd_statements_C_aspect_reuse_items_top10000.csv', 
        'wd_statements_C_aspect_reuse_properties.csv', 
        'wd_statements_propertiesSet.csv', 
        'wd_statements_items_usage_top10000.csv', 
        'currentSnap.csv', 
        'updateTimestamp.csv')
lapply(lF, function(x) {
  system(command = 
           paste0('cp ', etlDir, x, ' ', pubDataDir),
         wait = T)
})

### -----------------------------------
### --- final log
### -----------------------------------
### --------------------------------------------------
### --- copy and clean up log files:
### --------------------------------------------------
# - copy the main log file to published for timestamp
# - toRuntime log:
print("Copy main log to published; clean up log.")
# - archive:
lF <- list.files(logDir)
lF <- lF[grepl('WDCM_Engine_Statements_RuntimeLog', lF)]
lapply(lF, function(x) {
  system(command = 
           paste0('cp ', logDir, x, ' ', logDir, 'archive/'),
         wait = T)
})
# - clean up
file.remove(paste0(logDir, lF))
# - conclusion
print("DONE. Exiting.")

# - toLog
print(paste0("WDCM Statements updated ended at: ", Sys.time()))


