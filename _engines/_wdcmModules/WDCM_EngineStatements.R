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

# - fPath
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = T)
fPath <- unlist(strsplit(fPath, split = "/", fixed = T))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")

# - renv
renv::load(project = fPath, quiet = FALSE)

# - lib
library(WMDEData)

# - pars
print(paste0("Read WDCM params: ", Sys.time()))
params <- XML::xmlParse(paste0(fPath, "wdcmConfig.xml"))
params <- XML::xmlToList(params)

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
paramsDeployment <- XML::xmlParse(
  paste0(fPath, "wdcmConfig_Deployment.xml")
  )
paramsDeployment <- XML::xmlToList(paramsDeployment)
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
# - Set proxy
WMDEData::set_proxy(http_proxy = params$general$http_proxy,
                    https_proxy = params$general$https_proxy)

# - clear tempDataDir
# - toLog
print(paste0("WDCM Statements:clear etlDir", 
             as.character(Sys.time()))
      )
lF <- list.files(etlDir)
if (length(lF) > 0) {
  lapply(paste0(etlDir, lF), file.remove)
}

### --- statistics
statistics <- list()

### --- determine current wmf.wikidata_entity snapshot
# - toLog
print(paste0(
  "WDCM Statements: determine current WD json dump snapshot.",
  as.character(Sys.time()))
  )
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Define query
query <- 'SHOW PARTITIONS wmf.wikidata_entity;'
queryFile <- paste0(etlDir, 
                    "snapshot_query.hql")
write(query, queryFile)
# - Run HiveQL query
filename <- "wdsnaps.csv"
WMDEData::kerberos_runHiveQL(kerberosUser = "analytics-privatedata",
                             query = queryFile,
                             localPath = etlDir,
                             localFilename = filename)
snaps <- read.csv(paste0(etlDir, "wdsnaps.csv"), 
                  stringsAsFactors = F)
currentSnap <- tail(snaps$partition, 1)
currentSnap <- substr(currentSnap, 10, 19)
write.csv(currentSnap, 
          paste0(etlDir, "currentSnap.csv"))

### ---------------------------------------------------------------------------
### --- Apache Spark ETL 
### ---------------------------------------------------------------------------
# - toLog
print(paste0("Run Apache Spark ETL: ", Sys.time()))
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - delete hdfsDir
WMDEData::hdfs_rmdir(kerberosUser = "analytics-privatedata", 
                     hdfsDir = hdfsDir)
# -  make hdfsDir
WMDEData::hdfs_mkdir(kerberosUser = "analytics-privatedata", 
                     hdfsDir = hdfsDir)
# - Run Spark ETL
WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                            pysparkPath = paste0(fPath, 'wdcmModule_Statements_ETL.py'),
                            sparkMaster = sparkMaster,
                            sparkDeployMode = sparkDeployMode,
                            sparkNumExecutors = sparkNumExecutors,
                            sparkDriverMemory = sparkDriverMemory,
                            sparkExecutorMemory = sparkExecutorMemory,
                            sparkConfigDynamic = sparkConfigDynamic)
print(paste0("Run Apache Spark ETL (DONE): ", Sys.time()))

### ---------------------------------------------------------------------------
### --- Load Spark ETL results
### ---------------------------------------------------------------------------

### --- Copy splits from hdfs to local dataDir

# - toLog
print(paste0("Read Apache Spark ETL data: ", Sys.time()))

### --- Item usage in Wikidata properties
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = etlDir,
                                    localFilenamePrefix = "wd_statements_item_usage",
                                    hdfsDir = hdfsDir,
                                    hdfsFilenamePrefix = "wd_statements_item_usage",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c('item', 'propertyCount')

# - statistics
statistics$items_used_in_wd_properties <- dim(dataSet)[1]

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_item_usage.csv"))
rm(dataSet); gc()

### --- Property usage in Wikidata: from claims
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = etlDir,
                                    localFilenamePrefix = "wd_statements_property_usage",
                                    hdfsDir = hdfsDir,
                                    hdfsFilenamePrefix = "wd_statements_property_usage",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c('property', 'usage')

# - statistics
statistics$property_use_in_wd_properties <- dim(dataSet)[1]

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_property_usage.csv"))
rm(dataSet); gc()

### --- Property usage in Wikidata: references
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = etlDir,
                                    localFilenamePrefix = "wd_statements_properties_used_in_references",
                                    hdfsDir = hdfsDir,
                                    hdfsFilenamePrefix = "wd_statements_properties_used_in_references",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c('property', 'used_in_references')

# - store
write.csv(dataSet, 
          paste0(
            etlDir, 
            "wd_statements_properties_used_in_references.csv")
          )
rm(dataSet); gc()

### --- Property usage in Wikidata: qualifiers
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = etlDir,
                                    localFilenamePrefix = "wd_statements_properties_used_in_qualifiers",
                                    hdfsDir = hdfsDir,
                                    hdfsFilenamePrefix = "wd_statements_properties_used_in_qualifiers",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c('property', 'used_in_qualifiers')

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_properties_used_in_qualifiers.csv"))
rm(dataSet); gc()

### --- Number of references per Wikidata property
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = etlDir,
                                    localFilenamePrefix = "wd_statements_num_ref_per_property",
                                    hdfsDir = hdfsDir,
                                    hdfsFilenamePrefix = "wd_statements_num_ref_per_property",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c('property', 'num_references')

# - store
write.csv(dataSet, 
          paste0(etlDir, "wd_statements_num_ref_per_property.csv"))
rm(dataSet); gc()

### ---------------------------------------------------------------------------
### --- Map-Reduce ETL from Hadoop: C.xx reuse aspects 
### ---------------------------------------------------------------------------

### --- Items whose properties are reused in C.xx usage aspects

### --- ETL from goransm.wdcm_clients_wb_entity_usage
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Define query
queryFile <- paste0(etlDir, 
                    "wd_statements_HiveQL_Query.hql")
hiveQLquery <- 'SET hive.mapred.mode=unstrict; 
  SELECT eu_entity_id AS entity, COUNT(*) AS c_reuse FROM goransm.wdcm_clients_wb_entity_usage 
  WHERE eu_aspect RLIKE \'C\' GROUP BY eu_entity_id ORDER BY c_reuse DESC;'
write(hiveQLquery, queryFile)
filename <- "wd_statements_C_aspect_reuse_items.tsv"
# - Run HiveQL query
WMDEData::kerberos_runHiveQL(kerberosUser = "analytics-privatedata",
                             query = queryFile,
                             localPath = etlDir,
                             localFilename = filename)
# - to Report
print("DONE w. ETL from Hadoop: wdcm_clients_wb_entity_usage.")

### --- Properties as reused in C.xx usage aspects
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Define query
queryFile <- paste0(etlDir, 
                    "wd_statements_HiveQL_Query.hql")
hiveQLquery <- 'SET hive.mapred.mode=unstrict; 
  SELECT eu_aspect AS aspect, COUNT(*) AS c_reuse FROM goransm.wdcm_clients_wb_entity_usage 
  WHERE eu_aspect RLIKE \'C\' GROUP BY eu_aspect ORDER BY c_reuse DESC;'
write(hiveQLquery, queryFile)
filename <- "wd_statements_C_aspect_reuse_properties.tsv"
# - Run HiveQL query
WMDEData::kerberos_runHiveQL(kerberosUser = "analytics-privatedata",
                             query = queryFile,
                             localPath = etlDir,
                             localFilename = filename)
# - to Report
print("DONE w. ETL from Hadoop: wdcm_clients_wb_entity_usage.")

### --- Fix/Wrangle C aspect reuse datasets:
dataSet <- data.table::fread(paste0(
  etlDir, "wd_statements_C_aspect_reuse_properties.tsv"),
  header = T)
dataSet$aspect <- gsub("C.", "", dataSet$aspect, fixed = T)
w <- which(dataSet$aspect == "C")
dataSet$aspect[w] <- 'Other'
colnames(dataSet) <- c('property', 'C_reuse')
write.csv(dataSet, 
          paste0(
            etlDir, 
            "wd_statements_C_aspect_reuse_properties.csv")
          )
file.remove(paste0(
  etlDir, 
  "wd_statements_C_aspect_reuse_properties.tsv")
  )
dataSet <- data.table::fread(paste0(
  etlDir, 
  "wd_statements_C_aspect_reuse_items.tsv"),
  header = T)
dataSet_10000 <- head(dataSet, 10000)
colnames(dataSet_10000) <- c('item', 'C_reuse')
write.csv(dataSet_10000, 
          paste0(
            etlDir, 
            "wd_statements_C_aspect_reuse_items_top10000.csv")
          )
colnames(dataSet) <- c('item', 'C_reuse')
write.csv(dataSet, 
          paste0(
            etlDir, 
            "wd_statements_C_aspect_reuse_items.csv")
          )
file.remove(paste0(
  etlDir, 
  "wd_statements_C_aspect_reuse_items.tsv")
  )
rm(dataSet); rm(dataSet_10000); gc()

### ---------------------------------------------------------------------------
### --- Analytics datasets 
### ---------------------------------------------------------------------------

### --- properties
prop_usage <- 
  read.csv(paste0(etlDir,
                  "wd_statements_property_usage.csv"),
           header = T,
           row.names = 1)
prop_in_references <- 
  read.csv(paste0(etlDir,
                  "wd_statements_properties_used_in_references.csv"),
           header = T,
           row.names = 1)
prop_in_qualifiers <- 
  read.csv(paste0(etlDir, 
                  "wd_statements_properties_used_in_qualifiers.csv"),
           header = T,
           row.names = 1)
num_ref_in_properties <- 
  read.csv(paste0(etlDir,
                  "wd_statements_num_ref_per_property.csv"),
           header = T,
           row.names = 1)

# - wrangle
propertiesSet <- dplyr::full_join(prop_usage, 
                                  prop_in_references, 
                                  by = "property")
propertiesSet <- dplyr::full_join(propertiesSet, 
                                  prop_in_qualifiers, 
                                  by = "property")
propertiesSet <- dplyr::full_join(propertiesSet, 
                                  num_ref_in_properties, 
                                  by = "property")
apiPF <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'
prop_labs <- WMDEData::api_fetch_labels(items = propertiesSet$property,
                                        language = "en",
                                        fallback = TRUE,
                                        APIprefix = apiPF)
propertiesSet <- dplyr::left_join(propertiesSet, 
                                  prop_labs, 
                                  by = c("property" = "item"))
colnames(propertiesSet)[length(colnames(propertiesSet))] <- 
  "en_label"
propertiesSet <- dplyr::select(propertiesSet, 
                               property, en_label, 
                               usage, used_in_references, 
                               used_in_qualifiers,
                               num_references)
propertiesSet[is.na(propertiesSet)] <- 0
colnames(propertiesSet) <- c("property", 
                             "en_label",
                             "used_in_claims", 
                             "used_in_references",
                             "used_in_qualifiers",
                             "num_references")
write.csv(propertiesSet, 
          paste0(etlDir, "wd_statements_propertiesSet.csv"))

### --- items
items_usage <- data.table::fread(
  paste0(etlDir, 'wd_statements_item_usage.csv'),
  header = T)
items_usage$V1 <- NULL
items_usage <- head(items_usage, 10000)
apiPF <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'
item_labs <- WMDEData::api_fetch_labels(items = items_usage$item,
                                        language = "en",
                                        fallback = TRUE,
                                        APIprefix = apiPF)
items_usage <- dplyr::left_join(items_usage,
                                item_labs,
                                by = "item")
colnames(items_usage)[length(colnames(items_usage))] <- 
  "en_label"
colnames(items_usage) <- c("item",
                           "used_in_properties", 
                           "en_label")
write.csv(items_usage, 
          paste0(
            etlDir, 
            "wd_statements_items_usage_top10000.csv"))
rm(items_usage); gc()

### --- items C aspect reuse
items_reuse <- 
  read.csv(paste0(
    etlDir,
    "wd_statements_C_aspect_reuse_items_top10000.csv"),
    header = T,
    row.names = 1)
item_labs <- WMDEData::api_fetch_labels(items = items_reuse$item,
                                        language = "en",
                                        fallback = TRUE,
                                        APIprefix = apiPF)
items_reuse <- dplyr::left_join(items_reuse,
                                item_labs,
                                by = "item")
colnames(items_reuse)[length(colnames(items_reuse))] <- 
  "en_label"
items_reuse <- items_reuse[, c('item', 'en_label', 'C_reuse')]
write.csv(items_reuse, 
          paste0(
            etlDir, 
            "wd_statements_C_aspect_reuse_items_top10000.csv")
          )

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
