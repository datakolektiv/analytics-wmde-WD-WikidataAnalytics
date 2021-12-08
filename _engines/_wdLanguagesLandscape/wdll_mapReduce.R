#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdll_mapReduce.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- August 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate WD Languages Landscape modules:
### --- 2. wdll_mapReduce.R
### --- Map-Reduce, Hive procedures for WDLL
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- Script 2: wdll_mapReduce.R
### ---------------------------------------------------------------------------

### --- Setup

# - to runtime Log:
print(paste("--- wdll_mapReduce.R RUN STARTED ON:", 
            Sys.time(), sep = " "))

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

# - params
params <- XML::xmlParse(
  paste0(fPath, "WD_LanguagesLandscape_Config.xml")
  )
params <- XML::xmlToList(params)

# - dirs
dataDir <- params$general$dataDir
logDir <- params$general$logDir
outDir <- params$general$outDir
publicDir <- params$general$pubDataDir
hdfsPath <- params$general$hdfsPath

### --- Map-Reduce ETL: WD re-use dataset
### --- ETL from goransm.wdcm_clients_wb_entity_usage
### --- w. HiveQL from Beeline

# - toReport
print(paste("wdll_MapReduce: HiveQL from Beeline, ETL from goransm.wdcm_clients_wb_entity_usage init:", 
            Sys.time(), sep = " "))
filename <- "wd_entities_reuse.tsv"
queryFile <- paste0(fPath, "wd_reuse_HiveQL_Query.hql")
kerberosPrefix <- 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata '
hiveQLquery <- 'SET hive.mapred.mode=unstrict; SELECT eu_entity_id, COUNT(*) AS eu_count FROM 
                  (SELECT DISTINCT eu_entity_id, eu_page_id, wiki_db FROM goransm.wdcm_clients_wb_entity_usage) 
                AS t GROUP BY eu_entity_id;'
# - write hql
write(hiveQLquery, queryFile)
# - to Report
print(paste("wdll_MapReduce: Fetching reuse data from goransm.wdcm_clients_wb_entity_usage STARTED ON:", 
            Sys.time(), sep = " "))
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Run HiveQL query
kerberos_runHiveQL(kerberosUser = "analytics-privatedata",
                   query = queryFile,
                   localPath = dataDir,
                   localFilename = filename)
# - to Report
print("wdll_MapReduce: DONE w. ETL from Hadoop: WD re-use dataset.")
print("wdll_MapReduce: DONE w. fundamental dataset production.")

### --- Compose labels dataset from hdfs

# - to runtime Log:
print(paste("wdll_MapReduce: Collect Final Labels Dataset STARTED ON:", 
            Sys.time(), sep = " "))
dataSet <- WMDEData::hdfs_read_from(kerberosUser = 
                                      "analytics-privatedata",
                                    localPath = dataDir,
                                    localFilenamePrefix = 
                                      "wd_dump_item_language",
                                    hdfsDir = hdfsPath,
                                    hdfsFilenamePrefix = 
                                      "wd_dump_item_language",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c("entity", "language")

# - collect stats
stats <- list()
stats$totalN_Labels <- dim(dataSet)[1]

### --- collect unique language codes

uniqueLanguageCodes <- sort(unique(dataSet$language))
# - collect stats
stats$uniqueN_Labels <- length(uniqueLanguageCodes)

# - store
print("wdll_MapReduce: store: dataSet")
saveRDS(dataSet,
        paste0(outDir, "wd_entities_languages.Rds"))

### --- how many languages, per item:

data.table::setkey(dataSet, entity)
itemCount <- dataSet[, .N ,by = entity]

# - remove DataSet: save memory on stat100*
rm(dataSet); gc()
colnames(itemCount) <- c("entity", "language_count")

### --- WDCM Usage dataset

print("wdll_MapReduce: fread: wd_entities_reuse.tsv")
wdcmReuse <- data.table::fread(paste0(
  dataDir, "wd_entities_reuse.tsv"), 
  sep = "\t")

# - schema
colnames(wdcmReuse) <- c("entity", "reuse")
data.table::setkey(wdcmReuse, entity)
data.table::setorder(wdcmReuse, -reuse)

# - collect stats
stats$totalN_entities_reused <- dim(wdcmReuse)[1]

# - store
print("wdll_MapReduce: saveRDS: wdcmReuse")
saveRDS(wdcmReuse,
        paste0(outDir, "wd_entities_reuse.Rds"))

# - left join: wdcmReuse on itemCount
itemCount <- merge(itemCount,
                   wdcmReuse,
                   by = "entity",
                   all.x = TRUE)
itemCount <- itemCount[!is.na(reuse)]

# - store
print("wdll_MapReduce: saveRDS: itemCount")
saveRDS(itemCount,
        paste0(outDir, "wd_entities_count.Rds"))
rm(itemCount); gc()

# - load fundamental data set
print("wdll_MapReduce: readRDS: wd_entities_languages.Rds")
dataSet <- readRDS(paste0(
  outDir, "wd_entities_languages.Rds")
  )

# - left join: wdcmReuse on dataSet
data.table::setkey(dataSet, entity)
dataSet <- merge(dataSet,
                 wdcmReuse,
                 by = "entity",
                 all.x = TRUE)
# - clear
rm(wdcmReuse); gc()

# - compute re-use per language
countUsedItems <- dataSet[!is.na(reuse), .N, by = "language"]
colnames(countUsedItems)[2] <- "num_items_reused"
dataSet[is.na(reuse), reuse := 0]
dataSet <- dplyr::select(dataSet, -entity)
dataSet <- dataSet[, .(reuse = sum(reuse), item_count = .N), by = "language"]
dataSet <- merge(dataSet,
                 countUsedItems,
                 by = "language",
                 all.x = TRUE)

# - store dataSet
print("wdll_MapReduce: write.csv: dataSet -> wd_languages_count.csv")
write.csv(dataSet,
          paste0(outDir, "wd_languages_count.csv")
          )

# - store statistics
saveRDS(stats, paste0(outDir, "wdll_stats.Rds"))

# - to runtime Log:
print(paste("--- wdll_mapReduce.R RUN ENDED ON:", 
            Sys.time(), sep = " "))

