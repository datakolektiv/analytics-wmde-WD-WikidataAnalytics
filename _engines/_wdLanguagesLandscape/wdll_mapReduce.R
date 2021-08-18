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
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of WD Languages Landscape (WDLL)
### ---
### --- WDLL is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDLL is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDLL If not, see <http://www.gnu.org/licenses/>.

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
library(XML)
library(data.table)
library(dplyr)

# - params
params <- XML::xmlParse(paste0(fPath, "WD_LanguagesLandscape_Config.xml"))
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
filename <- paste0(dataDir, "wd_entities_reuse.tsv")
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
system(command = paste0(kerberosPrefix, ' hdfs dfs -ls'), 
       wait = TRUE)
# - Run query
query <- system(command = paste(kerberosPrefix, 
                                '/usr/local/bin/beeline --incremental=true --silent -f "',
                                paste0(queryFile),
                                '" > ', filename,
                                sep = ""),
                wait = TRUE)
# - to Report
print("wdll_MapReduce: DONE w. ETL from Hadoop: WD re-use dataset.")
print("wdll_MapReduce: DONE w. fundamental dataset production.")

### --- Compose labels dataset from hdfs

# - to runtime Log:
print(paste("wdll_MapReduce: Collect Final Labels Dataset STARTED ON:", 
            Sys.time(), sep = " "))
# - copy splits from hdfs to local dataDir
# - from statements:
system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ', 
              hdfsPath, 'wd_dump_item_language > ', 
              dataDir, 'files.txt'), 
       wait = TRUE)
files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
files <- as.character(files$V8)[2:length(as.character(files$V8))]
file.remove(paste0(dataDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ', 
                files[i], ' > ',  
                paste0(dataDir, "wd_dump_item_language", i, ".csv")), wait = TRUE)
}
print("wdll_MapReduce: read splits - dataSet")
# - read splits: dataSet
# - load
lF <- list.files(dataDir)
lF <- lF[grepl("wd_dump_item_language", lF)]
dataSet <- lapply(paste0(dataDir, lF), 
                  function(x) {
                    fread(x, header = FALSE)
                    })
# - collect
dataSet <- data.table::rbindlist(dataSet)
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
itemCount <- data.table::merge(itemCount,
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
dataSet <- data.table::merge(dataSet,
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
dataSet <- data.table::merge(dataSet,
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

