#!/usr/bin/env Rscript

### -----------------------------------------------------------------
### --- Script: WDCM_Sqoop_Clients.R
### --- Version 1.0.0
### --- Author: Dan Andreescu, Software Engineer, WMF (Sqoop, HiveQL)
### --- Author: Goran S. Milovanovic, Data Analyst, WMDE (R code)
### --- June 2020.
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv 
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### -----------------------------------------------------------------
### --- DESCRIPTION:
### --- WDCM_Sqoop_Clients.R takes a list of client projects
### --- that maintain the wbc_entity_usage tables
### --- and sqoopes these tables into a single Hadoop Avro file
### --- on production (currently: stat1004.eqiad.wmnet).
### -----------------------------------------------------------------
### --- OUTPUT: 
### --- Files are stored in hdfs, WMF Analytics cluster
### --- database: goransm
### --- directory: wdcmsqoop
### --- table: wdcm_clients_wb_entity_usage
### -----------------------------------------------------------------

### -----------------------------------------------------------------
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
### -----------------------------------------------------------------

### --- Setup
library(XML)
library(data.table)

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
fPathR <- params$general$fPath_R
logDir <- params$general$logDir

### --- to logDir
setwd(logDir)

### --- to runtime Log:
startT <- Sys.time()
print(paste("--- UPDATE RUN STARTED ON:", startT, sep = " "))

### --- prepare Log file
id = numeric()
section = character()
project = character()
startTime = character()
endTime = character()
logFrame <- data.frame(id, section, project, startTime, endTime,
                       stringsAsFactors = F)

### --- delete previous runtime log:
lF <- list.files()
if ('WDCM_SqoopRuntimeLog.log' %in% lF) {
  file.remove('WDCM_SqoopRuntimeLog.log')
}

### -----------------------------------------------------------------
### --- Sqoop wbc_entity_usage tables where present
# - count sqooped projects:
NSqoop <- 0
# - define shards:
shards <- paste0('s', 1:8, '-analytics-replica.eqiad.wmnet -P ', "331", 1:8)
shardHostPort <- paste0('s', 1:8, '-analytics-replica.eqiad.wmnet')

for (i in 1:length(shards)) {
  
  ### --- list all databases in a shard
  # - command:
  mySqlCommand <- paste0('mysql -h ', 
                         shards[i], 
                         ' -e \'SHOW DATABASES;\' > ', 
                         logDir, 
                         paste0("shardTables_", i, ".tsv"))
  system(command = mySqlCommand, 
         wait = TRUE)
  
  shardTables <- fread(paste0("shardTables_", i, ".tsv"), sep = '\t')
  
  ### --- select client projects to sqoop from a shard
  wClients <- which(grepl("wiki$|books$|voyage$|source$|quote$|wiktionary$|news$|media$", 
                          shardTables$Database))
  shardTables <- shardTables$Database[wClients]
  # - remove test wikis:
  wTest <- which(grepl("^test", shardTables))
  if (length(wTest) > 0) {shardTables <- shardTables[-wTest]}
  # - remove wikimania
  wWikiMania <- which(grepl("wikimania", shardTables))
  if (length(wWikiMania) > 0) {shardTables <- shardTables[-wWikiMania]}
  
  # - loop over shardTables: if the respective client has a wbc_entity_usage table, sqoop it
  for (j in 1:length(shardTables)) {
    # - show tables from database j:
    tryShardTables <- tryCatch({
      mySqlCommand <- paste0('mysql -h ', 
                             shards[i], 
                             ' -e ',
                             '"USE ', 
                             shardTables[j], 
                             '; SHOW TABLES;" > ', 
                             logDir, 
                             "currentShardTables.tsv")
      system(command = mySqlCommand,
             wait = TRUE)
      TRUE
    },
    error = function(conditon) {
      return(FALSE)
    },
    warning = function(condition) {
      return(FALSE)
    })
    
    # - Sqoop if there is anything to sqoop:
    if (tryShardTables) {
      clientTables <- tryCatch({
        fread("currentShardTables.tsv", sep = '\t', data.table = F)
      }, 
      error = function(condition) {
        FALSE
      }, 
      warning = function(condition) {
        FALSE
      })
      
      if (!is.logical(clientTables)) {
        
        if (!(identical(dim(clientTables), c(0,0))) | is.null(dim(clientTables))) {
          
          if ('wbc_entity_usage' %in% clientTables[, 1]) {
            
            # - to Log:
            print(paste0("Now sqooping: shard = ", shards[i], " project = ", shardTables[j]))
            
            # - logFrame:
            sTime <- as.character(Sys.time())
            
            # Sqoop this:
            NSqoop <- NSqoop + 1
            
            # - drop wdcm_clients_wb_entity_usage if this is the first entry
            if (NSqoop == 1) {
              
              hiveCommand <- '"USE goransm; DROP TABLE IF EXISTS wdcm_clients_wb_entity_usage;"'
              hiveCommand <- paste("sudo -u analytics-privatedata kerberos-run-command analytics-privatedata beeline --silent -e ", 
                                   hiveCommand, sep = "")
              system(command = hiveCommand, wait = TRUE)
              # -  delete files for EXTERNAL Hive table from /user/goransm/wdcmsqoop (hdfs path)
              system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rm -r /tmp/wmde/analytics/wdcm/wdcmsqoop', 
                     wait = T)
            }
            
            # - make EXTERNAL Hive tables directory: /tmp/wmde/analytics/wdcm/wdcmsqoop (hdfs path)
            system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -mkdir /tmp/wmde/analytics/wdcm/wdcmsqoop', 
                   wait = T)
            # - sqoop command:
            # - /usr/bin/sqoop import --connect jdbc:mysql://s1-analytics-replica.eqiad.wmnet:3311/enwiki
            sqoopCommand <- paste("sudo -u analytics-privatedata kerberos-run-command analytics-privatedata /usr/bin/sqoop import --connect jdbc:mysql://", shardHostPort[i], ":331",i, "/", shardTables[j],
                                  ' --password-file /user/goransm/mysql-analytics-research-client-pw.txt --username research -m 16 --driver org.mariadb.jdbc.Driver ',
                                  '--query "select * from wbc_entity_usage where \\$CONDITIONS" --split-by eu_row_id --as-avrodatafile --target-dir /tmp/wmde/analytics/wdcm/wdcmsqoop/wdcm_clients_wb_entity_usage/wiki_db=',
                                  shardTables[j],
                                  ' --delete-target-dir',
                                  sep = "")
            system(command = sqoopCommand, wait = TRUE)
            
            # - create Hive table if this is the first entry:
            if (NSqoop == 1) {
              hiveCommand <- "\"USE goransm; CREATE EXTERNAL TABLE \\\`goransm.wdcm_clients_wb_entity_usage\\\`(
              \\\`eu_row_id\\\`           bigint      COMMENT '',
              \\\`eu_entity_id\\\`        string      COMMENT '',
              \\\`eu_aspect\\\`           string      COMMENT '',
              \\\`eu_page_id\\\`          bigint      COMMENT ''
              )
              COMMENT
              ''
              PARTITIONED BY (
              \\\`wiki_db\\\` string COMMENT 'The wiki_db project')
              ROW FORMAT SERDE
              'org.apache.hadoop.hive.serde2.avro.AvroSerDe'
              STORED AS INPUTFORMAT
              'org.apache.hadoop.hive.ql.io.avro.AvroContainerInputFormat'
              OUTPUTFORMAT
              'org.apache.hadoop.hive.ql.io.avro.AvroContainerOutputFormat'
              LOCATION
              'hdfs://analytics-hadoop/tmp/wmde/analytics/wdcm/wdcmsqoop/wdcm_clients_wb_entity_usage';\""
              hiveCommand <- paste("sudo -u analytics-privatedata kerberos-run-command analytics-privatedata beeline --silent -e ", 
                                   hiveCommand, sep = "")
              system(command = hiveCommand, wait = TRUE)
            }
            
            # - repair partitions:
            system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata beeline --silent -e "USE goransm; SET hive.mapred.mode = nonstrict; MSCK REPAIR TABLE wdcm_clients_wb_entity_usage;"', 
                   wait = TRUE)
            
            # - logFrame
            eTime <- Sys.time()
            logFrame <- rbind(logFrame, 
                              data.frame(id = NSqoop, 
                              section = shards[i], 
                              project = shardTables[j], 
                              startTime = as.character(sTime), 
                              endTime = as.character(eTime), 
                              stringsAsFactors = F))
            # - log
            write.csv(logFrame, paste0(logDir, 'WDCM_Sqoop_Report.csv'))
            
          }
        }
      }
    }
  }
}

# - to Report
endT <- Sys.time()
print(paste("--- UPDATE RUN ENDED ON:", endT, sep = " "))
