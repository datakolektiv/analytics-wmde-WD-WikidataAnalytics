#!/usr/bin/env Rscript

### -----------------------------------------------------------------
### --- Script: WDCM_Sqoop_Clients.R
### --- Version 1.0.0
### --- Author: Dan Andreescu, Software Engineer, WMF (Sqoop, HiveQL)
### --- Author: Goran S. Milovanovic, Data Analyst, WMDE (R)
### --- August 2021.
### --- Developed under the contract between Goran Milovanovic 
### --- PR Data Kolektiv and WMDE.
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

### --- Read WDCM paramereters
# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = T)
fPath <- unlist(strsplit(fPath, split = "/", fixed = T))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")
params <- XML::xmlParse(paste0(fPath, "wdcmConfig.xml"))
params <- XML::xmlToList(params)

### --- renv
renv::load(project = fPath, quiet = FALSE)

### --- lib
library(WMDEData)

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
logFrame <- data.frame(id, 
                       section, 
                       project, 
                       startTime, 
                       endTime,
                       stringsAsFactors = F)

### --- delete previous log:
lF <- list.files()
if ("WDCM_Sqoop_Clients_RUNTIME_LOG.log" %in% lF) {
  file.remove("WDCM_Sqoop_Clients_RUNTIME_LOG.log")
}

### --- Sqoop wbc_entity_usage tables where present

# - count sqooped projects:
NSqoop <- 0

# - define shards:
shards <- paste0('s', 
                 1:8, 
                 '-analytics-replica.eqiad.wmnet -P ', 
                 '331', 
                 1:8)
shardHostPort <- paste0('s', 
                        1:8, 
                        '-analytics-replica.eqiad.wmnet')

for (i in 1:length(shards)) {
  
  # - list all databases in a shard
  # - command:
  mySqlCommand <- paste0('mysql -h ', 
                         shards[i], 
                         ' -e \'SHOW DATABASES;\' > ', 
                         logDir, 
                         paste0("shardTables_", i, ".tsv"))
  system(command = mySqlCommand, 
         wait = TRUE)
  shardTables <- data.table::fread(
    paste0("shardTables_", i, ".tsv"), 
    sep = '\t')
  
  # - select client projects to sqoop from a shard
  wClients <- which(grepl(
    "wiki$|books$|voyage$|source$|quote$|wiktionary$|news$|media$",
    shardTables$Database)
    )
  shardTables <- shardTables$Database[wClients]
  # - remove test wikis:
  wTest <- which(grepl("^test", shardTables))
  if (length(wTest) > 0) {shardTables <- shardTables[-wTest]}
  # - remove wikimania
  wWikiMania <- which(grepl("wikimania", shardTables))
  if (length(wWikiMania) > 0) {shardTables <- shardTables[-wWikiMania]}
  
  # - loop over shardTables: if the respective client 
  # - has a wbc_entity_usage table, sqoop it
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
        data.table::fread("currentShardTables.tsv", sep = "\t", 
                          data.table = F)
      }, 
      error = function(condition) {
        FALSE
      }, 
      warning = function(condition) {
        FALSE
      })
      
      if (!is.logical(clientTables)) {
        
        if (!(identical(dim(clientTables), c(0,0))) | is.null(dim(clientTables))) {
          
          if ("wbc_entity_usage" %in% clientTables[, 1]) {
            
            # - to Log:
            print(paste0("Now sqooping: shard = ", shards[i], " project = ", shardTables[j]))
            
            # - logFrame:
            sTime <- as.character(Sys.time())
            
            # - Sqoop this:
            NSqoop <- NSqoop + 1
            
            # - drop wdcm_clients_wb_entity_usage if this is the first entry
            if (NSqoop == 1) {
              
              hiveCommand <- 
                '"USE goransm; DROP TABLE IF EXISTS wdcm_clients_wb_entity_usage;"'
              hiveCommand <- 
                paste0('sudo -u analytics-privatedata kerberos-run-command ', 
                       'analytics-privatedata beeline --silent -e ',
                      hiveCommand)
              system(command = hiveCommand, wait = TRUE)
              # -  delete files for EXTERNAL Hive table from /user/goransm/wdcmsqoop (hdfs path)
              system(
                command = 
                  paste0('sudo -u analytics-privatedata kerberos-run-command ',
                  'analytics-privatedata hdfs dfs -rm -r ', 
                  '/tmp/wmde/analytics/wdcm/wdcmsqoop'), 
                wait = T)
            }
            
            # - make EXTERNAL Hive tables directory: /tmp/wmde/analytics/wdcm/wdcmsqoop (hdfs path)
            system(
              command = 
                paste0('sudo -u analytics-privatedata kerberos-run-command ', 
                       'analytics-privatedata hdfs dfs -mkdir ', 
                       '/tmp/wmde/analytics/wdcm/wdcmsqoop'), 
              wait = T)
            # - sqoop command:
            # - /usr/bin/sqoop import --connect jdbc:mysql://s1-analytics-replica.eqiad.wmnet:3311/enwiki
            sqoopCommand <- 
              paste0(
                paste0('sudo -u analytics-privatedata kerberos-run-command ', 
                       'analytics-privatedata /usr/bin/sqoop import ', 
                       '--connect jdbc:mysql://'), 
                shardHostPort[i], 
                ':331', 
                i, 
                '/', 
                shardTables[j], 
                paste0(' --password-file ', 
                       '/user/goransm/mysql-analytics-research-client-pw.txt ', 
                       '--username research -m 16 '), 
                paste0('--query "select * from wbc_entity_usage where ', 
                       '\\$CONDITIONS" --split-by eu_row_id --as-avrodatafile ', 
                       '--target-dir /tmp/wmde/analytics/wdcm/wdcmsqoop/wdcm_clients_wb_entity_usage/wiki_db='), 
                shardTables[j],
                ' --delete-target-dir'
                )
            print(paste0("Sqoop NOW: ", shardTables[j], "."))
            system(command = sqoopCommand, wait = TRUE)
            
            # - create Hive table if this is the first entry:
            if (NSqoop == 1) {
              hiveCommand <- 
              "\"USE goransm; CREATE EXTERNAL TABLE \\\`goransm.wdcm_clients_wb_entity_usage\\\`(
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
              hiveCommand <- 
                paste0(
                  "sudo -u analytics-privatedata kerberos-run-command analytics-privatedata beeline --silent -e ", 
                  hiveCommand
                  )
              system(command = hiveCommand, wait = TRUE)
            }
            
            # - repair partitions:
            system(
              command = 
                paste0('sudo -u analytics-privatedata kerberos-run-command ', 
                       'analytics-privatedata beeline --silent -e "USE goransm; ', 
                       'SET hive.mapred.mode = nonstrict; ', 
                       'MSCK REPAIR TABLE wdcm_clients_wb_entity_usage;"'), 
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
            write.csv(logFrame, 
                      paste0(logDir, "WDCM_Sqoop_Report.csv")
                      )
            
          }
        }
      }
    }
  }
}

# - to Report
endT <- Sys.time()
print(paste0("--- UPDATE RUN ENDED ON: ", 
             endT)
      )
