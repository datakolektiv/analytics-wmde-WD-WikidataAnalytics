#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WD_Datamodel_Terms.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- September 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- R ETL procedures for the WD JSON dumps in hdfs
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- WD_Datamodel_Terms.R RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

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
params <- XML::xmlParse(paste0(
  fPath, "WD_LanguagesLandscape_Config.xml")
  )
params <- XML::xmlToList(params)

### --- Directories
dataDir <- params$general$datamodel_terms_dataDir
publicDir <- params$general$datamodel_terms_publicDir
logDir <- params$general$logDir
hdfsPath <- params$general$datamodel_terms_hdfsPath

# - Set proxy
WMDEData::set_proxy(http_proxy = params$general$http_proxy,
                    https_proxy = params$general$https_proxy)

### --- WDQS Classes endPoint
endPointURL <- params$general$wdqs_endpoint

### --- Spark: spark2-submit parameters
### --- spark2-submit parameters:
params <- XML::xmlParse(
  paste0(fPath, "WD_LanguagesLandscape_Config_Deploy.xml")
  )
params <- XML::xmlToList(params)
sparkMaster <- params$spark$master
sparkDeployMode <- params$spark$deploy_mode
sparkNumExecutors <- params$spark$num_executors
sparkDriverMemory <- params$spark$driver_memory
sparkExecutorMemory <- params$spark$executor_memory
sparkExecutorCores <- params$spark$executor_cores
sparkConfigDynamic <- params$spark$config

### --- Check wmf.wikidata_entity snapshot
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Define query
query <- 'SHOW PARTITIONS wmf.wikidata_entity;'
queryFile <- paste0(dataDir, "snapshot_query.hql")
write(query, queryFile)
# - Run HiveQL query
filename <- "wdsnaps.csv"
WMDEData::kerberos_runHiveQL(kerberosUser = "analytics-privatedata",
                             query = queryFile,
                             localPath = dataDir,
                             localFilename = filename)
snaps <- read.csv(paste0(dataDir, "wdsnaps.csv"), 
                  stringsAsFactors = F)
currentSnap <- tail(snaps$partition, 1)
currentSnap <- substr(currentSnap, 10, 19)
# - NOTE: the reference dataset for snapshot detection
# - is DM_Terms_Labels.csv 
labs <- read.csv(paste0(dataDir, 'DM_Terms_Labels.csv'), 
                 stringsAsFactors = F)
stopSnap <- tail(labs$snapshot, 1)
rm(labs)

### --- If wmf.wikidata_entity is updated, update:
if (currentSnap != stopSnap) {
  
  # - wait 10 hours: maybe the wmf.wikidata_entity table is not populated
  Sys.sleep(10*60*60)
  
  # - first collect Astronomical Objects
  astronomicalObjects = c(
    'Q523', 'Q318', 'Q1931185', 'Q1457376', 'Q2247863', 'Q3863', 'Q83373',
    'Q2154519', 'Q726242', 'Q1153690', 'Q204107', 'Q71963409', 'Q67206691',
    'Q1151284', 'Q67206701', 'Q66619666', 'Q72802727', 'Q2168098', 'Q6243',
    'Q72802508', 'Q11282', 'Q72803170', 'Q1332364', 'Q72802977', 'Q6999',
    'Q1491746', 'Q272447', 'Q497654', 'Q204194', 'Q130019', 'Q744691',
    'Q71798532', 'Q46587', 'Q11276', 'Q71965429', 'Q5871', 'Q72803622',
    'Q72803426', 'Q3937', 'Q72803708', 'Q168845', 'Q24452', 'Q67201574',
    'Q2557101', 'Q691269', 'Q13632', 'Q10451997', 'Q28738741', 'Q22247'
  )
  collected <- vector(mode = "list", 
                      length = length(astronomicalObjects))
  for (i in 1:length(astronomicalObjects)) {
    # - Construct Query:
    query <- paste0('SELECT ?item WHERE { 
                    SERVICE gas:service { 
                    gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" . 
                    gas:program gas:in wd:', astronomicalObjects[i], ' .
                    gas:program gas:linkType wdt:P279 .
                    gas:program gas:out ?subClass .
                    gas:program gas:traversalDirection "Reverse" .
                    } . 
                    ?item wdt:P31 ?subClass 
                    }') 
    # - Run Query:
    rc <- WMDEData::wdqs_send_query(query = query,
                                    SPARQL_Endpoint = endPointURL,
                                    max_retry = 10)
    rc <- data.frame(item = unlist(
      stringr::str_extract_all(rc, "Q[[:digit:]]+")
      ),
      stringsAsFactors = F)
    
    # - collect
    collected[[i]] <- rc
    rm(rc)
    
    } # - end collect Astronomical Objects loop
  
  # - wrangle + copy to hdfs collected
  collected <- data.table::rbindlist(collected)
  colnames(collected) <- "id"
  write.csv(collected, 
            paste0(dataDir, "collectedAstronomy.csv"))
  
  # - delete hdfsPath
  WMDEData::hdfs_rmdir(kerberosUser = "analytics-privatedata", 
                       hdfsDir = hdfsPath)
  # -  make hdfsPath
  WMDEData::hdfs_mkdir(kerberosUser = "analytics-privatedata", 
                       hdfsDir = hdfsPath)
  
  # -  copy to hdfsPath
  print("---- Move to hdfs.")
  hdfs_copy_to(kerberosUser = "analytics-privatedata",
               localPath = dataDir,
               localFilename = "collectedAstronomy.csv",
               hdfsDir = hdfsPath)
  
  # - now collect Scientific Papers
  scientificPapers = c('Q7318358', 'Q2782326', 'Q18918145', 
                       'Q1504425', 'Q7316896', 'Q92998777', 
                       'Q10885494', 'Q15706459', 'Q58901470', 
                       'Q59458414', 'Q56478376', 'Q12183006', 
                       'Q82969330', 'Q58900805')
  collected <- vector(mode = "list", length = length(scientificPapers))
  for (i in 1:length(scientificPapers)) {
    # - Construct Query:
    query <- paste0('SELECT ?item WHERE { 
                    SERVICE gas:service { 
                    gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" . 
                    gas:program gas:in wd:', scientificPapers[i], ' .
                    gas:program gas:linkType wdt:P279 .
                    gas:program gas:out ?subClass .
                    gas:program gas:traversalDirection "Reverse" .
                    } . 
                    ?item wdt:P31 ?subClass 
                    }') 
    # - Run Query:
    rc <- WMDEData::wdqs_send_query(query = query,
                                    SPARQL_Endpoint = endPointURL,
                                    max_retry = 10)
    rc <- data.frame(item = unlist(
      stringr::str_extract_all(rc, "Q[[:digit:]]+")
    ),
    stringsAsFactors = F)
    
    # - collect
    collected[[i]] <- rc
    rm(rc)
    
  } # - end collect Scientific Papers loop
  
  # - wrangle + copy to hdfs collected
  collected <- data.table::rbindlist(collected)
  colnames(collected) <- "id"
  write.csv(collected, 
            paste0(dataDir, "collectedScientificPapers.csv"))
  
  # -  copy to hdfsPath
  print("---- Move to hdfs.")
  hdfs_copy_to(kerberosUser = "analytics-privatedata",
               localPath = dataDir,
               localFilename = "collectedScientificPapers.csv",
               hdfsDir = hdfsPath)

  ### --- Spark ETL
  # - to runtime Log:
  print(paste("--- WD_Datamodel_Terms.py STARTED ON:", 
              Sys.time(), sep = " "))
  # - Kerberos init
  WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
  # - Run Spark ETL
  WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                              pysparkPath = paste0(fPath, 'WD_Datamodel_Terms.py'),
                              sparkMaster = sparkMaster,
                              sparkDeployMode = sparkDeployMode,
                              sparkNumExecutors = sparkNumExecutors,
                              sparkDriverMemory = sparkDriverMemory,
                              sparkExecutorMemory = sparkExecutorMemory,
                              sparkConfigDynamic = sparkConfigDynamic)
  print(paste0("Run Apache Spark ETL (DONE): ", Sys.time()))
  
  ### --- Update EVERYTHING
  
  # - update Labels
  labels <- read.csv(paste0(dataDir, 
                            "DM_Terms_Labels.csv"),
                     header = T,
                     row.names = 1,
                     stringsAsFactors = F,
                     check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Labels.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  labels <- rbind(labels, update)
  write.csv(labels, paste0(dataDir, 
                           "DM_Terms_Labels.csv"))
  
  # - update aliases
  aliases <- read.csv(paste0(dataDir, 
                             "DM_Terms_Aliases.csv"),
                      header = T,
                      row.names = 1,
                      stringsAsFactors = F,
                      check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Aliases.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  aliases <- rbind(aliases, update)
  write.csv(aliases, paste0(dataDir, 
                            "DM_Terms_Aliases.csv"))
  
  # - update descriptions
  descriptions <- read.csv(paste0(dataDir, 
                                  "DM_Terms_Descriptions.csv"),
                           header = T,
                           row.names = 1,
                           stringsAsFactors = F,
                           check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Descriptions.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  descriptions <- rbind(descriptions, update)
  write.csv(descriptions, paste0(dataDir, 
                                 "DM_Terms_Descriptions.csv"))
  
  ### --- Update ASTRONOMICAL OBJECTS
  
  # - update Labels
  labels <- read.csv(paste0(dataDir, 
                            "DM_Terms_Labels_ASTRONOMY.csv"),
                     header = T,
                     row.names = 1,
                     stringsAsFactors = F,
                     check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Labels_ASTRONOMY.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  labels <- rbind(labels, update)
  write.csv(labels, paste0(dataDir, 
                           "DM_Terms_Labels_ASTRONOMY.csv"))
  
  # - update aliases
  aliases <- read.csv(paste0(dataDir, 
                             "DM_Terms_Aliases_ASTRONOMY.csv"),
                      header = T,
                      row.names = 1,
                      stringsAsFactors = F,
                      check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Aliases_ASTRONOMY.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  aliases <- rbind(aliases, update)
  write.csv(aliases, paste0(dataDir, 
                            "DM_Terms_Aliases_ASTRONOMY.csv"))
  
  # - update descriptions
  descriptions <- read.csv(paste0(dataDir, 
                                  "DM_Terms_Descriptions_ASTRONOMY.csv"),
                           header = T,
                           row.names = 1,
                           stringsAsFactors = F,
                           check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Descriptions_ASTRONOMY.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  descriptions <- rbind(descriptions, update)
  write.csv(descriptions, paste0(dataDir, 
                                 "DM_Terms_Descriptions_ASTRONOMY.csv"))

  ### --- Update SCIENTIFIC PAPERS
  
  # - update Labels
  labels <- read.csv(paste0(dataDir, 
                            "DM_Terms_Labels_SCIENTIFICPAPERS.csv"),
                     header = T,
                     row.names = 1,
                     stringsAsFactors = F,
                     check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Labels_SCIENTIFICPAPERS.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  labels <- rbind(labels, update)
  write.csv(labels, paste0(dataDir, 
                           "DM_Terms_Labels_SCIENTIFICPAPERS.csv"))
  
  # - update aliases
  aliases <- read.csv(paste0(dataDir, 
                             "DM_Terms_Aliases_SCIENTIFICPAPERS.csv"),
                      header = T,
                      row.names = 1,
                      stringsAsFactors = F,
                      check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Aliases_SCIENTIFICPAPERS.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  aliases <- rbind(aliases, update)
  write.csv(aliases, paste0(dataDir, 
                            "DM_Terms_Aliases_SCIENTIFICPAPERS.csv"))
  
  # - update descriptions
  descriptions <- read.csv(paste0(dataDir, 
                                  "DM_Terms_Descriptions_SCIENTIFICPAPERS.csv"),
                           header = T,
                           row.names = 1,
                           stringsAsFactors = F,
                           check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Descriptions_SCIENTIFICPAPERS.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  descriptions <- rbind(descriptions, update)
  write.csv(descriptions, paste0(dataDir, 
                                 "DM_Terms_Descriptions_SCIENTIFICPAPERS.csv"))
  
  ### --- Update EVERYTHING MINUS (ASTRONOMICAL OBJECTS + SCIENTIFIC PAPERS)
  
  # - update Labels
  labels <- read.csv(paste0(dataDir, 
                            "DM_Terms_Labels_EVERYTHINGMINUS.csv"),
                     header = T,
                     row.names = 1,
                     stringsAsFactors = F,
                     check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Labels_EVERYTHINGMINUS.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  labels <- rbind(labels, update)
  write.csv(labels, paste0(dataDir, 
                           "DM_Terms_Labels_EVERYTHINGMINUS.csv"))
  
  # - update aliases
  aliases <- read.csv(paste0(dataDir, 
                             "DM_Terms_Aliases_EVERYTHINGMINUS.csv"),
                      header = T,
                      row.names = 1,
                      stringsAsFactors = F,
                      check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Aliases_EVERYTHINGMINUS.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  aliases <- rbind(aliases, update)
  write.csv(aliases, paste0(dataDir, 
                            "DM_Terms_Aliases_EVERYTHINGMINUS.csv"))
  
  # - update descriptions
  descriptions <- read.csv(paste0(dataDir, 
                                  "DM_Terms_Descriptions_EVERYTHINGMINUS.csv"),
                           header = T,
                           row.names = 1,
                           stringsAsFactors = F,
                           check.names = F)
  update <- read.csv(paste0(dataDir, 
                            "update_Descriptions_EVERYTHINGMINUS.csv"),
                     header = T,
                     stringsAsFactors = F,
                     check.names = F)
  colnames(update)[1] <- "language"
  descriptions <- rbind(descriptions, update)
  write.csv(descriptions, paste0(dataDir, 
                                 "DM_Terms_Descriptions_EVERYTHINGMINUS.csv"))
  
  # - copy to public directory:
  cFiles <- list.files(dataDir)
  cFiles <- cFiles[grepl("^DM_Terms", cFiles)]
  
  for (i in 1:length(cFiles)) {
    print(paste0("Copying: ", cFiles[i], " to publicDir."))
    system(command = 
             paste0("cp ", dataDir, cFiles[i], " ", publicDir),
           wait = T)
  }
  
  ### --- copy and clean up log files:
  
  # - copy the main log file to published for timestamp
  # - toRuntime log:
  print("Copy main log to archive; clean up log.")
  system(command = 
           paste0('cp ', 
                  logDir, 
                  'WD_Datamodel_Terms_LOG.log ', 
                  logDir, 
                  'archive'),
         wait = T)
  # - clean up
  file.remove(paste0(logDir, 
                     "WD_Datamodel_Terms_LOG.log"))
  
  # - to runtime Log:
  print(paste("--- WD_Datamodel_Terms.R RUN ENDED ON:", 
              Sys.time(), sep = " "))
  # - conclusion
  print("DONE. Exiting.")
  
} else {
  
  # - to runtime Log:
  print("Nothing to update.")
  # - conclusion
  print("DONE. Exiting.")

}

