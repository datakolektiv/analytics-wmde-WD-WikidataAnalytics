#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WD_PageviewsPerType_Engine.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- September 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- ETL/Post-Processing for the WD_PageviewsPerType dashboard
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of WD_PageviewsPerType project
### ---
### --- WD_PageviewsPerType is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WD_PageviewsPerType is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WD_PageviewsPerType If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- Script: WD_PageviewsPerType_Engine.R
### ---------------------------------------------------------------------------

# - toReport
print(paste0(
  "--- WD_PageviewsPerType_Engine.R started at: ", 
  Sys.time())
)

### --- Read WDCM paramereters
# - fPath: where the scripts is run from?
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
params <- XML::xmlParse(
  paste0(fPath, "WD_PageviewsPerTypeConfig.xml")
  )
params <- XML::xmlToList(params)

### --- Directories
# - form paths:
dataDir <- params$general$dataDir
hdfsDataDir <- params$general$hdfsDataDir
aggregateDir <- params$general$aggregateDir
publicDir <- params$general$publicDir
logDir <- params$general$logDir
logArchiveDir <- params$general$logArchiveDir

# - spark2-submit parameters:
paramsDeploy <- XML::xmlParse(
  paste0(fPath, "WD_PageviewsPerTypeConfig_Deployment.xml")
  )
paramsDeploy <- XML::xmlToList(paramsDeploy)
sparkMaster <- paramsDeploy$spark$master
sparkDeployMode <- paramsDeploy$spark$deploy_mode
sparkNumExecutors <- paramsDeploy$spark$num_executors
sparkDriverMemory <- paramsDeploy$spark$driver_memory
sparkExecutorMemory <- paramsDeploy$spark$executor_memory
sparkExecutorCores <- paramsDeploy$spark$executor_cores
sparkConfigDynamic <- paramsDeploy$spark$config

### --- load all raw data sets
# - check if there are any data in dataDir:
if (length(list.files(dataDir)) == 0) {
  # - toReport
  print("NOTE: dataDir is empty; checking if this is the first data intake.")
  # - check if this is the first data intake:
  if (length(list.files(aggregateDir)) == 0) {
    # - toReport
    print("NOTE: aggregateDir is empty; so, this is the first data intake.")
    print("Running WD_PageviewsPerType_Engine_Initiate.py now: first intake.")
    # - RUN WD_PageviewsPerType_Engine_Initiate.py
    # - Kerberos init
    WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
    # - Run Spark ETL
    WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                                pysparkPath = paste0(fPath, "WD_PageviewsPerType_Engine_Initiate.py"),
                                sparkMaster = sparkMaster,
                                sparkDeployMode = sparkDeployMode,
                                sparkNumExecutors = sparkNumExecutors,
                                sparkDriverMemory = sparkDriverMemory,
                                sparkExecutorMemory = sparkExecutorMemory,
                                sparkConfigDynamic = sparkConfigDynamic)
    # - copy data sets from the hdfs dataDir
    system(paste0(
      'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
      hdfsDataDir, 
      ' > ', 
      dataDir, 
      'files.txt'),
      wait = T)
    files <- read.table(
      paste0(dataDir, 'files.txt'), 
      skip = 1)
    files <- as.character(files$V8)
    file.remove(paste0(dataDir, 'files.txt'))
    fileNames <- unlist(lapply(files, function(x) {
      tail(strsplit(x, split = "/")[[1]], 1)
      }))
    for (i in 1:length(files)) {
      system(paste0(
        'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
        files[i], 
        ' > ', 
        dataDir, 
        'singleFile.txt'),
        wait = T)
      singleFile <- read.table(
        paste0(dataDir, 'singleFile.txt'), skip = 1
        )
      singleFile <- 
        as.character(singleFile$V8)[2:length(as.character(singleFile$V8))]
      file.remove(paste0(dataDir, 'singleFile.txt'))
      system(paste0(
        'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
        singleFile, 
        ' > ',
        paste0(dataDir, fileNames[i])), 
        wait = T)
    }
    # - clean up the hdfs dataDir
    system(
      paste0(
        'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rm -r ',
        hdfsDataDir, 
        '*'),
      wait = T)
    # - produce the first aggregated data set from dataDir
    dataSet <- data.table::lapply(
      paste0(dataDir, 
             list.files(dataDir)),
      data.table::fread)
    datetime <- list.files(dataDir)
    datetime <- 
      sapply(strsplit(datetime, split = "_"), function(x) {
        x[3]
        })
    datetime <- 
      sapply(strsplit(datetime, split = ".", fixed = T), function(x) {
        x [1]
        })
    for (i in 1:length(dataSet)) {
      dt <- strsplit(datetime[i], split = "-")[[1]]
      dataSet[[i]]$year <- dt[1]
      dataSet[[i]]$month <- dt[2]
      dataSet[[i]]$day <- dt[3]
    }
    dataSet <- data.table::rbindlist(dataSet)
    colnames(dataSet)[1:4] <- c("namespace_id", 
                                "access_method", 
                                "agent_type", 
                                "pageviews")
    namespaces <- data.frame(namespace = c(0, 120, 146, 640), 
                             namespace_name = c("Item", 
                                                "Property", 
                                                "Lexeme", 
                                                "EntitySchema"))
    dataSet$namespace <- sapply(dataSet$namespace_id, function(x) {
      w <- which(namespaces$namespace == x)
      return(namespaces$namespace_name[w])
    })
    dataSet$timestamp <- 
      as.POSIXct(paste(dataSet$year, 
                       dataSet$month, 
                       dataSet$day, 
                       sep = "-"))
    # - store first aggregated file
    saveRDS(dataSet, 
            paste0(aggregateDir, 'WD_pageviewsPerType.Rds'))
    # - clean up dataDir
    file.remove(paste0(dataDir, list.files(dataDir)))
  } else {
    # - toReport
    print("NOTE: aggregateDir is not empty; so, this is not the first data intake.")
    print("Running WD_PageviewsPerType_Engine.py now: daily update.")
    # - run WD_PageviewsPerType_Engine.py
    # - Kerberos init
    WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
    # - Run Spark ETL
    WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                                pysparkPath = paste0(fPath, "WD_PageviewsPerType_Engine.py"),
                                sparkMaster = sparkMaster,
                                sparkDeployMode = sparkDeployMode,
                                sparkNumExecutors = sparkNumExecutors,
                                sparkDriverMemory = sparkDriverMemory,
                                sparkExecutorMemory = sparkExecutorMemory,
                                sparkConfigDynamic = sparkConfigDynamic)
    print("Collected new data to append; appending now.")
    # - copy data sets from the hdfs dataDir
    system(paste0(
      'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
      hdfsDataDir,
      ' > ',
      dataDir,
      'files.txt'),
      wait = T)
    files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
    files <- as.character(files$V8)[1:length(as.character(files$V8))]
    file.remove(paste0(dataDir, 'files.txt'))
    fileNames <- unlist(lapply(files, function(x) {
      tail(strsplit(x, split = "/")[[1]], 1)
    }))
    for (i in 1:length(files)) {
      system(paste0(
        'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
        files[i], 
        ' > ', 
        dataDir, 
        'singleFile.txt'),
        wait = T)
      singleFile <- 
        read.table(paste0(dataDir, 'singleFile.txt'), skip = 1)
      singleFile <- 
        as.character(singleFile$V8)[2:length(as.character(singleFile$V8))]
      file.remove(paste0(dataDir, 'singleFile.txt'))
      system(paste0(
        'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
        singleFile, 
        ' > ',
        paste0(dataDir, fileNames[i])), 
        wait = T)
    }
    # - clean up the hdfs dataDir
    system(paste0(
      'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rm -r ',
      hdfsDataDir, 
      '*'),
      wait = T)
    # - append new data to the aggregated data set
    dataSet <- lapply(paste0(dataDir, list.files(dataDir)), 
                      data.table::fread)
    datetime <- list.files(dataDir)
    datetime <- sapply( strsplit(datetime, split = "_"), 
                        function(x) {
                          x[3]
                          })
    datetime <- sapply(strsplit(datetime, split = ".", fixed = T), 
                       function(x) {
                         x [1]
                         })
    for (i in 1:length(dataSet)) {
      dt <- strsplit(datetime[i], split = "-")[[1]]
      dataSet[[i]]$year <- dt[1]
      dataSet[[i]]$month <- dt[2]
      dataSet[[i]]$day <- dt[3]
    }
    dataSet <- data.table::rbindlist(dataSet)
    colnames(dataSet)[1:4] <- c("namespace_id", 
                                "access_method", 
                                "agent_type", 
                                "pageviews")
    namespaces <- data.frame(namespace = c(0, 120, 146, 640), 
                             namespace_name = c("Item", 
                                                "Property", 
                                                "Lexeme", 
                                                "EntitySchema"))
    dataSet$namespace <- sapply(dataSet$namespace_id, function(x) {
      w <- which(namespaces$namespace == x)
      return(namespaces$namespace_name[w])
    })
    dataSet$timestamp <- 
      as.POSIXct(paste(dataSet$year, 
                       dataSet$month, 
                       dataSet$day, 
                       sep = "-"))
    # - load existing dataset from aggregateDir
    existingData <- 
      readRDS(paste0(aggregateDir, "WD_pageviewsPerType.Rds")) 
    # - append new data
    existingData <- rbind(existingData, 
                          dataSet)
    # - check for duplicated entries
    existingData$control <- paste(existingData$namespace_id, 
                                  existingData$access_method,
                                  existingData$timestamp, 
                                  existingData$agent_type,
                                  sep = "-")
    w <- which(duplicated(existingData$control))
    if (length(w) > 0) {
      existingData <- existingData[-w, ]
    }
    existingData$control <- NULL
    # - store first aggregated file  
    saveRDS(existingData, 
            paste0(aggregateDir, 'WD_pageviewsPerType.Rds'))
    # - clean up dataDir
    file.remove(paste0(dataDir, list.files(dataDir)))
  }
} else {
  # - toReport
  print("NOTE: found data in dataDir; checking if this is the first data intake.")
  # - check if this is the first data intake:
  if (length(list.files(aggregateDir)) == 0) {
    # - toReport
    print("NOTE: aggregateDir is empty; so, this is the first data intake.")
    print("Producing the first aggregated dataset.")
    # - produce the first aggregated data set from dataDir
    dataSet <- lapply(paste0(dataDir, list.files(dataDir)), 
                      data.table::fread)
    datetime <- list.files(dataDir)
    datetime <- sapply( strsplit(datetime, split = "_"), function(x) {
      x[3]
    })
    datetime <- sapply(strsplit(datetime, split = ".", fixed = T), 
                       function(x) {x [1]})
    for (i in 1:length(dataSet)) {
      dt <- strsplit(datetime[i], split = "-")[[1]]
      dataSet[[i]]$year <- dt[1]
      dataSet[[i]]$month <- dt[2]
      dataSet[[i]]$day <- dt[3]
    }
    dataSet <- data.table::rbindlist(dataSet)
    colnames(dataSet)[1:4] <- c("namespace_id", 
                                "access_method", 
                                "agent_type", 
                                "pageviews")
    namespaces <- data.frame(namespace = c(0, 120, 146, 640), 
                             namespace_name = c("Item", 
                                                "Property", 
                                                "Lexeme", 
                                                "EntitySchema"))
    dataSet$namespace <- sapply(dataSet$namespace_id, function(x) {
      w <- which(namespaces$namespace == x)
      return(namespaces$namespace_name[w])
    })
    dataSet$timestamp <- 
      as.POSIXct(paste(dataSet$year, 
                       dataSet$month, 
                       dataSet$day, 
                       sep = "-"))
    # - store first aggregated file
    saveRDS(dataSet, 
            paste0(aggregateDir, 'WD_pageviewsPerType.Rds'))
    # - clean up dataDir
    file.remove(paste0(dataDir, list.files(dataDir)))
  } else {
    # - toReport
    print("NOTE: aggregateDir is not empty; so, this is not the first data intake.")
    print("Found already collected new data to append; appending now.")
    # - append new data to the aggregated data set
    dataSet <- lapply(paste0(dataDir, list.files(dataDir)), 
                      data.table::fread)
    datetime <- list.files(dataDir)
    datetime <- sapply( strsplit(datetime, split = "_"), 
                        function(x) {
                          x[3]
                          })
    datetime <- sapply(strsplit(datetime, split = ".", fixed = T), 
                       function(x) {
                         x [1]
                         })
    for (i in 1:length(dataSet)) {
      dt <- strsplit(datetime[i], split = "-")[[1]]
      dataSet[[i]]$year <- dt[1]
      dataSet[[i]]$month <- dt[2]
      dataSet[[i]]$day <- dt[3]
    }
    dataSet <- data.table::rbindlist(dataSet)
    colnames(dataSet)[1:4] <- c("namespace_id", 
                                "access_method", 
                                "agent_type", 
                                "pageviews")
    namespaces <- data.frame(namespace = c(0, 120, 146, 640), 
                             namespace_name = c("Item", 
                                                "Property", 
                                                "Lexeme", 
                                                "EntitySchema"))
    dataSet$namespace <- sapply(dataSet$namespace_id, function(x) {
      w <- which(namespaces$namespace == x)
      return(namespaces$namespace_name[w])
    })
    dataSet$timestamp <- 
      as.POSIXct(paste(dataSet$year, 
                       dataSet$month, 
                       dataSet$day, 
                       sep = "-"))
    # - load existing dataset from aggregateDir
    existingData <- 
      readRDS(paste0(aggregateDir, "WD_pageviewsPerType.Rds")) 
    # - append new data
    existingData <- rbind(existingData, 
                          dataSet)
    # - check for duplicated entries
    existingData$control <- paste(existingData$namespace_id, 
                                  existingData$access_method,
                                  existingData$timestamp,
                                  existingData$agent_type,
                                  sep = "-")
    w <- which(duplicated(existingData$control))
    if (length(w) > 0) {
      existingData <- existingData[-w, ]
    }
    existingData$control <- NULL
    # - store first aggregated file  
    saveRDS(existingData, 
            paste0(aggregateDir, 'WD_pageviewsPerType.Rds'))
    # - clean up dataDir
    file.remove(paste0(dataDir, list.files(dataDir)))
  }
}

# - copy aggregated data set to publicDir
# - toRuntime log:
print("Copy to public directory.")
system(command = 
         paste0('cp ', 
                aggregateDir, 
                '* ' , 
                publicDir),
       wait = T)

# - copy log logArchiveDir
print("Clean up logArchiveDir directory.")
lF <- list.files(logArchiveDir)
if (length(lF) > 0) {
  file.remove(paste0(
    logArchiveDir, 
    list.files(logArchiveDir))
    )
}
print("Copy log to logArchiveDir directory.")
system(command =
         paste0('mv ',
                logDir,
                'WD_PageviewsPerType_RuntimeLOG.log ',
                logArchiveDir),
       wait = T)
