#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdcmModule_Orchestra.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate WDCM modules:
### --- 1. wdcmModule_CollectItems.R
### --- 2. wdcmModule_ETL.py (PySpark)
### --- 3. wdcmModule_ML.R
### --- 4. wdcmModule_Compose.R
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
### ---
### --- WDCM is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDCM is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDCM. If not, see <http://www.gnu.org/licenses/>.

### ---------------------------------------------------------------------------
### --- Script 0: wdcmModule_Orchestra.R
### ---------------------------------------------------------------------------

# - XML
library(XML)

# - to runtime Log:
print(paste("--- wdcmModule_Orchestra.R RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Read WDCM paramereters: wdcmConfig.xml
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
# - fPath: where the scripts is run from?
fPathR <- params$general$fPath_R
fPathPython <- params$general$fPath_Python
# - form paths:
ontologyDir <- params$general$ontologyDir
logDir <- params$general$logDir
itemsDir <- params$general$itemsDir
structureDir <- params$general$structureDir
etlDir <- params$general$etlDir
etlDirGeo <- params$general$etlDirGeo
mlDir <- params$general$mlDir
tempDir <- params$general$tempDir

# - production published-datasets:
dataDir <- params$general$publicDir

# - hdfs ETL paths
hdfsPATH_WDCM_ETL <- params$general$hdfsPATH_WDCM_ETL
hdfsWDCM_ETL_GEODir <- params$general$hdfsPATH_WDCM_ETL_GEO

### --- Read WDCM paramereters: wdcmConfig_Deployment.xml
paramsDeployment <- xmlParse(paste0(fPath, "wdcmConfig_Deployment.xml"))
paramsDeployment <- xmlToList(paramsDeployment)
# - spark2-submit parameters:
sparkMaster <- paramsDeployment$spark$master
sparkDeployMode <- paramsDeployment$spark$deploy_mode
sparkNumExecutors <- paramsDeployment$spark$num_executors
sparkDriverMemory <- paramsDeployment$spark$driver_memory
sparkExecutorMemory <- paramsDeployment$spark$executor_memory
sparkConfigDynamic <- paramsDeployment$spark$config

### --------------------------------------------------
### --- log Orchestra START:
### --------------------------------------------------
# - toRuntime Log:
print("Log: START")
# - set log dir:
setwd(logDir)
# - write to WDCM main reporting file:
lF <- list.files()
if ('WDCM_MainReport.csv' %in% lF) {
  mainReport <- read.csv('WDCM_MainReport.csv',
                         header = T,
                         row.names = 1,
                         check.names = F,
                         stringsAsFactors = F)
  newReport <- data.frame(Step = 'Orchestra START',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, 'WDCM_MainReport.csv')
} else {
  newReport <- data.frame(Step = 'Orchestra START',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  write.csv(newReport, 'WDCM_MainReport.csv')
}

### --------------------------------------------------
### --- Run wdcmModule_CollectItems.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdcmModule_CollectItems.R")

system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPathR, 'wdcmModule_CollectItems.R '),
                        paste0(logDir, '> wdcmModule_CollectItems_LOG.log 2>&1')
                        ),
       wait = T)

### --------------------------------------------------
### --- Run wdcmModule_ETL.py
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdcmModule_ETL.py")

# - clean ETL dirs
if (length(list.files(etlDir)) > 1) {
  file.remove(paste0(etlDir, list.files(etlDir)))
}
if (length(list.files(etlDirGeo)) > 1) {
  file.remove(paste0(etlDirGeo, list.files(etlDirGeo)))
}

# - Kerberos init
system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
       wait = T)

# - Run PySpark ETL
system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                        sparkMaster, ' ',
                        sparkDeployMode, ' ',
                        sparkNumExecutors, ' ',
                        sparkDriverMemory, ' ',
                        sparkExecutorMemory, ' ',
                        sparkConfigDynamic, ' ',
                        paste0(fPathPython, 'wdcmModule_ETL.py')
                        ),
       wait = T)

### --------------------------------------------------
### --- log ETL:
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: ETL step completed.")
# - set log dir:
setwd(logDir)
# - write to WDCM main reporting file:
lF <- list.files()
if ('WDCM_MainReport.csv' %in% lF) {
  mainReport <- read.csv('WDCM_MainReport.csv',
                         header = T,
                         row.names = 1,
                         check.names = F,
                         stringsAsFactors = F)
  newReport <- data.frame(Step = 'ETL',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, 'WDCM_MainReport.csv')
} else {
  newReport <- data.frame(Step = 'ETL',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  write.csv(newReport, 'WDCM_MainReport.csv')
}

### --------------------------------------------------
### --- Run wdcmModule_ML.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdcmModule_ML.R")

system(command = paste0('export USER=goransm && nice -10 Rscript  ', 
                        paste0(fPathR, 'wdcmModule_ML.R '),
                        paste0(logDir, '> wdcmModule_ML_LOG.log 2>&1')
                        ),
       wait = T)

### --------------------------------------------------
### --- log ML:
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: ML step completed.")
# - set log dir:
setwd(logDir)
# - write to WDCM main reporting file:
lF <- list.files()
if ('WDCM_MainReport.csv' %in% lF) {
  mainReport <- read.csv('WDCM_MainReport.csv',
                         header = T,
                         row.names = 1,
                         check.names = F,
                         stringsAsFactors = F)
  newReport <- data.frame(Step = 'ML',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, 'WDCM_MainReport.csv')
} else {
  newReport <- data.frame(Step = 'ML',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  write.csv(newReport, 'WDCM_MainReport.csv')
}

### --------------------------------------------------
### --- Run wdcmModule_Compose.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdcmModule_Compose.R")

system(command = paste0('export USER=goransm && nice -10 Rscript  ', 
                        paste0(fPathR, 'wdcmModule_Compose.R '),
                        paste0(logDir, '> wdcmModule_Compose.log 2>&1')
),
wait = T)

### --------------------------------------------------
### --- log Compose:
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: Compose step completed.")
# - set log dir:
setwd(logDir)
# - write to WDCM main reporting file:
lF <- list.files()
if ('WDCM_MainReport.csv' %in% lF) {
  mainReport <- read.csv('WDCM_MainReport.csv',
                         header = T,
                         row.names = 1,
                         check.names = F,
                         stringsAsFactors = F)
  newReport <- data.frame(Step = 'Compose',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, 'WDCM_MainReport.csv')
} else {
  newReport <- data.frame(Step = 'Compose',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  write.csv(newReport, 'WDCM_MainReport.csv')
}

### --------------------------------------------------
### --- copy ETL/ML outputs to public directory:
### --------------------------------------------------

# - toRuntime log:
print("Copy ETL outputs to public directory.")
# - copy ETL outputs
system(command = 
         paste0('cp ', etlDir, '* ' , dataDir, 'etl/'),
       wait = T)
# - toRuntime log:
print("Copy ETL GEO outputs to public directory.")
# - copy ETL GEO outputs
system(command = 
         paste0('cp ', etlDirGeo, '* ' , dataDir, 'geo/'),
       wait = T)
# - toRuntime log:
print("Copy ML results to public directory.")
# - copy ML results
system(command = 
         paste0('cp ', mlDir, '* ' , dataDir, 'ml/'),
       wait = T)

### --------------------------------------------------
### --- log Orchestra END:
### --------------------------------------------------
# - toRuntime Log:
print("Log: END Orchestra")
# - set log dir:
setwd(logDir)
# - write to WDCM main reporting file:
lF <- list.files()
if ('WDCM_MainReport.csv' %in% lF) {
  mainReport <- read.csv('WDCM_MainReport.csv',
                         header = T,
                         row.names = 1,
                         check.names = F,
                         stringsAsFactors = F)
  newReport <- data.frame(Step = 'Orchestra END',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, 'WDCM_MainReport.csv')
} else {
  newReport <- data.frame(Step = 'Orchestra END',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  write.csv(newReport, 'WDCM_MainReport.csv')
}

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- wdcmModule_Orchestra.R RUN COMPLETED IN: ", 
             generalT2 - generalT1, "."))

### --------------------------------------------------
### --- copy and clean up log files:
### --------------------------------------------------
# - copy the main log file to published for timestamp
# - toRuntime log:
print("Copy main log to published; clean up log.")
system(command = 
         paste0('cp ', logDir, 'WDCM_MainReport.csv ' , dataDir),
       wait = T)
# - archive:
lF <- list.files(logDir)
lF <- lF[grepl('log$|Errors', lF)]
lapply(lF, function(x) {
  system(command = 
           paste0('cp ', logDir, x, ' ', logDir, 'archive/'),
         wait = T)
})
# - clean up
file.remove(paste0(logDir, lF))
# - conclusion
print("DONE. Exiting.")

