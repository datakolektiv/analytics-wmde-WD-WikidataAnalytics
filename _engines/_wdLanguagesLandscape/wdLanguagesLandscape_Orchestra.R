#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdLanguagesLandscape_Orchestra.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- August 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate WD Languages Landscape modules:
### --- 1. WD_LanguagesLandscape.py
### --- 2. wdll_mapReduce.R
### --- 3. wdll_Similarity.R
### --- 4. wdll_DataModel.R
### --- 5. wdll_Analytics.R
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
### --- Script 0: wdLanguagesLandscape_Orchestra.R
### ---------------------------------------------------------------------------

### --- Setup

# - to runtime Log:
print(paste("--- WD_LanguagesLandscape.R RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Read WLP paramereters
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

# - pars
params <- XML::xmlParse(paste0(fPath, "WD_LanguagesLandscape_Config.xml"))
params <- XML::xmlToList(params)

# - dirs
dataDir <- params$general$dataDir
logDir <- params$general$logDir
outDir <- params$general$outDir
publicDir <- params$general$pubDataDir
hdfsPath <- params$general$hdfsPath

### --------------------------------------------------
### --- log Orchestra START:
### --------------------------------------------------
# - toRuntime Log:
print("Log: START")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("WLLP_MainReport.csv" %in% lF) {
  mainReport <- read.csv("WLLP_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "WLLP Orchestra START",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "WLLP Orchestra START",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "WLLP_MainReport.csv")
}

### --------------------------------------------------
### --- Run Pyspark: WD_LanguagesLandscape.py
### --------------------------------------------------

### --- Read WLLP paramereters: WD_LanguagesLandscape_Config_Deploy.xml
params <- XML::xmlParse(paste0(fPath, 
                          "WD_LanguagesLandscape_Config_Deploy.xml"))
params <- XML::xmlToList(params)
sparkMaster <- params$spark$master
sparkDeployMode <- params$spark$deploy_mode
sparkNumExecutors <- params$spark$num_executors
sparkDriverMemory <- params$spark$driver_memory
sparkExecutorMemory <- params$spark$executor_memory
sparkExecutorCores <- params$spark$executor_cores
sparkConfigDynamic <- params$spark$config

# - clean dataDir
if (length(list.files(dataDir)) > 1) {
  file.remove(paste0(dataDir, list.files(dataDir))) 
}

### --- Spark ETL: WD Dump processing
# - to runtime Log:
print(paste("--- wd_processDump_Spark.py Pyspark ETL Procedures STARTED ON:", 
            Sys.time(), sep = " "))
# - Kerberos init
system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
       wait = TRUE)
system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                        sparkMaster, ' ',
                        sparkDeployMode, ' ', 
                        sparkDriverMemory, ' ',
                        sparkExecutorMemory, ' ',
                        sparkExecutorCores, ' ',
                        sparkConfigDynamic, ' ',
                        paste0(fPath, "wdll_PysparkETL.py")),
       wait = TRUE)

### --------------------------------------------------
### --- log Pyspark: WD_LanguagesLandscape.py
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: Pyspark: WD_LanguagesLandscape.py step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("WLLP_MainReport.csv" %in% lF) {
  mainReport <- read.csv("WLLP_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "Pyspark ETL",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "Pyspark ETL",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "WLLP_MainReport.csv")
}

### --------------------------------------------------
### --- Run MapReduce ETL: wdll_mapReduce.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdll_mapReduce.R")

system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPath, 'wdll_mapReduce.R '),
                        paste0(logDir, '> wdll_mapReduce_LOG.log 2>&1')
                        ),
       wait = TRUE)

### --------------------------------------------------
### --- log wdll_mapReduce.R
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: wdll_mapReduce.R step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("WLLP_MainReport.csv" %in% lF) {
  mainReport <- read.csv("WLLP_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "MapReduce ETL",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "MapReduce ETL",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "WLLP_MainReport.csv")
}

### --------------------------------------------------
### --- Run Similarity: wdll_Similarity.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdll_Similarity.R")

system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPath, 'wdll_Similarity.R '),
                        paste0(logDir, '> wdll_Similarity_LOG.log 2>&1')
),
wait = TRUE)

### --------------------------------------------------
### --- log wdll_Similarity.R
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: wdll_Similarity.R step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("WLLP_MainReport.csv" %in% lF) {
  mainReport <- read.csv("WLLP_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "Similarity",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "Similarity",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "WLLP_MainReport.csv")
}

### --------------------------------------------------
### --- Run Data Model: wdll_DataModel.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdll_DataModel.R")
system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPath, 'wdll_DataModel.R '),
                        paste0(logDir, '> wdll_DataModel_LOG.log 2>&1')),
       wait = TRUE)

### --------------------------------------------------
### --- log wdll_DataModel.R
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: wdll_DataModel.R step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("WLLP_MainReport.csv" %in% lF) {
  mainReport <- read.csv("WLLP_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "Data Model",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "Data Model",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "WLLP_MainReport.csv")
}

### --------------------------------------------------
### --- Run Anallytics: wdll_Analytics.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN wdll_Analytics.R")

system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPath, 'wdll_Analytics.R '),
                        paste0(logDir, '> wdll_Analytics_LOG.log 2>&1')
                        ),
       wait = TRUE)

### --------------------------------------------------
### --- log wdll_Analytics.R
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: wdll_Analytics.R step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("WLLP_MainReport.csv" %in% lF) {
  mainReport <- read.csv("WLLP_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "Analytics",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "Analytics",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "WLLP_MainReport.csv")
}


### --------------------------------------------------
### --- copy outputs to public directory:
### --------------------------------------------------

# - toRuntime log:
print("Copy outputs to public directory.")
write(paste0("Last updated on: ", Sys.time()), 
      paste0(outDir, "WDLanguagesUpdateString.txt"))

cFiles <- c("WD_Languages_OntologyStructure.csv",
            "WD_Languages_UsedLanguages.csv",
            "WD_Languages_Jaccard_Similarity.csv",
            "WD_Vis_UNESCO\\ Language\\ Status_Sitelinks.csv",
            "WD_Vis_EthnologueLanguageStatus_Sitelinks.csv",
            "WD_Vis_UNESCO\\ Language\\ Status_NumItems.csv",
            "WD_Vis_EthnologueLanguageStatus_NumItems.csv",
            "WD_Vis_UNESCO\\ Language\\ Status_ItemReuse.csv",
            "WD_Vis_EthnologueLanguageStatus_ItemReuse.csv",
            "wd_languages_count.csv", 
            "WDLanguagesUpdateString.txt")
for (i in 1:length(cFiles)) {
  print(paste0("Copying: ", cFiles[i], " to publicDir."))
  system(command = 
           paste0('cp ', outDir, cFiles[i], ' ', publicDir),
         wait = TRUE)
}

### --------------------------------------------------
### --- log Orchestra END:
### --------------------------------------------------
# - toRuntime Log:
print("Log: END WLLP Orchestra")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("WLLP_MainReport.csv" %in% lF) {
  mainReport <- read.csv("WLLP_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "WLLP Orchestra END",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "WLLP Orchestra END",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, 'WLLP_MainReport.csv')
}

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- wdLanguagesLandscape_Orchestra.R RUN COMPLETED IN: ", 
             generalT2 - generalT1, "."))

### --------------------------------------------------
### --- copy and clean up log files:
### --------------------------------------------------
# - copy the main log file to published for timestamp
# - toRuntime log:
print("Copy main log to published; clean up log.")
system(command = 
         paste0('cp ', logDir, 'WLLP_MainReport.csv ' , dataDir),
       wait = TRUE)
# - archive:
lF <- list.files(logDir)
lF <- lF[grepl("log$|Errors", lF)]
lapply(lF, function(x) {
  system(command = 
           paste0('cp ', logDir, x, ' ', logDir, 'archive/'),
         wait = TRUE)
})
# - clean up
file.remove(paste0(logDir, lF))
# - conclusion
print("DONE. Exiting.")

