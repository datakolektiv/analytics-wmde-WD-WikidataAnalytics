#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WD_HumanEditsPerClass, v 0.0.1
### --- script: WD_HumanEditsPerClass.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- ETL and Analytics for the Wikidata Human Edits Per Class Project
### --- (WHEPC)
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Human Edits Per Class Project (WHEPC)
### ---
### --- WHEPC is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WHEPC is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WHEPC If not, see <http://www.gnu.org/licenses/>.

### ---------------------------------------------------------------------------
### --- 0: Init
### ---------------------------------------------------------------------------

### --- library
library(XML)
library(data.table)
library(tidyverse)

# - to runtime Log:
print(paste("--- WD_HumanEditsPerClass.R RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Read paramereters
# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = T)
fPath <- unlist(strsplit(fPath, split = "/", fixed = T))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")
params <- xmlParse(paste0(fPath, "wdHumanEditsPerClass_Config.xml"))
params <- xmlToList(params)

### --- dirTree
dataDir <- params$general$dataDir
analyticsDir <- params$general$analyticsDir
hdfsPath <- params$general$hdfsPath
publicDir <- params$general$publicDir

params <- xmlParse(paste0(fPath, "wdHumanEditsPerClass_Config_Deploy.xml"))
params <- xmlToList(params)
# - spark2-submit parameters:
sparkMaster <- params$spark$master
sparkDeployMode <- params$spark$deploy_mode
sparkNumExecutors <- params$spark$num_executors
sparkDriverMemory <- params$spark$driver_memory
sparkExecutorMemory <- params$spark$executor_memory
sparkExecutorCores <- params$spark$executor_cores
sparkConfigDynamic <- params$spark$config

### ---------------------------------------------------------------------------
### --- 1: Run Pyspark ETL
### ---------------------------------------------------------------------------

# - toRuntime Log:
print("Log: RUN WD_HumanEditsPerClass_ETL.py")

# - clean dataDir
if (length(list.files(dataDir)) > 1) {
  file.remove(paste0(dataDir, list.files(dataDir)))
}
# - Kerberos init
system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
       wait = T)
# - Run PySpark ETL
system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                        sparkMaster, ' ',
                        sparkDeployMode, ' ', 
                        sparkDriverMemory, ' ',
                        sparkExecutorMemory, ' ',
                        sparkExecutorCores, ' ',
                        sparkConfigDynamic, ' ',
                        paste0(fPath, 'WD_HumanEditsPerClass_ETL.py')),
       wait = T)

# - toRuntime Log:
print("Log: RUN WD_HumanEditsPerClass_ETL.py COMPLETED.")

### ---------------------------------------------------------------------------
### --- 2: Compose final datasets from hdfs
### ---------------------------------------------------------------------------

### --- dataset: humanTouchClass
# - copy splits from hdfs to local dataDir
system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ', 
              hdfsPath, 'humanTouchClass.csv > ', 
              dataDir, 'files.txt'), 
       wait = T)
files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
files <- as.character(files$V8)[2:length(as.character(files$V8))]
file.remove(paste0(dataDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ', 
                files[i], ' > ',  
                paste0(dataDir, "humanTouchClass", i, ".csv")), wait = T)
}
# - read splits: dataSet
# - load
lF <- list.files(dataDir)
lF <- lF[grepl("humanTouchClass", lF)]
dataSet <- lapply(paste0(dataDir, lF), function(x) {fread(x, 
                                                          header = F, 
                                                          sep = ",")})
# - collect
dataSet <- rbindlist(dataSet)
# - clean up
file.remove(paste0(dataDir, list.files(dataDir)))
# - schema
colnames(dataSet) <- c('wd_class', 'human_edited_items', 'num_items', 
                       'proportion_items_touched', 'percent_items_touched', 'label')
# - sanitize
# - find empty classes, if any
wEmpty <- which(!grepl("^Q", dataSet$wd_class))
if (length(wEmpty > 0)) {
  dataSet <- dataSet[-wEmpty, ]
}
# - sort by proportion_items_touched, num_items, desc
dataSet <- dataSet[order(-proportion_items_touched, -num_items)]
# - store in analyticsDir
write.csv(dataSet, 
          paste0(analyticsDir, "humanTouchClass.csv"))
rm(dataSet); gc()

### --- dataSet: humanBotClasses
# - copy splits from hdfs to local dataDir
system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ', 
              hdfsPath, 'humanBotClasses.csv > ', 
              dataDir, 'files.txt'), 
       wait = T)
files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
files <- as.character(files$V8)[2:length(as.character(files$V8))]
file.remove(paste0(dataDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ', 
                files[i], ' > ',  
                paste0(dataDir, "humanBotClasses", i, ".csv")), wait = T)
}
# - read splits: dataSet
# - load
lF <- list.files(dataDir)
lF <- lF[grepl("humanBotClasses", lF)]
dataSet <- lapply(paste0(dataDir, lF), function(x) {fread(x, header = F)})
# - collect
dataSet <- rbindlist(dataSet)
# - clean up
file.remove(paste0(dataDir, list.files(dataDir)))
# - schema
colnames(dataSet) <- c('wd_class', 'human_edits', 'bot_edits', 
                       'total_edits', 'human_to_bot_ratio', 'human_ratio', 
                       'bot_ratio', 'human_percent', 'bot_percent', 'label')
# - sanitize
# - find empty classes, if any
wEmpty <- which(!grepl("^Q", dataSet$wd_class))
if (length(wEmpty > 0)) {
  dataSet <- dataSet[-wEmpty, ]
}
# - sort by proportion_items_touched, num_items, desc
dataSet <- dataSet[order(-human_to_bot_ratio, -total_edits)]
# - store in analyticsDir
write.csv(dataSet, 
          paste0(analyticsDir, "humanBotClasses.csv"))
rm(dataSet); gc()

### --- dataSet: classMedianEditors
# - copy splits from hdfs to local dataDir
system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ', 
              hdfsPath, 'classMedianEditors.csv > ', 
              dataDir, 'files.txt'), 
       wait = T)
files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
files <- as.character(files$V8)[2:length(as.character(files$V8))]
file.remove(paste0(dataDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ', 
                files[i], ' > ',  
                paste0(dataDir, "classMedianEditors", i, ".csv")), wait = T)
}
# - read splits: dataSet
# - load
lF <- list.files(dataDir)
lF <- lF[grepl("classMedianEditors", lF)]
dataSet <- lapply(paste0(dataDir, lF), function(x) {fread(x, header = F)})
# - collect
dataSet <- rbindlist(dataSet)
# - clean up
file.remove(paste0(dataDir, list.files(dataDir)))
# - schema
colnames(dataSet) <- c('wd_class', 'median_unique_editors', 'num_items', 'label')
# - sanitize
# - find empty classes, if any
wEmpty <- which(!grepl("^Q", dataSet$wd_class))
if (length(wEmpty > 0)) {
  dataSet <- dataSet[-wEmpty, ]
}
# - sort by median_unique_editors, num_items, desc
dataSet <- dataSet[order(-median_unique_editors, -num_items)]
# - store in analyticsDir
write.csv(dataSet, 
          paste0(analyticsDir, "classMedianEditors.csv"))
rm(dataSet); gc()

### ---------------------------------------------------------------------------
### --- 3: Analytics
### ---------------------------------------------------------------------------

### --- classMedianEditors
classMedianEditors <- fread(paste0(analyticsDir, "classMedianEditors.csv"))
classMedianEditors$V1 <- NULL
classMedianEditors <- arrange(classMedianEditors, 
                              desc(num_items))

### --- humanTouchClass
humanTouchClass <- fread(paste0(analyticsDir, "humanTouchClass.csv"))
humanTouchClass$V1 <- NULL
head(humanTouchClass)
humanTouchClass <- select(humanTouchClass, 
                          wd_class, num_items,
                          human_edited_items, percent_items_touched)
classMedianEditors <- select(classMedianEditors, 
                             wd_class, median_unique_editors, label)
humanTouchClass <- left_join(humanTouchClass, 
                             classMedianEditors,
                             by = "wd_class")
rm(classMedianEditors)

### --- humanBotClasses
humanBotClasses <- fread(paste0(analyticsDir, "humanBotClasses.csv"))
humanBotClasses$V1 <- NULL
humanBotClasses$label <- NULL
humanBotClasses <- select(humanBotClasses,
                          wd_class,
                          human_edits, bot_edits, total_edits,
                          human_to_bot_ratio,
                          human_percent, bot_percent)
humanTouchClass <- left_join(humanTouchClass,
                             humanBotClasses,
                             by = "wd_class")
rm(humanBotClasses)
humanTouchClass <- humanTouchClass[, c('wd_class',
                                       'label',
                                       'num_items',
                                       'human_edited_items',
                                       'percent_items_touched',
                                       'median_unique_editors',
                                       'human_edits',
                                       'bot_edits',
                                       'total_edits',
                                       'human_to_bot_ratio',
                                       'human_percent',
                                       'bot_percent')]
humanTouchClass$percent_items_touched <- round(humanTouchClass$percent_items_touched, 2)
humanTouchClass$human_to_bot_ratio <- round(humanTouchClass$human_to_bot_ratio, 2)
humanTouchClass$human_percent <- round(humanTouchClass$human_percent, 2)
humanTouchClass$bot_percent <- round(humanTouchClass$bot_percent, 2)
humanTouchClass <- arrange(humanTouchClass, desc(num_items))
class <- humanTouchClass$wd_class
humanTouchClass$wd_class <- paste0("https://www.wikidata.org/wiki/", humanTouchClass$wd_class)
humanTouchClass$wd_class <- paste0('<a href = "', humanTouchClass$wd_class, '" target = "_blank">',
                                   class,
                                   "</a>")
write.csv(humanTouchClass, 
          paste0(analyticsDir, 'WD_HumanEdits.csv'))

### ---------------------------------------------------------------------------
### --- 4: Publish
### ---------------------------------------------------------------------------

# - toRuntime log:
print("Copy outputs to public directory.")
# - copy ETL outputs
system(command = 
         paste0('cp ', analyticsDir, 'WD_HumanEdits.csv ' , publicDir, 'WD_HumanEdits.csv'),
       wait = T)
# - toRuntime log:
print("Copy output: COMPLETED.")

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- WD_HumanEditsPerClass.R RUN COMPLETED IN: ", 
             generalT2 - generalT1, "."))





