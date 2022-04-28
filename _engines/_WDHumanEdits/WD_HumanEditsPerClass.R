#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WD_HumanEditsPerClass, v 1.0.0
### --- script: WD_HumanEditsPerClass.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- September 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- ETL and Analytics for the Wikidata Human Edits Per Class Project
### --- (WHEPC)
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- 0: Init
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- WD_HumanEditsPerClass.R RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

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
  paste0(
    fPath, "wdHumanEditsPerClass_Config.xml")
  )
params <- XML::xmlToList(params)

# - dirTree
dataDir <- params$general$dataDir
analyticsDir <- params$general$analyticsDir
hdfsPath <- params$general$hdfsPath
publicDir <- params$general$publicDir

# - spark2-submit parameters:
params <- XML::xmlParse(
  paste0(fPath, "wdHumanEditsPerClass_Config_Deploy.xml")
  )
params <- XML::xmlToList(params)
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
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Run Spark ETL
WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                            pysparkPath = paste0(fPath, "WD_HumanEditsPerClass_ETL.py"),
                            sparkMaster = sparkMaster,
                            sparkDeployMode = sparkDeployMode,
                            sparkNumExecutors = sparkNumExecutors,
                            sparkDriverMemory = sparkDriverMemory,
                            sparkExecutorMemory = sparkExecutorMemory,
                            sparkConfigDynamic = sparkConfigDynamic)

# - toRuntime Log:
print("Log: RUN WD_HumanEditsPerClass_ETL.py COMPLETED.")

### ---------------------------------------------------------------------------
### --- 2: Compose final datasets from hdfs
### ---------------------------------------------------------------------------

### --- compose: humanTouchClass.csv
print(paste(
  "--- Log: Load ETL datasets.",
  "Compose: humanTouchClass.csv. ",
  Sys.time(), sep = " "))
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = dataDir,
                                    localFilenamePrefix = "humanTouchClass",
                                    hdfsDir = hdfsPath,
                                    hdfsFilenamePrefix = "humanTouchClass.csv",
                                    fr_header = FALSE)
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

### --- compose: humanBotClasses.csv
print(paste(
  "--- Log: Load ETL datasets.",
  "Compose: humanBotClasses.csv ",
  Sys.time(), sep = " "))
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = dataDir,
                                    localFilenamePrefix = "humanBotClasses",
                                    hdfsDir = hdfsPath,
                                    hdfsFilenamePrefix = "humanBotClasses.csv",
                                    fr_header = FALSE)
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


### --- compose: classMedianEditors
print(paste(
  "--- Log: Load ETL datasets.",
  "Compose: classMedianEditors ",
  Sys.time(), sep = " "))
dataSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                    localPath = dataDir,
                                    localFilenamePrefix = "classMedianEditors",
                                    hdfsDir = hdfsPath,
                                    hdfsFilenamePrefix = "classMedianEditors.csv",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c('wd_class', 'median_unique_editors', 
                       'num_items', 'label')
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
classMedianEditors <- data.table::fread(
  paste0(analyticsDir, "classMedianEditors.csv")
  )
classMedianEditors$V1 <- NULL
classMedianEditors <- dplyr::arrange(
  classMedianEditors,
  dplyr::desc(num_items)
  )

### --- humanTouchClass
humanTouchClass <- data.table::fread(
  paste0(analyticsDir, "humanTouchClass.csv")
  )
humanTouchClass$V1 <- NULL
head(humanTouchClass)
humanTouchClass <- dplyr::select(humanTouchClass,
                                 wd_class, 
                                 num_items,
                                 human_edited_items, 
                                 percent_items_touched)
classMedianEditors <- dplyr::select(classMedianEditors,
                                    wd_class, 
                                    median_unique_editors, 
                                    label)
humanTouchClass <- dplyr::left_join(humanTouchClass,
                                    classMedianEditors,
                                    by = "wd_class")
rm(classMedianEditors)

### --- humanBotClasses
humanBotClasses <- data.table::fread(
  paste0(analyticsDir, "humanBotClasses.csv")
  )
humanBotClasses$V1 <- NULL
humanBotClasses$label <- NULL
humanBotClasses <- dplyr::select(humanBotClasses,
                                 wd_class,
                                 human_edits, 
                                 bot_edits, 
                                 total_edits,
                                 human_to_bot_ratio,
                                 human_percent, bot_percent)
humanTouchClass <- dplyr::left_join(humanTouchClass,
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

# - compute indicators
humanTouchClass$percent_items_touched <- 
  round(humanTouchClass$percent_items_touched, 2)
humanTouchClass$human_to_bot_ratio <- 
  round(humanTouchClass$human_to_bot_ratio, 2)
humanTouchClass$human_percent <- 
  round(humanTouchClass$human_percent, 2)
humanTouchClass$bot_percent <- 
  round(humanTouchClass$bot_percent, 2)
humanTouchClass <- dplyr::arrange(humanTouchClass, 
                                  dplyr::desc(num_items))
class <- humanTouchClass$wd_class
humanTouchClass$wd_class <- 
  paste0("https://www.wikidata.org/wiki/", 
         humanTouchClass$wd_class)
humanTouchClass$wd_class <- 
  paste0('<a href = "', 
         humanTouchClass$wd_class, 
         '" target = "_blank">',
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
         paste0('cp ', 
                analyticsDir, 
                'WD_HumanEdits.csv ', 
                publicDir, 
                'WD_HumanEdits.csv'),
       wait = T)
# - toRuntime log:
print("Copy output: COMPLETED.")

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(
  paste0("--- WD_HumanEditsPerClass.R RUN COMPLETED IN: ",
         generalT2 - generalT1, "."))
