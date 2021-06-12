#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Current Events
### --- Script: Q_CE_Functions.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: functions to support the Qurator Project(s) 
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of QURATOR Current Events
### ---
### --- QURATOR Current Events is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- QURATOR Current Events is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with QURATOR Current Events If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Setup
library(tidyverse)
library(httr)
library(jsonlite)
library(WikidataR)

### --- dirTree
fPath <- paste0(getwd(), "/")
dataDir <- paste0(fPath, "_data/")
analyticsDir <- paste0(fPath, "_analytics/")
reportingDir <- paste0(fPath, "_reporting/")

### --- functions
source(paste0(fPath, 'Q_CF_Functions.R'))

### --- Set proxy for stat100*
Sys.setenv(
  http_proxy = 'http://webproxy.eqiad.wmnet:8080',
  https_proxy = 'http://webproxy.eqiad.wmnet:8080')

### ----------------------------------------------------------------------------
### --- Module 1: targetProperty, datavalue:item, against referenceClass
### --- ETL/Problem Solver
### ----------------------------------------------------------------------------

# - load problems
m1_problems <- read.csv(paste0(fPath, 'm1_problems.csv'), 
                        stringsAsFactors = F, 
                        header = T)

# - iterate over problems and solve
failed <- numeric()
for (i in 3:dim(m1_problems)[1]) {
  
  print(paste0("--------------- ", 
               "Solving now problem : ", 
               i, "/", dim(m1_problems)[1], ".",
               " --------------- ")
  )
  
  # - define problem
  class <- m1_problems$class[i]
  targetProperty <- m1_problems$targetProperty[i]
  referenceClass <- m1_problems$referenceClass[i]
  
  # - determine classLabel
  classLabel <- get_property(class)
  classLabel <- classLabel[[1]]$label$en$value
  # - determine targetPropertyLabel
  targetPropertyLabel <- get_property(targetProperty)
  targetPropertyLabel <- targetPropertyLabel[[1]]$label$en$value
  # - determine referenceClassLabel
  referenceClassLabel <- get_property(referenceClass)
  referenceClassLabel <- referenceClassLabel[[1]]$label$en$value
  
  # - report problem
  print(paste0("This problem is: ", 
               paste0(class, "_", classLabel), 
               "; ", 
               paste0(targetProperty, "_", targetPropertyLabel), 
               "; ",
               "; ", 
               paste0(referenceClass, "_", referenceClassLabel), 
               "; ")
        )
  
  # - clean up dataDir
  lF <- list.files(dataDir)
  lF <- lF[grepl('^result_M1', lF)]
  if (length(lF) > 0) {
    file.remove(paste0(dataDir, lF))
  }
  
  # - solve problem
  t1 <- Sys.time()
  wd_cluster_fetch_items_M1(class = class,
                            targetProperty = targetProperty,
                            referenceClass = referenceClass,
                            fPath = fPath,
                            dataDir = dataDir)
  timeTaken <- difftime(Sys.time(), t1, units = "mins")
  print(paste0("This took: ", round(timeTaken, 2), " minutes."))
  
  # - read result - and process it, if there is any
  lF <- list.files(dataDir)
  lF <- lF[grepl('^result_M1', lF)]
  if (length(lF) > 0) {
    
    dataSet <- read.csv(paste0(dataDir, lF), 
                        stringsAsFactors = F, 
                        header = T)
    dataSet <- dataSet[, c('id', 'propValue')]
    colnames(dataSet) <- c('id', 'propertyValue')
    dataSet$property <- targetProperty
    dataSet$propertyLabel <- targetPropertyLabel
    dataSet$referenceClass <- referenceClass
    dataSet$referenceClassLabel <- referenceClassLabel
    
    # - fetch 'en' item labels
    items <- unique(dataSet$id)
    itemLabs <- wd_api_fetch_labels(items = items,
                                    language = 'en',
                                    fallback = T)
    colnames(itemLabs) <- c('id', 'itemLabel')
    dataSet <- dplyr::left_join(dataSet,
                                itemLabs,
                                by = 'id')
    
    # - fetch 'en' propValue labels
    items <- unique(dataSet$propertyValue)
    itemLabs <- wd_api_fetch_labels(items = items,
                                    language = 'en',
                                    fallback = T)
    colnames(itemLabs) <- c('propertyValue', 'propertyValueLabel')
    dataSet <- dplyr::left_join(dataSet,
                                itemLabs,
                                by = 'propertyValue')
    
    # - fix for No label defined
    dataSet$itemLabel <- ifelse(dataSet$itemLabel == 'No label defined', 
                                dataSet$id, 
                                dataSet$itemLabel)
    dataSet$propertyValueLabel <- ifelse(dataSet$propertyValueLabel == 'No label defined',
                                          dataSet$propertyValue,
                                          dataSet$propertyValueLabel)

    # - add explanation
    tItem <- paste0('<a href="https://www.wikidata.org/wiki/', 
                    dataSet$id, 
                    '" target = "_blank">', 
                    dataSet$itemLabel, 
                    '</a>')
    tProperty1 <- paste0('<a href="https://www.wikidata.org/wiki/Property:',
                         targetProperty,
                         '" target = "_blank">',
                         targetPropertyLabel,
                         '</a>')
    tProperty2 <- paste0('<a href="https://www.wikidata.org/wiki/',
                         dataSet$propertyValue,
                         '" target = "_blank">',
                         dataSet$propertyValueLabel,
                         '</a>')
    tReferenceClass <- paste0('<a href="https://www.wikidata.org/wiki/',
                              referenceClass,
                              '" target = "_blank">',
                              referenceClassLabel,
                              '</a>')
    dataSet$explanation <- paste0(tItem, 
                                  ' has a Statement "',
                                  tProperty1,
                                  ': ',
                                  tProperty2, 
                                  '" but ', 
                                  dataSet$propertyValueLabel, 
                                  ' is not found in the class "', 
                                  tReferenceClass, 
                                  '".')

    
    # - add metadata
    dumpSnapshot <- gsub("\\.csv", "", 
                         strsplit(lF, split = "_")[[1]][3])
    dataSet$wdDumpSnapshot <- dumpSnapshot
    dataSet$establishedOn <- as.character(Sys.time())
    dataSet$problemType <- 'M1'
    dataSet$timeTaken <- timeTaken
    
    # - store results
    CSVfilename <- paste0("M1_", classLabel, 
                          "_", targetPropertyLabel, 
                          "_", referenceClassLabel, 
                          ".csv")
    CSVfilename <- gsub(" ", "_", CSVfilename, fixed = T)
    write.csv(dataSet, 
              paste0(analyticsDir, CSVfilename)) 
    
  } else {
    print("------ FATAL: The following problem failed:")
    print(paste0("This problem is: ", 
                 paste0(class, "_", classLabel), 
                 "; ", 
                 paste0(targetProperty, "_", targetPropertyLabel), 
                 "; ",
                 "; ", 
                 paste0(referenceClass, "_", referenceClassLabel), 
                 "; "))
    failed <- append(failed, i)
  }
  
}

### ----------------------------------------------------------------------------
### --- Module 1: targetProperty, datavalue:item, against referenceClass
### --- Reporter
### ----------------------------------------------------------------------------

### --- Collect solved M1 problems
lF <- list.files(analyticsDir)
lF <- lF[grepl("^M1", lF)]
dataM1 <- lapply(paste0(analyticsDir, lF), fread, header = T)
dataM1 <- rbindlist(dataM1)
dataM1$V1 <- NULL
infoM1 <- list(problemType = unique(dataM1$problemType),
               wdDumpSnapshot = unique(dataM1$wdDumpSnapshot)
               )
infoM1 <- as.data.frame(infoM1)
write.csv(infoM1, 
          paste0(reportingDir, "infoM1.csv"))
dataM1 <- dplyr::select(
  dataM1, 
  id, itemLabel, 
  property, propertyLabel,
  propertyValue, propertyValueLabel,
  referenceClass, referenceClassLabel,
  explanation, establishedOn
)
colnames(dataM1)[1] <- 'item'
write.csv(dataM1, 
          paste0(reportingDir, "dataM1.csv"))

### --- File info on unsolved
m1_problems$solved <- TRUE
m1_problems$solved[failed] <- FALSE
write.csv(m1_problems, 
          paste0(reportingDir, "m1_problems_solved.csv"))

