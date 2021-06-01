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
library(data.table)
library(WikidataR)


### --- dirTree
fPath <- paste0(getwd(), "/")
dataDir <- paste0(fPath, "_data/")
analyticsDir <- paste0(fPath, "_analytics/")
reportingDir <- paste0(fPath, "_reporting/")
hdfsDir <- 'hdfs:///tmp/wmde/analytics/qurator/'

### --- functions
source(paste0(fPath, 'Q_CF_Functions.R'))

### --- Set proxy for stat100*
Sys.setenv(
  http_proxy = 'http://webproxy.eqiad.wmnet:8080',
  https_proxy = 'http://webproxy.eqiad.wmnet:8080')

### --- WDQS endpoint
sparqlEndPointURL <- 
  "https://query.wikidata.org/bigdata/namespace/wdq/sparql?format=json&query="

### ----------------------------------------------------------------------------
### --- Module 2: Property constraints/Subject class
### --- ETL/Problem Solver
### ----------------------------------------------------------------------------

### --- obtain property constraints from Wikidata
propConstraints <- wd_fetchPropertyConstraints(sparqlEndPointURL)

### --- load problems
m2_problems <- read.csv(paste0(fPath, 'm2_problems.csv'), 
                        stringsAsFactors = F, 
                        header = T)

# - iterate over problems and solve
failed <- numeric()
for (i in 29:dim(m2_problems)[1]) {
  
  print(paste0("--------------- ", 
               "Solving now problem : ", 
               i, "/", dim(m2_problems)[1], ".",
               " --------------- ")
  )
  
  # - define problem
  targetProperty <- m2_problems$targetProperty[i]
  
  # - define property constraints
  targetClasses <- propConstraints %>% 
    dplyr::filter(grepl(
      paste0(targetProperty, "$"), 
      propConstraints$`Property_.value`))
  targetClasses <- targetClasses[, c('Property_.value',
                                     'Property_Label.value',
                                     'class_.value', 
                                     'class_Label.value',
                                     'relation_.value', 
                                     'relation_Label.value'
  )]
  targetClasses$Property_.value <- gsub('http://www.wikidata.org/entity/', 
                                        '',
                                        targetClasses$Property_.value)
  targetClasses$class_.value <- gsub('http://www.wikidata.org/entity/',
                                     '',
                                     targetClasses$class_.value)
  targetClasses$relation_.value <- gsub('http://www.wikidata.org/entity/',
                                        '',
                                        targetClasses$relation_.value)
  
  # - extract relation
  wdRelation <- lapply(unique(targetClasses$relation_.value), 
                       function(x) {
                         p <- WikidataR:: get_item(id = x)
                         p <- p[[1]]$claims$P1687
                         return(p$mainsnak$datavalue$value$id)  
                       })
  names(wdRelation) <- unique(targetClasses$relation_.value)
  wdRelation <- stack(wdRelation)
  wdRelation <- rbind(wdRelation)
  # - join wdRelation to targetClasses
  targetClasses <- dplyr::left_join(targetClasses, 
                                    wdRelation, 
                                    by = c('relation_.value' = 'ind'))
  targetClasses <- targetClasses %>% 
    dplyr::select('values', 'class_.value')
  colnames(targetClasses) <- c('property', 'class')
  targetClasses <- targetClasses %>% 
    dplyr::group_by(property) %>% 
    dplyr::summarise(classes = paste(class, collapse = ', '))
  
  # - report problem
  print(paste0("This problem is: ", targetProperty, "."))

  # - clean up dataDir
  lF <- list.files(dataDir)
  lF <- lF[grepl('^results_M2', lF)]
  if (length(lF) > 0) {
    file.remove(paste0(dataDir, lF))
  }
  
  # - solve problem
  t1 <- Sys.time()
  wd_cluster_fetch_items_M2(targetProperty = targetProperty,
                            referenceProperty = targetClasses$property,
                            referenceClasses = targetClasses$classes,
                            fPath = fPath,
                            dataDir = dataDir)
  timeTaken <- difftime(Sys.time(), t1, units = "mins")
  print(paste0("This took: ", round(timeTaken, 2), " minutes."))
  
  # - read result - and process it, if there is any
  system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ', 
                hdfsDir, ' > ', 
                dataDir, 'files.txt'), 
         wait = T)
  files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
  files <- as.character(files$V8[grepl("result_M2", files$V8)])
  
  if (length(files) > 0) {
    
    # - collect wdDumpSnapshot
    wdDumpSnapshot <- strsplit(files, "/")[[1]][8]
    wdDumpSnapshot <- gsub("result_M2_", "", wdDumpSnapshot)
    wdDumpSnapshot <- gsub(".csv", "", wdDumpSnapshot, fixed = T)
    system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
                  files, ' > ', 
                  dataDir, 'files.txt'), 
           wait = T)
    files <- read.table(paste0(dataDir, 'files.txt'), skip = 1)
    files <- as.character(files$V8)[2:length(as.character(files$V8))]
    file.remove(paste0(dataDir, 'files.txt'))
    
      for (j in 1:length(files)) {
        system(paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ', 
                      files[j], ' > ',  
                      paste0(dataDir, "results_M2_", j, ".csv")), wait = T)
      }
      # - read splits: dataSet
      # - load
      lF <- list.files(dataDir)
      lF <- lF[grepl("results_M2_", lF)]
      dataSet <- lapply(paste0(dataDir, lF), 
                        function(x) {fread(x, header = F)})
      # - collect
      dataSet <- rbindlist(dataSet)
      
      if (dim(dataSet)[1] > 0) {
      
        # - schema
        colnames(dataSet) <- c('item', 'property')
        
        # - PREPARE OUTPUT: targetClasses$classes, Explanation, timestamp, etc.
        # - add Wikidata JSON Dump
        dataSet$wdDumpSnapshot <- wdDumpSnapshot
        # - add metadata
        dataSet$establishedOn <- as.character(Sys.time())
        dataSet$problemType <- 'M2'
        dataSet$timeTaken <- timeTaken
        
        # - fetch 'en' item labels
        print("--- fetch 'en' item labels")
        items <- unique(dataSet$item)
        itemLabs <- wd_api_fetch_labels(items = items,
                                        language = 'en',
                                        fallback = T)
        colnames(itemLabs) <- c('item', 'itemLabel')
        dataSet <- dplyr::left_join(dataSet,
                                    itemLabs,
                                    by = 'item')
        # - fetch 'en' referenceClasses labels
        items <-  targetClasses$classes
        items <- strsplit(items, ", ")[[1]]
        itemLabs <- wd_api_fetch_labels(items = items,
                                        language = 'en',
                                        fallback = T)
        itemLabs <- paste0(itemLabs$en_label, " (", itemLabs$title, ")")
        itemLabs <- paste(itemLabs, 
                          collapse = ", ")
        dataSet$referenceClasses <- itemLabs
        # - fetch propertLabel
        propLab <- wd_api_fetch_labels(items = unique(dataSet$property),
                                       language = 'en',
                                       fallback = T)
        dataSet$propertyLab <- propLab$en_label
        head(dataSet)
        
        # - add explanation
        dataSet$explanation <- paste0(dataSet$itemLabel, " (", dataSet$item, ") ", 
                                      "has property ", dataSet$propertyLab, " (", dataSet$property, "), ", 
                                      "but the item is not found in any of the following classes: ", 
                                      dataSet$referenceClasses)
        dataSet$referenceClasses <- NULL
        colnames(dataSet)
        dataSet <- dplyr::select(dataSet, 
                                 item, 
                                 itemLabel, 
                                 property,
                                 propertyLab, 
                                 explanation, 
                                 problemType, 
                                 wdDumpSnapshot,
                                 establishedOn, 
                                 timeTaken)
        
        # - store results
        prop <- unique(dataSet$property)
        CSVfilename <- paste0("M2_", propLab$title,
                              "_", propLab$en_label, 
                              "_problems_solved.csv")
        CSVfilename <- gsub(" ", "_", CSVfilename, fixed = T)
        write.csv(dataSet,
                  paste0(analyticsDir, CSVfilename))
        
      } else {
        print("------ FATAL: The following problem failed:")
        print(paste0("--------------- ", 
                     "The problem that FAILED is : ", 
                     i, "/", dim(m2_problems)[1], ".",
                     " --------------- ")
        )
        failed <- append(failed, i)
      }
  
  } else {
    print("------ FATAL: The following problem failed:")
    print(paste0("--------------- ", 
                 "The problem that FAILED is : ", 
                 i, "/", dim(m2_problems)[1], ".",
                 " --------------- ")
    )
    failed <- append(failed, i)
  }
  
}


### ----------------------------------------------------------------------------
### --- Module 2: Property constraints/Subject class
### --- Reporter
### ----------------------------------------------------------------------------

### --- Collect solved M2 problems
lF <- list.files(analyticsDir)
lF <- lF[grepl("^M2", lF)]
dataM2 <- lapply(paste0(analyticsDir, lF), fread, header = T)
dataM2 <- rbindlist(dataM2)
dataM2$V1 <- NULL
infoM2 <- list(problemType = unique(dataM2$problemType),
               wdDumpSnapshot = unique(dataM2$wdDumpSnapshot)
               )
infoM2 <- as.data.frame(infoM2)
write.csv(infoM2, 
          paste0(reportingDir, "infoM2.csv"))
dataM2 <- dplyr::select(
  dataM2, 
  item, itemLabel, 
  property, propertyLab,
  explanation, establishedOn
)
colnames(dataM2)[4] <- 'propertyLabel'
write.csv(dataM2, 
          paste0(reportingDir, "dataM2.csv"))

### --- File info on unsolved
m2_problems$solved <- TRUE
m2_problems$solved[failed] <- FALSE
write.csv(m2_problems, 
          paste0(reportingDir, "m2_problems_solved.csv"))






