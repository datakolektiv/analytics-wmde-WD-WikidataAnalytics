#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdcmModule_Compose.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Reshape and wrangle WDCM data sets for WDCM Dashboards
### --- NOTE: the execution of this WDCM script is always dependent upon the
### --- previous WDCM_Sqoop_Clients.R run, as well
### --- as the previous execution of wdcmModule_CollectItems.R and 
### --- wdcmModule_ETL.py (Pyspark ETL), wdcmModule_ML.R (ML)
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

### ---------------------------------------------------------------------------
### --- Script 5: wdcmModule_Compose.R
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- wdcmModule_Compose.R UPDATE RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Setup
library(httr)
library(XML)
library(jsonlite)
# - wrangling:
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(data.table)

### --- functions
# - projectType() to determine project type
projectType <- function(projectName) {
  unname(sapply(projectName, function(x) {
    if (grepl("commons", x, fixed = T)) {"Commons"
    } else if (grepl("mediawiki|meta|species|wikidata", x)) {"Other"
    } else if (grepl("wiki$", x)) {"Wikipedia"
    } else if (grepl("quote$", x)) {"Wikiquote"
    } else if (grepl("voyage$", x)) {"Wikivoyage"
    } else if (grepl("news$", x)) {"Wikinews"
    } else if (grepl("source$", x)) {"Wikisource"
    } else if (grepl("wiktionary$", x)) {"Wiktionary"
    } else if (grepl("versity$", x)) {"Wikiversity"
    } else if (grepl("books$", x)) {"Wikibooks"
    } else {"Other"}
  }))
}

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
# - fPath: where the scripts is run from?
fPath <- params$general$fPath_R
# - form paths:
ontologyDir <- params$general$ontologyDir
logDir <- params$general$logDir
itemsDir <- params$general$itemsDir
structureDir <- params$general$structureDir
etlDir <- params$general$etlDir
mlDir <- params$general$mlDir
mlInputDir <- params$general$mlInputDir
# - production published-datasets:
dataDir <- params$general$publicDir

### ----------------------------------------------
### --- Production: Public Data Sets
### ----------------------------------------------

### --- produce wdcm_category.csv
print("Produce wdcm_category.csv now...")
setwd(etlDir)
lF <- list.files()
lF <- lF[grepl("^wdcm_category_sum_", lF)]
wdcm_category <- lapply(lF, fread)
wdcm_category <- rbindlist(wdcm_category)
write.csv(wdcm_category, "wdcm_category.csv")
print("DONE.")

### --- produce wdcm_project_category_item_100.csv
print("Produce wdcm_project_category_item_100.csv now...")
setwd(etlDir)
lF <- list.files()
lF <- lF[grepl("^wdcm_project_category_item100", lF)]
wdcm_project_category_item100 <- lapply(lF, fread)
wdcm_project_category_item100 <- rbindlist(wdcm_project_category_item100)
wdcm_project_category_item100$eu_label <- 
  gsub('^""|""$', '', wdcm_project_category_item100$eu_label)
wdcm_project_category_item100$eu_label <- 
  ifelse(wdcm_project_category_item100$eu_label == "", 
         wdcm_project_category_item100$eu_entity_id,
         wdcm_project_category_item100$eu_label)
write.csv(wdcm_project_category_item100, 
          "wdcm_project_category_item100.csv")
print("DONE.")

### --- produce wdcm_category_item.csv
print("Produce wdcm_category_item.csv now...")
setwd(etlDir)
lF <- list.files()
lF <- lF[grepl("^wdcm_category_item_", lF)]
catNames <- sapply(lF, function(x) {
  strsplit(x, split = ".", fixed = T)[[1]][1]
})
catNames <- unname(sapply(catNames, function(x) {
  strsplit(x, split = "_", fixed = T)[[1]][4]
}))
wdcm_category_item <- lapply(lF, fread)
for (i in 1:length(wdcm_category_item)) {
  wdcm_category_item[[i]]$Category <- catNames[i]
}
wdcm_category_item <- rbindlist(wdcm_category_item)
wdcm_category_item$eu_label <- 
  gsub('^""|""$', '', wdcm_category_item$eu_label)
wdcm_category_item$eu_label <- 
  ifelse(wdcm_category_item$eu_label == "", 
         wdcm_category_item$eu_entity_id,
         wdcm_category_item$eu_label)
write.csv(wdcm_category_item, 
          "wdcm_category_item.csv")
print("DONE.")

### --- fix labels for wdcm_project_item100.csv
print("Fix labels for wdcm_project_item100.csv now...")
setwd(etlDir)
wdcm_project_item100 <- fread('wdcm_project_item100.csv')
wdcm_project_item100$eu_label <- 
  gsub('^""|""$', '', wdcm_project_item100$eu_label)
wdcm_project_item100$eu_label <- 
  ifelse(wdcm_project_item100$eu_label == "", 
         wdcm_project_item100$eu_entity_id,
         wdcm_project_item100$eu_label)
setwd(etlDir)
write.csv(wdcm_project_item100, 
          "wdcm_project_item100_labels.csv")
print("Fix labels for wdcm_project_item100.csv now...")

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- wdcmModule_Compose.R UPDATE DONE IN: ", 
             generalT2 - generalT1, "."))
