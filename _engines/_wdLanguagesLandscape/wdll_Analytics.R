#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdll_Analytics.RR
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- August 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate WD Languages Landscape modules:
### --- 5. wdll_Analytics.R
### --- Analytics & Visualizations in WDLL
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
### --- Script 5: wdll_Analytics.R
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- wdll_Analytics.R RUN STARTED ON:", 
            Sys.time(), sep = " "))

### --- Setup

# - to runtime Log:
print(paste("--- wdll_Analytics.R: read params.", 
            Sys.time(), sep = " "))
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
library(XML)
library(data.table)
library(stringr)
library(spam)
library(spam64)
library(text2vec)
library(WikidataR)
library(httr)
library(jsonlite)
library(dplyr)
library(htmltab)
library(tidyr)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(scales)
library(igraph)

# - pars
params <- xmlParse(paste0(fPath, "WD_LanguagesLandscape_Config.xml"))
params <- xmlToList(params)

### --- dirs
dataDir <- params$general$dataDir
logDir <- params$general$logDir
outDir <- params$general$outDir
publicDir <- params$general$pubDataDir
hdfsPath <- params$general$hdfsPath

# - funs
# - to runtime Log:
print(paste("--- wdll_Analytics.R: source functions.", 
            Sys.time(), sep = " "))
source(paste0(fPath, 'wdll_Functions.R'))

#- set proxy
Sys.setenv(
  http_proxy = params$general$http_proxy,
  https_proxy = params$general$http_proxy)

# - WDQS endpoint
endPointURL <- params$general$wdqs_endpoint

# - publicDir
publicDir <- params$general$pubDataDir

### --- used languages

# - to runtime Log:
print(paste("--- wdll_Analytics.R: load WD_Languages_UsedLanguages.csvn as usedLanguages.", 
            Sys.time(), sep = " "))
usedLanguages <- read.csv(paste0(outDir, "WD_Languages_UsedLanguages.csv"),
                          header = T, 
                          check.names = F,
                          row.names = 1,
                          stringsAsFactors = F)

# - to runtime Log:
print(paste("--- wdll_Analytics.R: produce visualization datasets.", 
            Sys.time(), sep = " "))

# - visualize: UNESCOLanguageStatus (P3823) vs. numSitelinks
pFrame <- usedLanguages %>%
  dplyr::select(language, description, UNESCOLanguageStatus, numSitelinks)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$UNESCOLanguageStatus <- gsub("\\(Q.+$", "", pFrame$UNESCOLanguageStatus)
colnames(pFrame) <- c('Language Code', 
                      'Language', 
                      'UNESCO Language Status', 
                      'Sitelinks')
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_UNESCO Language Status_Sitelinks.csv"))

# - visualize: UNESCOLanguageStatus (P3823) vs. number of items (labels)
pFrame <- usedLanguages %>%
  dplyr::select(language, description, UNESCOLanguageStatus, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$UNESCOLanguageStatus <- gsub("\\(Q.+$", "", pFrame$UNESCOLanguageStatus)
colnames(pFrame) <- c('Language Code', 
                      'Language', 
                      'UNESCO Language Status', 
                      'Labels')
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_UNESCO Language Status_NumItems.csv"))

# - visualize: UNESCOLanguageStatus (P3823) vs. item reuse
pFrame <- usedLanguages %>%
  dplyr::select(language, description, UNESCOLanguageStatus, 
                reuse, num_items_reused, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$UNESCOLanguageStatus <- gsub("\\(Q.+$", "", pFrame$UNESCOLanguageStatus)
colnames(pFrame) <- c('Language Code', 
                      'Language', 
                      'UNESCO Language Status', 
                      'Reuse', 
                      'Items Reused', 
                      'Items')
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_UNESCO Language Status_ItemReuse.csv"))

# - visualize: EthnologueLanguageStatus (P3823) vs. numSitelinks
pFrame <- usedLanguages %>%
  dplyr::select(language, description, EthnologueLanguageStatus, numSitelinks)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$EthnologueLanguageStatus <- gsub("\\(Q.+$", "", pFrame$EthnologueLanguageStatus)
colnames(pFrame) <- c('Language Code', 
                      'Language', 
                      'Ethnologue Language Status', 
                      'Sitelinks')
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_EthnologueLanguageStatus_Sitelinks.csv"))

# - visualize: EthnologueLanguageStatus (P3823) vs. number of items (labels)
pFrame <- usedLanguages %>%
  dplyr::select(language, description, EthnologueLanguageStatus, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$EthnologueLanguageStatus <- gsub("\\(Q.+$", "", pFrame$EthnologueLanguageStatus)
colnames(pFrame) <- c('Language Code', 
                      'Language', 
                      'Ethnologue Language Status', 
                      'Labels')
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_EthnologueLanguageStatus_NumItems.csv"))

# - visualize: EthnologueLanguageStatus (P3823) vs. item reuse
pFrame <- usedLanguages %>%
  dplyr::select(language, description, EthnologueLanguageStatus, 
                reuse, num_items_reused, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$EthnologueLanguageStatus <- gsub("\\(Q.+$", "", pFrame$EthnologueLanguageStatus)
colnames(pFrame) <- c('Language Code', 
                      'Language', 
                      'Ethnologue Language Status', 
                      'Reuse', 
                      'Items Reused', 
                      'Items')
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_EthnologueLanguageStatus_ItemReuse.csv"))

### --- wd_Superclasses_Recurrently() for Ontology Structure

# - to runtime Log:
print(paste("--- wdll_Analytics.R: wd_Superclasses_Recurrently() for Ontology Structure.", 
            Sys.time(), sep = " "))
entity <- unique(usedLanguages$languageURI)
myWD <- wd_Superclasses_Recurrently(entity = entity, 
                                    language = 'en', 
                                    cleanup = T,
                                    fetchSubClasses = F,
                                    fetchCounts = F,
                                    SPARQL_Endpoint = endPointURL)

saveRDS(myWD, 
        paste0(outDir, 'myWD.Rds'))

# - prepate dataSet for dashboard visualization
# - to runtime Log:
print(paste("--- wdll_Analytics.R: prepate dataSet for dashboard visualization.", 
            Sys.time(), sep = " "))
dC <- myWD$structure
dC <- dplyr::filter(dC,
                    ((item %in% entity) | (grepl("lang|ling", dC$itemLabel))) & 
                      ((superClass %in% entity) | (grepl("lang|ling", dC$superClassLabel))))
write.csv(dC, 
          paste0(outDir, 'WD_Languages_OntologyStructure.csv'))

# - to runtime Log:
print(paste("--- wdll_Analytics.R ENDED ON:", 
            Sys.time(), sep = " "))
