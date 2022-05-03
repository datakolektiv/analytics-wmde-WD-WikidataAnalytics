#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Current Events (QCE)
### --- Version 1.0.0
### --- Script: curiousfactsrevolver.R
### --- December 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: data update for the Qurator Curious Facts system
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### --- Setup
library(httr)

### --- Directory Tree
dataDir <- "data/"
pubDir <- 
  "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/qurator/curious_facts/"

### --- Revolver

repeat {
  
  ### --- download
  
  # - download M1 data
  url <- paste0(pubDir, "dataM1.csv")
  httr::GET(url, write_disk(paste0(dataDir, "dataM1.csv"), overwrite = TRUE))
  # - download M1 info
  url <- paste0(pubDir, "infoM1.csv")
  httr::GET(url, write_disk(paste0(dataDir, "infoM1.csv"), overwrite = TRUE))
  
  # - download M2 data
  url <- paste0(pubDir, "dataM2.csv")
  httr::GET(url, write_disk(paste0(dataDir, "dataM2.csv"), overwrite = TRUE))
  # - download M2 info
  url <- paste0(pubDir, "infoM2.csv")
  httr::GET(url, write_disk(paste0(dataDir, "infoM2.csv"), overwrite = TRUE))
  
  # - download M3 data
  url <- paste0(pubDir, "dataM3.csv")
  httr::GET(url, write_disk(paste0(dataDir, "dataM3.csv"), overwrite = TRUE))
  # - download M3 info
  url <- paste0(pubDir, "infoM3.csv")
  httr::GET(url, write_disk(paste0(dataDir, "infoM3.csv"), overwrite = TRUE))
  
  ### --- wait
  
  cycle <- 24*60*60
  Sys.sleep(cycle)
  
}
