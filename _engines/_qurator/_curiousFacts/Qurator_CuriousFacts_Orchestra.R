#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Qurator_CuriousFacts_Orchestra.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- August 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate Qurator Curious Facts modules:
### --- 1. Qurator_CuriousFacts_M1.R
### --- 2. Qurator_CuriousFacts_M2.R
### --- 3. Qurator_CuriousFacts_M3.R
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
print(paste("--- Qurator Curious Facts Orchestra RUN STARTED ON:", 
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
library(WMDEData)

# - pars
params <- XML::xmlParse(
  paste0(fPath,
         "wd_cluster_fetch_items.xml")
  )
params <- XML::xmlToList(params)

# - dirs
dataDir <- paste0(fPath, "_data/")
analyticsDir <- paste0(fPath, "_analytics/")
reportingDir <- paste0(fPath, "_reporting/")
logDir <- paste0(fPath, "_log/")
logArchiveDir <- paste0(fPath, "_log/archive/")
publicDir <- params$publicDir
hdfsDir <- params$hdfsDir

### --------------------------------------------------
### --- log Orchestra START:
### --------------------------------------------------
# - toRuntime Log:
print("Log: START")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("QCF_MainReport.csv" %in% lF) {
  mainReport <- read.csv("QCF_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "QCF Orchestra START",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "WLLP_MainReport.csv")
} else {
  newReport <- data.frame(Step = "QCF Orchestra START",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "QCF_MainReport.csv")
}

### --------------------------------------------------
### --- Run Qurator_CuriousFacts_M1.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN Qurator_CuriousFacts_M1.R")
system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPath, 'Qurator_CuriousFacts_M1.R > '),
                        paste0(logDir, 'Qurator_CuriousFacts_M1.log 2>&1')
                        ),
       wait = TRUE)

### --------------------------------------------------
### --- log Qurator_CuriousFacts_M1.R
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: Qurator_CuriousFacts_M1.R step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("QCF_MainReport.csv" %in% lF) {
  mainReport <- read.csv("QCF_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "M1 anomalies",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "QCF_MainReport.csv")
} else {
  newReport <- data.frame(Step = "M1 anomalies",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "QCF_MainReport.csv")
}

### --------------------------------------------------
### --- Run Qurator_CuriousFacts_M2.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN Qurator_CuriousFacts_M2.R")
system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPath, 'Qurator_CuriousFacts_M2.R > '),
                        paste0(logDir, 'Qurator_CuriousFacts_M2.log 2>&1')
                        ),
       wait = TRUE)

### --------------------------------------------------
### --- log Qurator_CuriousFacts_M2.R
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: Qurator_CuriousFacts_M2.R step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("QCF_MainReport.csv" %in% lF) {
  mainReport <- read.csv("QCF_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "M2 anomalies",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "QCF_MainReport.csv")
} else {
  newReport <- data.frame(Step = "M2 anomalies",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "QCF_MainReport.csv")
}

### --------------------------------------------------
### --- Run Qurator_CuriousFacts_M3.R
### --------------------------------------------------

# - toRuntime Log:
print("Log: RUN Qurator_CuriousFacts_M3.R")

system(command = paste0('export USER=goransm && nice -10 Rscript ',
                        paste0(fPath, 'Qurator_CuriousFacts_M3.R > '),
                        paste0(logDir, 'Qurator_CuriousFacts_M3.log 2>&1')
),
wait = TRUE)

### --------------------------------------------------
### --- log Qurator_CuriousFacts_M3.R
### --------------------------------------------------
# - to runtime Log:
print("--- LOG: Qurator_CuriousFacts_M3.R step completed.")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("QCF_MainReport.csv" %in% lF) {
  mainReport <- read.csv("QCF_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "M3 anomalies",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "QCF_MainReport.csv")
} else {
  newReport <- data.frame(Step = "M3 anomalies",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "QCF_MainReport.csv")
}

### --------------------------------------------------
### --- copy outputs to public directory:
### --------------------------------------------------

# - toRuntime log:
print("Copy outputs to public directory.")
system(command =
         paste0('cp ', reportingDir, "*", " ", publicDir),
       wait = TRUE)

### --------------------------------------------------
### --- log Orchestra END:
### --------------------------------------------------
# - toRuntime Log:
print("Log: END QCF Orchestra")
# - set log dir:
setwd(logDir)
# - write to main reporting file:
lF <- list.files()
if ("QCF_MainReport.csv" %in% lF) {
  mainReport <- read.csv("QCF_MainReport.csv",
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = "QCF Orchestra END",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, "QCF_MainReport.csv")
} else {
  newReport <- data.frame(Step = "QCF Orchestra END",
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, "QCF_MainReport.csv")
}

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- Qurator_CuriousFacts_Orchestra.R RUN COMPLETED IN: ", 
             difftime(generalT2, generalT1, units = "mins"), "."))

### --------------------------------------------------
### --- copy and clean up log files:
### --------------------------------------------------
# - copy the main log file to published for timestamp
# - toRuntime log:
print("Copy main log to published; clean up log.")
system(command = 
         paste0('cp ', logDir, 'QCF_MainReport.csv ' , logArchiveDir),
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

