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
### --- Script 5: wdcmModule_Compose.R
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- wdcmModule_Compose.R UPDATE RUN STARTED ON:", 
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
params <- XML::xmlParse(paste0(fPath, "wdcmConfig.xml"))
params <- XML::xmlToList(params)

# - proxy
WMDEData::set_proxy(http_proxy = params$general$http_proxy, 
                    https_proxy = params$general$http_proxy)

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
wdcm_category <- lapply(lF, data.table::fread)
wdcm_category <- data.table::rbindlist(wdcm_category)
write.csv(wdcm_category, "wdcm_category.csv")
print("DONE.")

### --- produce wdcm_project_category_item_100.csv
print("Produce wdcm_project_category_item_100.csv now...")
setwd(etlDir)
lF <- list.files()
lF <- lF[grepl("^wdcm_project_category_item100", lF)]
wdcm_project_category_item100 <- lapply(lF, function(x) {
  d <- data.table::fread(x)
  d$V1 <- NULL
  return(d)
  })
wdcm_project_category_item100 <- 
  data.table::rbindlist(wdcm_project_category_item100)
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
wdcm_category_item <- lapply(lF, data.table::fread)
for (i in 1:length(wdcm_category_item)) {
  wdcm_category_item[[i]]$Category <- catNames[i]
}
wdcm_category_item <- data.table::rbindlist(wdcm_category_item)
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
wdcm_project_item100 <- data.table::fread('wdcm_project_item100.csv')
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

### --- fetch labels for itemtopic matrices
### --- USE: wd_api_fetch_labels() from wdcmModule_Compose.R
lF <- list.files(mlDir)
lF <- lF[grepl("wdcm2_itemtopic", lF)]
for (i in 1:length(lF)) {
  print(paste0("Fetch English labels for: ", lF[i]))
  itemtopicFrame <- read.csv(paste0(mlDir, lF[i]),
                             header = T,
                             stringsAsFactors = F)
  apiPF <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'
  labs <- WMDEData::api_fetch_labels(items = itemtopicFrame$X,
                                     language = "en",
                                     fallback = TRUE,
                                     APIprefix = apiPF)
  itemtopicFrame <- dplyr::left_join(itemtopicFrame,
                                     labs,
                                     by = c("X" = "item"))
  colnames(itemtopicFrame)[length(colnames(itemtopicFrame))] <- 
    "en_label"
  itemtopicFrame$en_label[is.na(itemtopicFrame$en_label)] <- 
    itemtopicFrame$X
  itemtopicFrame$X <- paste0(itemtopicFrame$en_label, " (", 
                             itemtopicFrame$X, ")")
  itemtopicFrame$en_label <- NULL
  write.csv(itemtopicFrame, 
            paste0(mlDir, lF[i]))
}

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- wdcmModule_Compose.R UPDATE DONE IN: ", 
             generalT2 - generalT1, "."))
