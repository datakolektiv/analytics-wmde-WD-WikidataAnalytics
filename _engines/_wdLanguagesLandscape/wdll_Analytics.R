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
params <- XML::xmlParse(paste0(
  fPath, "WD_LanguagesLandscape_Config.xml")
  )
params <- XML::xmlToList(params)

### --- dirs
dataDir <- params$general$dataDir
logDir <- params$general$logDir
outDir <- params$general$outDir
publicDir <- params$general$pubDataDir
hdfsPath <- params$general$hdfsPath

# - Set proxy
# - to runtime Log:
print(paste("--- wdll_DataModel.R: set proxy.", 
            Sys.time(), sep = " "))
WMDEData::set_proxy(http_proxy = params$general$http_proxy, 
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
                          header = TRUE, 
                          check.names = FALSE,
                          row.names = 1,
                          stringsAsFactors = FALSE)

# - to runtime Log:
print(paste("--- wdll_Analytics.R: produce visualization datasets.", 
            Sys.time(), sep = " "))

# - visualize: UNESCOLanguageStatus (P3823) vs. numSitelinks
pFrame <- usedLanguages %>%
  dplyr::select(language, description, UNESCOLanguageStatus, numSitelinks)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$UNESCOLanguageStatus <- gsub("\\(Q.+$", "", pFrame$UNESCOLanguageStatus)
colnames(pFrame) <- c("Language Code", 
                      "Language", 
                      "UNESCO Language Status", 
                      "Sitelinks")
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_UNESCO Language Status_Sitelinks.csv"))

# - visualize: UNESCOLanguageStatus (P3823) vs. number of items (labels)
pFrame <- usedLanguages %>%
  dplyr::select(language, description, UNESCOLanguageStatus, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$UNESCOLanguageStatus <- gsub("\\(Q.+$", "", pFrame$UNESCOLanguageStatus)
colnames(pFrame) <- c("Language Code", 
                      "Language", 
                      "UNESCO Language Status", 
                      "Labels")
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_UNESCO Language Status_NumItems.csv"))

# - visualize: UNESCOLanguageStatus (P3823) vs. item reuse
pFrame <- usedLanguages %>%
  dplyr::select(language, description, UNESCOLanguageStatus, 
                reuse, num_items_reused, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$UNESCOLanguageStatus <- gsub("\\(Q.+$", "", pFrame$UNESCOLanguageStatus)
colnames(pFrame) <- c("Language Code", 
                      "Language", 
                      "UNESCO Language Status", 
                      "Reuse", 
                      "Items Reused", 
                      "Items")
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_UNESCO Language Status_ItemReuse.csv"))

# - visualize: EthnologueLanguageStatus (P3823) vs. numSitelinks
pFrame <- usedLanguages %>%
  dplyr::select(language, description, EthnologueLanguageStatus, numSitelinks)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$EthnologueLanguageStatus <- gsub("\\(Q.+$", "", pFrame$EthnologueLanguageStatus)
colnames(pFrame) <- c("Language Code", 
                      "Language", 
                      "Ethnologue Language Status", 
                      "Sitelinks")
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_EthnologueLanguageStatus_Sitelinks.csv"))

# - visualize: EthnologueLanguageStatus (P3823) vs. number of items (labels)
pFrame <- usedLanguages %>%
  dplyr::select(language, description, EthnologueLanguageStatus, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$EthnologueLanguageStatus <- gsub("\\(Q.+$", "", pFrame$EthnologueLanguageStatus)
colnames(pFrame) <- c("Language Code", 
                      "Language", 
                      "Ethnologue Language Status", 
                      "Labels")
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_EthnologueLanguageStatus_NumItems.csv"))

# - visualize: EthnologueLanguageStatus (P3823) vs. item reuse
pFrame <- usedLanguages %>%
  dplyr::select(language, description, EthnologueLanguageStatus, 
                reuse, num_items_reused, item_count)
pFrame <- pFrame[complete.cases(pFrame), ]
pFrame$EthnologueLanguageStatus <- gsub("\\(Q.+$", "", pFrame$EthnologueLanguageStatus)
colnames(pFrame) <- c("Language Code", 
                      "Language", 
                      "Ethnologue Language Status", 
                      "Reuse", 
                      "Items Reused", 
                      "Items")
write.csv(pFrame, 
          paste0(outDir, "WD_Vis_EthnologueLanguageStatus_ItemReuse.csv"))

### --- Ontology Structure
# - to runtime Log:
print(paste("--- wdll_Analytics.R: wd_Superclasses_Recurrently() for Ontology Structure.", 
            Sys.time(), sep = " "))
entity <- unique(usedLanguages$languageURI)
myWD <- WMDEData::wdqs_superclasses_recurrently(entity,
                                                language = 'en',
                                                cleanup = TRUE,
                                                fetchSubClasses = FALSE,
                                                fetchCounts = FALSE,
                                                SPARQL_Endpoint = endPointURL
                                                )
saveRDS(myWD, paste0(outDir, "myWD.Rds"))

# - prepate dataSet for dashboard visualization
# - to runtime Log:
print(paste("--- wdll_Analytics.R: prepate dataSet for dashboard visualization.", 
            Sys.time(), sep = " "))
dC <- myWD$structure
dC <- dplyr::filter(dC,
                    ((item %in% entity) | (grepl("lang|ling", dC$itemLabel))) & 
                      ((superClass %in% entity) | (grepl("lang|ling", dC$superClassLabel))))
write.csv(dC, 
          paste0(outDir, "WD_Languages_OntologyStructure.csv"))

# - to runtime Log:
print(paste("--- wdll_Analytics.R ENDED ON:", 
            Sys.time(), sep = " "))
