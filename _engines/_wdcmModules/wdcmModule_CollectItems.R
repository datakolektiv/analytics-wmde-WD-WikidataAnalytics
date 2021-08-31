#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdcmModule_CollectItems.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Contact WDQS and fetch all item IDs from the WDCM WD Ontology
### --- NOTE: the execution of this WDCM script is always dependent upon the
### --- previous WDCM_Sqoop_Clients.R run.
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
### --- Script 1: wdcmModule_CollectItems.R
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- wdcmModule_CollectItems.R takes a list of items (categories)
### --- defined by a given WDCM WD Ontology (human input) and then
### --- contacts the Wikidata Query Service to fetch all relevant item IDs.
### ---------------------------------------------------------------------------
### --- INPUT:
### --- the WDCM_Collect_Items.R reads the WDCM Ontology file (csv)
### --- ACTIVE WDCM TAXONOMY: WDCM_Ontology_Berlin_05032017.csv
### ---------------------------------------------------------------------------


# - to runtime Log:
print(paste("--- wdcmModule_CollectItems.R UPDATE RUN STARTED ON:", 
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
params <- XML::xmlParse(paste0(fPath,
                               "wdcmConfig.xml"))
params <- XML::xmlToList(params)

### --- Set proxy
WMDEData::set_proxy(http_proxy = params$general$http_proxy,
                    https_proxy = params$general$https_proxy)

### --- Directories
# - fPath: where the scripts is run from?
fPath <- params$general$fPath_R
# - form paths:
ontologyDir <- params$general$ontologyDir
logDir <- params$general$logDir
itemsDir <- params$general$itemsDir
itemsGeoDir <- params$general$etlDirGeo
# - production published-datasets:
dataDir <- params$general$publicDir
# - hdfs CollectedItems dir
hdfsDir_WDCMCollectedItemsDir <- 
  params$general$hdfsPATH_WDCMCollectedItems
# - hdfsCollectedGeoItemsDir dir
hdfsCollectedGeoItemsDir <- 
  params$general$hdfsPATH_WDCMCollectedGeoItems

### --- Read WDCM_Ontology
# - to runtime Log:
print("--- Reading Ontology.")
setwd(ontologyDir)
wdcmOntology <- read.csv(
  paste0(params$general$ontologyDir,
         params$general$ontology),
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE)

# - WDQS Classes endPoint
endPointURL <- params$general$wdqs_endpoint

# - set itemsDir:
setwd(itemsDir)

# - clear output dir:
lF <- list.files()
rmF <- file.remove(lF)

# - startTime (WDCM Main Report)
startTime <- as.character(Sys.time())

### --- WDQS/SPARQL/GAS: Collect WDCM_Ontology
for (i in 1:length(wdcmOntology$CategoryItems)) {
  
  # - to runtime Log:
  print(paste0("--- SPARQL category: ", 
               wdcmOntology$WikidataDescription[i])
        )
  
  searchItems <- stringr::str_trim(
    strsplit(wdcmOntology$CategoryItems[i],
             split = ",", 
             fixed = TRUE)[[1]],
    "both")
  
  itemsOut <- list()
  
  for (k in 1:length(searchItems)) {
    
    ### --- fetch all subclasses
    
    # - define targetClass
    targetClass <- searchItems[k]
    
    # - to runtime Log:
    print(paste0("--- SPARQL sub-category: ", targetClass))
    
    # - Construct Query:
    query <- paste0('SELECT ?item WHERE { 
                    SERVICE gas:service { 
                    gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" . 
                    gas:program gas:in wd:', targetClass, ' .
                    gas:program gas:linkType wdt:P279 .
                    gas:program gas:out ?subClass .
                    gas:program gas:traversalDirection "Reverse" .
                    } . 
                    ?item wdt:P31 ?subClass 
                    }')

    # - Run SPARQL/GAS program
    rc <- WMDEData::wdqs_send_query(query = query,
                                    SPARQL_Endpoint = endPointURL,
                                    max_retry = 10)
    
    # - parse result
    rc <- data.frame(item = 
                       unlist(stringr::str_extract_all(
                         rc, 
                         "Q[[:digit:]]+")
                         ),
                     stringsAsFactors = FALSE)
    
    # - collect
    itemsOut[[k]] <- rc
    
    }
  
  # - store
  if (length(itemsOut) > 0) {
    
    # - itemsOut as data.table:
    itemsOut <- data.table::rbindlist(itemsOut)

    # - keep only unique items:
    w <- which(!(duplicated(itemsOut$item)))
    itemsOut <- itemsOut[w]
    
    # store as CSV
    itemsOut$category <- wdcmOntology$Category[i]
    filename <- paste0(wdcmOntology$Category[i],"_ItemIDs.csv")
    # - store
    setwd(itemsDir)
    readr::write_csv(itemsOut, filename)
    
    # - to report
    print(paste0(
      "Collected ", 
          dim(itemsOut)[1], 
          " items in ", 
          wdcmOntology$WikidataDescription[i], 
          ".")
    )
    
    # clear:
    rm(itemsOut); gc()
  }
  
}

### --- Fix WDCM_Ontology (Phab T174896#3762820)
# - to runtime Log:
print("--- Fix WDCM_Ontology (Phab T174896#3762820)")

# - remove Geographical Object from Organization:
organizationItems <- data.table::fread(
  paste0(
    itemsDir, 
    'Organization_ItemIDs.csv')
  )
geoObjItems <- data.table::fread(
  paste0(
    itemsDir, 
    'Geographical Object_ItemIDs.csv')
  )
w <- which(organizationItems$item %in% geoObjItems$item)
if (length(w) > 0) {
  organizationItems <- organizationItems[-w, ]
}
# - store:
readr::write_csv(organizationItems, 'Organization_ItemIDs.csv')
# - clear:
rm(organizationItems); rm(geoObjItems); gc()
# - remove Book from Work of Art:
bookItems <- data.table::fread(
  paste0(
    itemsDir, 
    'Book_ItemIDs.csv')
  )
workOfArtItems <- data.table::fread(
  paste0(
    itemsDir, 
    'Work Of Art_ItemIDs.csv')
  )
w <- which(workOfArtItems$item %in% bookItems$item)
if (length(w) > 0) {
  workOfArtItems <- workOfArtItems[-w, ]
}
# - store:
readr::write_csv(workOfArtItems, 'Work Of Art_ItemIDs.csv')
# - clear:
rm(workOfArtItems); rm(bookItems); gc()
# - remove Architectural Structure from Geographical Object:
architectureItems <- data.table::fread(
  paste0(
    itemsDir, 
    'Architectural Structure_ItemIDs.csv')
  )
geoObjItems <- data.table::fread(
  paste0(
    itemsDir, 
    'Geographical Object_ItemIDs.csv')
  )
w <- which(geoObjItems$item %in% architectureItems$item)
if (length(w) > 0) {
  geoObjItems <- geoObjItems[-w, ]
}
# - store:
readr::write_csv(geoObjItems, 'Geographical Object_ItemIDs.csv')
# - clear:
rm(geoObjItems); rm(architectureItems); gc()

### --- log Collect_Items:
# - to runtime Log:
print("--- LOG: Collect_Items step completed.")
# - set log dir:
setwd(logDir)
# - write to WDCM main reporting file:
lF <- list.files()
if ('WDCM_MainReport.csv' %in% lF) {
  mainReport <- read.csv('WDCM_MainReport.csv',
                         header = TRUE,
                         row.names = 1,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
  newReport <- data.frame(Step = 'CollectItems',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, 'WDCM_MainReport.csv')
} else {
  newReport <- data.frame(Step = 'CollectItems',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = FALSE)
  write.csv(newReport, 'WDCM_MainReport.csv')
}

### --- rename files for hdfs
itemFiles <- list.files(itemsDir)
itemFiles <- gsub(" ", "\\ ", itemFiles, fixed = TRUE)
itemNames <- unname(sapply(itemFiles, function(x){
  gsub("\\s|_", "-", x)
}))
for (i in 1:length(itemFiles)) {
  system(command = paste0("mv ", paste0(itemsDir, itemFiles[i]), " ", 
                          paste0(itemsDir, itemNames[i])),
         wait = T)
}

### --- Copy to hdfs
print("---- Move to hdfs.")
# -  delete hdfsDir_WDCMCollectedItemsDir
WMDEData::hdfs_rmdir(kerberosUser = "analytics-privatedata", 
                     hdfsDir = hdfsDir_WDCMCollectedItemsDir)
# -  make hdfsDir_WDCMCollectedItemsDir
WMDEData::hdfs_mkdir(kerberosUser = "analytics-privatedata", 
                     hdfsDir = hdfsDir_WDCMCollectedItemsDir)
# -  copy to hdfsDir_WDCMCollectedItemsDir
hdfsC <- lapply(list.files(itemsDir), function(x) {
  WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata", 
                         localPath = itemsDir,
                         localFilename = x,
                         hdfsDir = hdfsDir_WDCMCollectedItemsDir)
})

### --- Collect GEO items
# - to runtime Log:
print(paste("--- WDCM GeoEngine update STARTED ON:", 
            Sys.time(), 
            sep = " ")
      )
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Read WDCM_GeoItems
# - to runtime Log:
print("--- Reading Ontology.")
wdcmGeoItems <- read.csv(paste0(ontologyDir, 
                                "WDCM_GeoItems_Belgrade_12152017.csv"),
                         header = TRUE,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)

### --- Select all instances accross all sub-classes of searchItems:
# - set itemsDir:
setwd(itemsGeoDir)

# - clear output dir:
lF <- list.files()
rmF <- file.remove(lF)

# - startTime (WDCM Main Report)
startTime <- as.character(Sys.time())

for (i in 1:length(wdcmGeoItems$item)) {
  
  # - to runtime Log:
  print(paste(
    "--- SPARQL category:", 
    i, 
    ":", 
    wdcmGeoItems$itemLabel[i], 
    sep = " ")
    )
  
  searchItems <- stringr::str_trim(wdcmGeoItems$item[i], "both")
  
  # - Construct Query:
  query <- paste0(
    'PREFIX wd: <http://www.wikidata.org/entity/> ',
    'PREFIX wdt: <http://www.wikidata.org/prop/direct/> ',
    'SELECT ?item ?coordinate ?itemLabel WHERE {?item (wdt:P31|(wdt:P31/wdt:P279*)) wd:',
    searchItems, '. ',
    '?item wdt:P625 ?coordinate. ',
    'SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }', 
    ' }'
    )
  
  # - Run SPARQL/GAS program
  rc <- WMDEData::wdqs_send_query(query = query,
                                  SPARQL_Endpoint = endPointURL,
                                  max_retry = 10)
  # - parse result
  rc <- jsonlite::fromJSON(rc, simplifyDataFrame = TRUE)
  # - parse:
  item <- rc$results$bindings$item$value
  coordinate <- rc$results$bindings$coordinate$value
  label <- rc$results$bindings$itemLabel$value
  # - as.data.frame:
  items <- data.frame(item = item,
                      coordinate = coordinate,
                      label = label,
                      stringsAsFactors = FALSE)
  # - clear:
  rm(item); rm(coordinate); rm(label); rm(rc); gc()
  # - keep unique result set:
  w <- which(duplicated(items$item))
  if (length(w) > 0) {items <- items[-w, ]}
  # - clear possible NAs from coordinates
  w <- which(is.na(items$coordinate) | (items$coordinate == ""))
  if (length(w) > 0) {items <- items[-w, ]}
  # - fix items
  items$item <- gsub("http://www.wikidata.org/entity/", 
                     "", 
                     items$item, 
                     fixed = TRUE)
  # - fix coordinates (lon, lat)
  items$coordinate <- gsub("Point(", 
                           "", 
                           items$coordinate, 
                           fixed = TRUE)
  items$coordinate <- gsub(")", 
                           "", 
                           items$coordinate, 
                           fixed = TRUE)
  lon <- stringr::str_extract(items$coordinate, "^.+\\s")
  lat <- stringr::str_extract(items$coordinate, "\\s.+$")
  items$coordinate <- NULL
  items$lon <- lon
  items$lat <- lat
  # clear:
  rm(lon); rm(lat); gc()
  # store as CSV
  readr::write_csv(items, 
                   path = paste0(
                     wdcmGeoItems$itemLabel[i], 
                     "_ItemIDs.csv")
                   )
  
  # - to report
  print(paste0(
    "Collected ", 
    dim(items)[1], 
    " items in ", 
    wdcmGeoItems$itemLabel[i], 
    ".")
  )
  
  # - take a break from WDQS
  print("Pause for 10 secs.")
  Sys.sleep(10)
    
}

### --- rename geo files for hdfs
itemFiles <- list.files(itemsGeoDir)
itemFiles <- gsub(" ", "\\ ", itemFiles, fixed = TRUE)
itemNames <- 
  unname(
    sapply(itemFiles, function(x){
      gsub("\\s|_", "-", x)
      })
    )
for (i in 1:length(itemFiles)) {
  system(command = paste0("mv ", paste0(itemsGeoDir, itemFiles[i]), " ", 
                          paste0(itemsGeoDir, itemNames[i])),
         wait = T)
}

### --- Copy to hdfs
print("---- Move to hdfs.")
# -  delete hdfsDir_WDCMCollectedItemsDir
WMDEData::hdfs_rmdir(kerberosUser = "analytics-privatedata", 
                     hdfsDir = hdfsCollectedGeoItemsDir)
# -  make hdfsDir_WDCMCollectedItemsDir
WMDEData::hdfs_mkdir(kerberosUser = "analytics-privatedata", 
                     hdfsDir = hdfsCollectedGeoItemsDir)
# -  copy to hdfsDir_WDCMCollectedItemsDir
hdfsC <- lapply(list.files(itemsGeoDir), function(x) {
  WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata", 
                         localPath = itemsGeoDir,
                         localFilename = x,
                         hdfsDir = hdfsCollectedGeoItemsDir)
})

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- wdcmModule_CollectItems.R UPDATE DONE IN: ", 
             generalT2 - generalT1, "."))
