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

### --- Setup

# - contact:
library(httr)
library(XML)
library(jsonlite)
# - wrangling:
library(stringr)
library(readr)
library(data.table)
library(tidyr)

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

### --- Set proxy
Sys.setenv(
  http_proxy = params$general$http_proxy,
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
hdfsDir_WDCMCollectedItemsDir <- params$general$hdfsPATH_WDCMCollectedItems
# - hdfsCollectedGeoItemsDir dir
hdfsCollectedGeoItemsDir <- params$general$hdfsPATH_WDCMCollectedGeoItems

### --- Read WDCM_Ontology
# - to runtime Log:
print("--- Reading Ontology.")
setwd(ontologyDir)
wdcmOntology <- read.csv(paste0(params$general$ontologyDir, params$general$ontology),
                         header = T,
                         check.names = F,
                         stringsAsFactors = F)

# - WDQS Classes endPoint
endPointURL <- params$general$wdqs_endpoint

# - set itemsDir:
setwd(itemsDir)

# - clear output dir:
lF <- list.files()
rmF <- file.remove(lF)

# - track uncompleted queries:
qErrors <- character()

# - startTime (WDCM Main Report)
startTime <- as.character(Sys.time())

for (i in 1:length(wdcmOntology$CategoryItems)) {
  
  # - to runtime Log:
  print(paste0("--- SPARQL category: ", wdcmOntology$WikidataDescription[i]))
  
  searchItems <- str_trim(
    strsplit(wdcmOntology$CategoryItems[i],
             split = ",", fixed = T)[[1]],
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

    # - Run Query:
    repeat {
      res <- tryCatch({
        GET(url = paste0(endPointURL, URLencode(query)))
      },
      error = function(condition) {
        print("Something's wrong on WDQS: wait 10 secs, try again.")
        Sys.sleep(10)
        GET(url = paste0(endPointURL, URLencode(query)))
      },
      warning = function(condition) {
        print("Something's wrong on WDQS: wait 10 secs, try again.")
        Sys.sleep(10)
        GET(url = paste0(endPointURL, URLencode(query)))
      }
      )  
      if (res$status_code == 200) {
        print(paste0(targetClass, ": success."))
        break
      } else {
        print(paste0(targetClass, ": failed; retry."))
        Sys.sleep(10)
        }
    }
    
    # - Extract item IDs:
    if (res$status_code == 200) {
      
      # - tryCatch rawToChar
      # - NOTE: might fail for very long vectors
      rc <- tryCatch(
        {
          rawToChar(res$content)
        },
        error = function(condition) {
          return(FALSE)
        }
      )
      
      if (rc == FALSE) {
        print("rawToChar() conversion failed. Skipping.")
        next
      }
      
      # - is.ExceptionTimeout
      queryTimeout <- grepl("timeout", rc, ignore.case = TRUE)
      if (queryTimeout) {
        print("Query timeout (!)")
      }
      
      rc <- data.frame(item = unlist(str_extract_all(rc, "Q[[:digit:]]+")), 
                       stringsAsFactors = F)
    } else {
      print(paste0("Server response: ", res$status_code))
      qErrors <- append(qErrors, targetClass)
    }
    
    itemsOut[[k]] <- rc
    
    }
  
  # - store
  if (length(itemsOut) > 0) {
    
    # - itemsOut as data.table:
    itemsOut <- rbindlist(itemsOut)

    # - keep only unique items:
    w <- which(!(duplicated(itemsOut$item)))
    itemsOut <- itemsOut[w]
    
    # store as CSV
    itemsOut$category <- wdcmOntology$Category[i]
    filename <- paste0(wdcmOntology$Category[i],"_ItemIDs.csv")
    # - store
    setwd(itemsDir)
    write_csv(itemsOut, filename)
    
    # clear:
    rm(itemsOut); gc()
  }
  
}

### --- Fix WDCM_Ontology (Phab T174896#3762820)
# - to runtime Log:
print("--- Fix WDCM_Ontology (Phab T174896#3762820)")

# - remove Geographical Object from Organization:
organizationItems <- fread(paste0(itemsDir, 'Organization_ItemIDs.csv'))
geoObjItems <- fread(paste0(itemsDir, 'Geographical Object_ItemIDs.csv'))
w <- which(organizationItems$item %in% geoObjItems$item)
if (length(w) > 0) {
  organizationItems <- organizationItems[-w, ]
}
# - store:
write_csv(organizationItems, 'Organization_ItemIDs.csv')
# - clear:
rm(organizationItems); rm(geoObjItems); gc()
# - remove Book from Work of Art:
bookItems <- fread(paste0(itemsDir, 'Book_ItemIDs.csv'))
workOfArtItems <- fread(paste0(itemsDir, 'Work Of Art_ItemIDs.csv'))
w <- which(workOfArtItems$item %in% bookItems$item)
if (length(w) > 0) {
  workOfArtItems <- workOfArtItems[-w, ]
}
# - store:
write_csv(workOfArtItems, 'Work Of Art_ItemIDs.csv')
# - clear:
rm(workOfArtItems); rm(bookItems); gc()
# - remove Architectural Structure from Geographical Object:
architectureItems <- fread(paste0(itemsDir, 'Architectural Structure_ItemIDs.csv'))
geoObjItems <- fread(paste0(itemsDir, 'Geographical Object_ItemIDs.csv'))
w <- which(geoObjItems$item %in% architectureItems$item)
if (length(w) > 0) {
  geoObjItems <- geoObjItems[-w, ]
}
# - store:
write_csv(geoObjItems, 'Geographical Object_ItemIDs.csv')
# - clear:
rm(geoObjItems); rm(architectureItems); gc()

### --- log Collect_Items:
# - to runtime Log:
print("--- LOG: Collect_Items step completed.")
# - set log dir:
setwd(logDir)
# - log uncompleted queries
write.csv(qErrors, "WDCM_CollectItems_SPARQL_Errors.csv")
# - write to WDCM main reporting file:
lF <- list.files()
if ('WDCM_MainReport.csv' %in% lF) {
  mainReport <- read.csv('WDCM_MainReport.csv',
                         header = T,
                         row.names = 1,
                         check.names = F,
                         stringsAsFactors = F)
  newReport <- data.frame(Step = 'CollectItems',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  mainReport <- rbind(mainReport, newReport)
  write.csv(mainReport, 'WDCM_MainReport.csv')
} else {
  newReport <- data.frame(Step = 'CollectItems',
                          Time = as.character(Sys.time()),
                          stringsAsFactors = F)
  write.csv(newReport, 'WDCM_MainReport.csv')
}

### --- rename files for hdfs
itemFiles <- list.files(itemsDir)
itemFiles <- gsub(" ", "\\ ", itemFiles, fixed = T)
itemNames <- unname(sapply(itemFiles, function(x){
  gsub("\\s|_", "-", x)
}))
for (i in 1:length(itemFiles)) {
  system(command = paste0("mv ", paste0(itemsDir, itemFiles[i]), " ", 
                          paste0(itemsDir, itemNames[i])),
         wait = T)
}

### --- Copy to hdfs
# -  delete hdfsDir_WDCMCollectedItemsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rm -r ',
           hdfsDir_WDCMCollectedItemsDir),
       wait = T)
# -  make hdfsDir_WDCMCollectedItemsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -mkdir ',
           hdfsDir_WDCMCollectedItemsDir),
       wait = T)
# -  copy to hdfsDir_WDCMCollectedItemsDir
print("---- Move to hdfs.")
hdfsC <- system(command = paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
                                 itemsDir, "* ",
                                 hdfsDir_WDCMCollectedItemsDir),
                wait = T)

### --- Collect GEO items
# - to runtime Log:
print(paste("--- WDCM GeoEngine update STARTED ON:", Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Read WDCM_GeoItems
# - to runtime Log:
print("--- Reading Ontology.")
wdcmGeoItems <- read.csv(paste0(ontologyDir, 
                                "WDCM_GeoItems_Belgrade_12152017.csv"),
                         header = T,
                         check.names = F,
                         stringsAsFactors = F)

### --- Select all instances accross all sub-classes of searchItems:
# - set itemsDir:
setwd(itemsGeoDir)

# - clear output dir:
lF <- list.files()
rmF <- file.remove(lF)

# - track uncompleted queries:
qErrors <- character()

# - startTime (WDCM Main Report)
startTime <- as.character(Sys.time())

for (i in 1:length(wdcmGeoItems$item)) {
  
  # - to runtime Log:
  print(paste("--- SPARQL category:", i, ":", wdcmGeoItems$itemLabel[i], sep = " "))
  
  searchItems <- str_trim(wdcmGeoItems$item[i], "both")
  
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
  
  # - init repeat counter
  c <- 0
  
  repeat {
    
    # - run query:
    res <- GET(url = paste0(endPointURL, URLencode(query)))
    
    # - check query:
    if (res$status_code != 200) {
      # - to runtime Log:
      print(paste("Server response not 200 for SPARQL category: ", i, "; repeating.", (c <- c + 1),sep = ""))
      rc <- 'error'
    } else {
      print(paste("Parsing now SPARQL category:", i, sep = ""))
      # - JSON:
      rc <- rawToChar(res$content)
      rc <- tryCatch(
        {
          # - fromJSON:
          fromJSON(rc, simplifyDataFrame = T)
        },
        warning = function(cond) {
          # - return error:
          print(paste("Parsing now SPARQL category:", i, " failed w. warning; repeating.", (c <- c + 1), sep = ""))
          'error'
        }, 
        error = function(cond) {
          # - return error:
          print(paste("Parsing now SPARQL category:", i, " failed w. error; repeating.", (c <- c + 1), sep = ""))
          'error'
        }
      )
    }
    
    # - condition:
    if (res$status_code == 200 & class(rc) == 'list') {
      print("Parsing successful.")
      
      # - clean:
      rm(res); gc()
      
      # - extract:
      item <- rc$results$bindings$item$value
      coordinate <- rc$results$bindings$coordinate$value
      label <- rc$results$bindings$itemLabel$value
      # - as.data.frame:
      items <- data.frame(item = item,
                          coordinate = coordinate,
                          label = label,
                          stringsAsFactors = F)
      # - clear:
      rm(item); rm(coordinate); rm(label); rm(rc); gc()
      # - keep unique result set:
      w <- which(duplicated(items$item))
      if (length(w) > 0) {items <- items[-w, ]}
      # - clear possible NAs from coordinates
      w <- which(is.na(items$coordinate) | (items$coordinate == ""))
      if (length(w) > 0) {items <- items[-w, ]}
      # - fix items
      items$item <- gsub("http://www.wikidata.org/entity/", "", items$item, fixed = T)
      # - fix coordinates (lon, lat)
      items$coordinate <- gsub("Point(", "", items$coordinate, fixed = T)
      items$coordinate <- gsub(")", "", items$coordinate, fixed = T)
      lon <- str_extract(items$coordinate, "^.+\\s")
      lat <- str_extract(items$coordinate, "\\s.+$")
      items$coordinate <- NULL
      items$lon <- lon
      items$lat <- lat
      # clear:
      rm(lon); rm(lat); gc()
      
      # store as CSV
      write_csv(items, path = paste0(wdcmGeoItems$itemLabel[i],"_ItemIDs.csv"))
      
      # exit:
      break
    }
    
    print("Pause for 10 secs.")
    Sys.sleep(10)
    
  }
  
}

### --- rename geo files for hdfs
itemFiles <- list.files(itemsGeoDir)
itemFiles <- gsub(" ", "\\ ", itemFiles, fixed = T)
itemNames <- unname(sapply(itemFiles, function(x){
  gsub("\\s|_", "-", x)
}))
for (i in 1:length(itemFiles)) {
  system(command = paste0("mv ", paste0(itemsGeoDir, itemFiles[i]), " ", 
                          paste0(itemsGeoDir, itemNames[i])),
         wait = T)
}

### --- Copy to hdfs
# -  delete hdfsCollectedGeoItemsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rm -r ',
           hdfsCollectedGeoItemsDir),
       wait = T)
# -  make hdfsCollectedGeoItemsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -mkdir ',
           hdfsCollectedGeoItemsDir),
       wait = T)
# -  copy to hdfsCollectedGeoItemsDir
print("---- Move to hdfs.")
hdfsC <- system(command = paste0('sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
                                 itemsGeoDir, "* ",
                                 hdfsCollectedGeoItemsDir),
                wait = T)

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- wdcmModule_CollectItems.R UPDATE DONE IN: ", 
             generalT2 - generalT1, "."))
