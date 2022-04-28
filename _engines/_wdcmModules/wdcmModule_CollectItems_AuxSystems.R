#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdcmModule_CollectItems_AuxSystems.R
### --- Version 1.0.0
### --- June 2020.
### --- Author: Goran S. Milovanovic, Data Analyst, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- February 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Contact WDQS and fetch all item IDs from the WDCM WD Ontology
### --- NOTE: the execution of this WDCM script is always dependent upon the
### --- previous WDCM_Sqoop_Clients.R run.
### --- This is the shortened version of the WDCM wdcmModule_CollectItems.R
### --- module used to enable WDCM (S)itelinks and WDCM (T)itles updates
### --- on various stat100* machines where it is not necessary to work w. hdfs.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- Script 1.1: wdcmModule_CollectItems_AuxSystems.R
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- wdcmModule_CollectItems_AuxSystems.R takes a list of items (categories)
### --- defined by a given WDCM WD Ontology (human input) and then
### --- contacts the Wikidata Query Service to fetch all relevant item IDs.
### ---------------------------------------------------------------------------
### --- INPUT:
### --- the wdcmModule_CollectItems_AuxSystems.R reads the WDCM Ontology file (csv)
### --- ACTIVE WDCM TAXONOMY: WDCM_Ontology_Berlin_05032017.csv
### ---------------------------------------------------------------------------

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
    # - write complete structure for category
    filename <- paste0(wdcmOntology$Category[i], "_Structure.csv")
    setwd(structureDir)
    write.csv(itemsOut, filename)
    
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

