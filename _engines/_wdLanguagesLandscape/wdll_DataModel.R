#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdll_DataModel.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- August 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate WD Languages Landscape modules:
### --- 4. wdll_DataModel.R
### --- SPARQL Data Model in WDLL
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
### --- Script 4: wdll_DataModel.R
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- wdll_DataModel.R RUN STARTED ON:", 
            Sys.time(), sep = " "))

### --- Setup

# - to runtime Log:
print(paste("--- wdll_DataModel.R: read params.", 
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

# - libs
library(XML)
library(httr)
library(jsonlite)
library(WikidataR)
library(data.table)

# - params
params <- XML::xmlParse(paste0(fPath, "WD_LanguagesLandscape_Config.xml"))
params <- XML::xmlToList(params)

# - dirs
dataDir <- params$general$dataDir
logDir <- params$general$logDir
outDir <- params$general$outDir
publicDir <- params$general$pubDataDir
hdfsPath <- params$general$hdfsPath

# - Set proxy
# - to runtime Log:
print(paste("--- wdll_DataModel.R: set proxy.", 
            Sys.time(), sep = " "))
Sys.setenv(
  http_proxy = params$general$http_proxy,
  https_proxy = params$general$http_proxy)

# - WDQS endpoint
endPointURL <- params$general$wdqs_endpoint

# - publicDir
publicDir <- params$general$pubDataDir

### --- used languages

# - to runtime Log:
print(paste("--- wdll_DataModel.R: read wd_languages_count.csv.", 
            Sys.time(), sep = " "))
usedLanguages <- read.csv(paste0(outDir, "wd_languages_count.csv"),
                          header = T, 
                          check.names = F,
                          row.names = 1,
                          stringsAsFactors = F)

### --- define data model for languages: essential properties

dmodelProperties <- c('P31', 'P279', 'P361', 'P17', 
                      'P4132', 'P2989', 'P3161', 'P5109', 
                      'P5110', 'P282', 'P1018', 'P1098', 
                      'P1999', 'P3823', 'P424', 'P2341', 
                      'P527', 'P218', 'P219', 'P220', 
                      'P1627', 'P3916', 'P1394', 'P2581')
dmodelPropertiesLabs <- c('instanceOf', 
                          'subclassOf', 
                          'partOf', 
                          'country', 
                          'linguisticTypology', 
                          'hasGrammaticalCase', 
                          'hasGrammaticalMood', 
                          'hasGrammaticalGender', 
                          'hasGrammaticalPerson', 
                          'writingSystem', 
                          'languageRegulatoryBody', 
                          'numberOfSpeakers', 
                          'UNESCOLanguageStatus', 
                          'EthnologueLanguageStatus', 
                          'WikimediaLanguageCode', 
                          'indigenousTo', 
                          'hasPart', 
                          'ISO639_1code', 
                          'ISO639_2code', 
                          'ISO639_3code', 
                          'EthnologueLanguageCode', 
                          'UNESCOThesaurusID', 
                          'GlottologCode', 
                          'BabelNetID')
dmodelProps <- data.frame(dmodelProperties = dmodelProperties, 
                          propertyLabel = dmodelPropertiesLabs, 
                          stringsAsFactors = F)

### --- dataModel basics: languages + labels + WikimediaLanguage Code 

# - to runtime Log:
print(paste("--- wdll_DataModel.R: WDQS dataModel basics: languages + labels + WikimediaLanguage Code.", 
            Sys.time(), sep = " "))
# - Construct Query: languages from Q1288568 Modern Languages class
query <- 'SELECT ?language ?languageLabel ?WikimediaLanguageCode ?desc
            WHERE {
              ?language wdt:P31/wdt:P279* wd:Q17376908 .
              OPTIONAL {?language wdt:P424 ?WikimediaLanguageCode} .
              OPTIONAL {?language rdfs:label ?desc filter (lang(?desc) = "en")} .
              SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
            }'

# - Run Query:
# - to runtime Log:
print(paste("--- wdll_DataModel.R: Run SPARQL Query.", 
            Sys.time(), sep = " "))
repeat {
  res <- tryCatch({
    httr::GET(url = paste0(endPointURL, URLencode(query)))
  },
  error = function(condition) {
    print("Something's wrong on WDQS: wait 10 secs, try again.")
    Sys.sleep(10)
    httr::GET(url = paste0(endPointURL, URLencode(query)))
  },
  warning = function(condition) {
    print("Something's wrong on WDQS: wait 10 secs, try again.")
    Sys.sleep(10)
    httr::GET(url = paste0(endPointURL, URLencode(query)))
  }
  )  
  if (res$status_code == 200) {
    print("Success.")
    break
  } else {
    print("Failed; retry.")
    Sys.sleep(10)
  }
}
# - Extract result:
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
} else {
  # - report
  print("The response status code for the SPARQL query was not 200.")
  print("Exiting.")
}
# - Parse JSON:
if (class(rc) == "logical") {
  print("rawToChar() conversion for the SPARQL query failed. Exiting.")
} else  {
  rc <- jsonlite::fromJSON(rc, simplifyDataFrame = T)
}
# - to runtime Log:
print(paste("--- wdll_DataModel.R: form dataModel.", 
            Sys.time(), sep = " "))
dataModel <- data.frame(language = rc[[2]]$bindings$language$value,
                        languageLabel = rc[[2]]$bindings$languageLabel$value,
                        wikimediaCode = rc[[2]]$bindings$WikimediaLanguageCode$value,
                        description = rc[[2]]$bindings$desc$value,
                        stringsAsFactors = F)
dataModel$language <- gsub("http://www.wikidata.org/entity/", "", dataModel$language)
dataModel <- dataModel[!duplicated(dataModel), ]

# - enter dataModel$wikimediaCode to usedLanguages
usedLanguages <- dplyr::left_join(usedLanguages, 
                                  dplyr::select(dataModel, 
                                                language, wikimediaCode, description),
                                  by = c("language" = "wikimediaCode"))
colnames(usedLanguages)[5] <- 'languageURI'

### --- Find duplicated languages (i.e. more than one Wikimedia language code)
# - compare the Wikimedia language codes in dataModel with the unique codes found in
# - languageUsage data.frame:

# - to runtime Log:
print(paste("--- wdll_DataModel.R: find and eliminate duplicated languages.", 
            Sys.time(), sep = " "))
dataModel$checkWMcode <- sapply(dataModel$wikimediaCode, function(x) {
  if(!is.na(x)) {
    if (x %in% unique(usedLanguages$language)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(NA)
  }
})
# - now mark duplicated languages
dataModel$duplicated <- sapply(dataModel$language, function(x) {
  if (sum(dataModel$language %in% x) > 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

# - remove language duplicates: where their Wikimedia language code is
# - not used:
dataModel <- dplyr::filter(dataModel,
                           !((duplicated == T) & (checkWMcode == F)))
dataModel$checkWMcode <- NULL
dataModel$duplicated <- NULL
# - check that all dataModel$language are items:
wnI <- which(!grepl("^Q", dataModel$language))
if (length(wnI) > 0) {
  dataModel <- dataModel[-wnI, ]
}

### --- dataModel labels: ID + label

dataModel$languageLabel <- 
  paste0(dataModel$languageLabel, " (", dataModel$language, ")")

### --- Collect Language Properties w. {WikidataR}

# - to runtime Log:
print(paste("--- wdll_DataModel.R: Collect Language Properties w. {WikidataR}.", 
            Sys.time(), sep = " "))
lprops <- vector(mode = "list", length = length(dataModel$language))
startTime <- Sys.time()
for (i in 1:length(lprops)) {
  # - report
  print(paste0("Assessing: ", i, ". - ", dataModel$languageLabel[i]))
  if (i %% 100 == 0) {
    print("---------------------------------------------------------------------")
    print(paste0(
      "Processing ", i, ". out of ", length(lprops), " in: ",
      round(difftime(Sys.time(), startTime, units = "min"), 2),
      " minutes. That would be: ", round(i/length(lprops)*100, 2), "%."))
    print("---------------------------------------------------------------------")
  }
  x <- dataModel$language[i]
  gprops <- F
  count <- 0
  repeat {
    count <- count + 1
    gprops <- tryCatch({
      WikidataR::get_item(x)
    },
    error = function(condition) {
      FALSE
    },
    warning = function(condition){
      FALSE
    })
    if (class(gprops) != "logical") {
      break
    } else if (count > 5) {
      print(paste0("--- Failed: ", x, ": ", dataModel$languageLabel))
      break
    } else {
      print("--- Repeat query!")
      Sys.sleep(1)
    }
  }
  if (class(gprops) != "logical") {
    # - number of sitelinks
    numSitelinks <- length(gprops[[1]]$sitelinks)
    # - parse claims
    gprops <- gprops[[1]]$claims
    # - extract properties
    gprops <- gprops[which(names(gprops) %in% dmodelProps$dmodelProperties)]
    if (length(gprops) > 0) {
      gprops <- lapply(gprops, function(x) {x$mainsnak})
      gprops <- lapply(gprops, function(x) {jsonlite::flatten(x, recursive = TRUE)})
      gprops <- rbindlist(gprops, fill = T, use.names = T)
      gprops$language <- x
      gprops$languageLabel <- dataModel$languageLabel[which(dataModel$language %in% x)][1]
      if ('property' %in% colnames(gprops)) {
        gprops <- dplyr::left_join(gprops, 
                                   dmodelProps,
                                   by = c("property" = "dmodelProperties"))
        gprops$numSitelinks <- numSitelinks
        # - output data
        lprops[[i]] <- gprops
        print(paste0("Processed: ", i, ". - ", dataModel$languageLabel[i]))
      } else {
        # - output NA
        lprops[[i]] <- NA
        print(paste0("Skipped: ", dataModel$languageLabel[i]))
      }
    } else {
      print("--- Nothing to process here, skipping.")
      next
    }
  }
}

# - complete lprops
w <- which(is.na(lprops))
if (length(w) > 0) {
  lprops[w] <- NULL 
}
lprops <- data.table::rbindlist(lprops, fill = T, use.names = T)
# - filter out P1098 (number of speakers)
w <- which(lprops$propertyLabel %in% 'numberOfSpeakers')
if (length(w) > 0) {
  lprops <- lprops[-w, ] 
}
# - write
write.csv(lprops, 
          paste0(outDir, "dataModel_Long.csv"))

# - select only essential fields
lprops <- dplyr::select(lprops, 
                        language, languageLabel, 
                        numSitelinks, snaktype, 
                        property, propertyLabel, 
                        datavalue.value, datavalue.type, 
                        `datavalue.value.entity-type`, 
                        datavalue.value.id)

# - wrangle dataModel
lprops$value <- ifelse(lprops$`datavalue.type` == "string", 
                       lprops$`datavalue.value`, 
                       lprops$`datavalue.value.id`)
lprops <- dplyr::select(lprops, 
                        language, languageLabel, 
                        numSitelinks, property, 
                        propertyLabel, value)

# - store dataModel
write.csv(lprops, 
          paste0(outDir, "dataModel_Long.csv"))

# - English labels for item values
# - to runtime Log:
print(paste("--- wdll_DataModel.R: English labels for item values.", 
            Sys.time(), sep = " "))
items <- unique(lprops$value[grepl("^Q[[:digit:]]+", lprops$value)])
# - wdqs: fetch English labels
# - fetch occupation labels w. {WikidataR}
labels <- sapply(items,
                 function(x) {
                   repeat {
                     i <- tryCatch({
                       WikidataR::get_item(x)
                     },
                     error = function(condition) {
                       Sys.sleep(2)
                       FALSE
                     })
                     if (class(i) == "wikidata") {
                       break
                     }
                   }
                   i[[1]]$labels$en$value
                 })
labNames <- names(labels) 
labels <- as.character(labels)

# - add labels for item values to lprops
lprops$value <- sapply(lprops$value, function(x) {
  if (grepl("^Q[[:digit:]]+", x)) {
    w <- which(labNames == x)
    return(paste0(labels[w], " (", labNames[w], ")"))
  } else {
    return(x)
  }
})

# - add Wikimedia Language codes to lprops
lprops <- dplyr::left_join(lprops, 
                           dplyr::select(dataModel, languageLabel, wikimediaCode), 
                           by = 'languageLabel')

# - re-structure dataModel
# - to runtime Log:
print(paste("--- wdll_DataModel.R: re-structure dataModel.", 
            Sys.time(), sep = " "))
dataModel_basic <- dplyr::select(lprops, 
                                 language, 
                                 languageLabel, 
                                 wikimediaCode,
                                 numSitelinks)
dataModel_basic <- dataModel_basic[!duplicated(dataModel_basic), ]
dataModel_properties <- dplyr::select(lprops, 
                                      language, 
                                      languageLabel, 
                                      wikimediaCode,
                                      property, propertyLabel, value)
dataModel_properties$property <- paste0(
  dataModel_properties$propertyLabel, 
  " (", 
  dataModel_properties$property, 
  ")"
)
dataModel_properties$propertyLabel <- NULL

write.csv(dataModel_basic, 
          paste0(outDir, 'dataModel_basic.csv'))
write.csv(dataModel_properties, 
          paste0(outDir, 'dataModel_properties.csv'))
write.csv(usedLanguages, 
          paste0(outDir, 'WD_Languages_UsedLanguages.csv'))

### --- add to usedLanguages:  

# - GlottologCode (P1394), EthnologueLanguageStatus (P3823)
# - EthnologueLanguageCode (P1627), ISO639_3code (P220), ISO639_2code (P219), 
# - UNESCOThesaurusID (P3916), UNESCOLanguageStatus (P1999)
# - add: EthnologueLanguageCode (P1627)
# - to runtime Log:
print(paste("--- wdll_DataModel.R: EthnologueLanguageCode.", 
            Sys.time(), sep = " "))
EthnologueLanguageCode <- dplyr::filter(dataModel_properties,
                                        property == 'EthnologueLanguageCode (P1627)') %>%
  dplyr::select(wikimediaCode, property, value) %>% 
  dplyr::filter(!is.na(wikimediaCode) & !is.na(value))
usedLanguages <- dplyr::left_join(usedLanguages, 
                                  dplyr::select(EthnologueLanguageCode, 
                                                wikimediaCode, value),
                                  by = c("language" = "wikimediaCode"))
colnames(usedLanguages)[7] <- 'EthnologueLanguageCode'

# - add: EthnologueLanguageStatus (P3823)
# - to runtime Log:
print(paste("--- wdll_DataModel.R: EthnologueLanguageStatus.", 
            Sys.time(), sep = " "))
EthnologueLanguageStatus <- dplyr::filter(dataModel_properties,
                                          property == 'EthnologueLanguageStatus (P3823)') %>%
  dplyr::select(wikimediaCode, property, value) %>% 
  dplyr::filter(!is.na(wikimediaCode) & !is.na(value))
usedLanguages <- dplyr::left_join(usedLanguages, 
                                  dplyr::select(EthnologueLanguageStatus, 
                                                wikimediaCode, value),
                                  by = c("language" = "wikimediaCode"))
colnames(usedLanguages)[8] <- 'EthnologueLanguageStatus'

# - add: UNESCOLanguageStatus (P1999)
# - to runtime Log:
print(paste("--- wdll_DataModel.R: UNESCOLanguageStatus.", 
            Sys.time(), sep = " "))
UNESCOLanguageStatus <- dplyr::filter(dataModel_properties,
                                      property == 'UNESCOLanguageStatus (P1999)') %>%
  dplyr::select(wikimediaCode, property, value) %>% 
  dplyr::filter(!is.na(wikimediaCode) & !is.na(value))
usedLanguages <- dplyr::left_join(usedLanguages, 
                                  dplyr::select(UNESCOLanguageStatus, 
                                                wikimediaCode, value),
                                  by = c("language" = "wikimediaCode"))
colnames(usedLanguages)[9] <- 'UNESCOLanguageStatus'

# - add: numSitelinks from dataModel_basic:
# - to runtime Log:
print(paste("--- wdll_DataModel.R: add: numSitelinks from dataModel_basic.", 
            Sys.time(), sep = " "))
usedLanguages <- dplyr::left_join(usedLanguages, 
                                  dplyr::select(dataModel_basic, 
                                                language, numSitelinks),
                                  by = c("languageURI" = "language"))
# - to runtime Log:
print(paste("--- wdll_DataModel.R: store: WD_Languages_UsedLanguages.csv.", 
            Sys.time(), sep = " "))
write.csv(usedLanguages, 
          paste0(outDir, 'WD_Languages_UsedLanguages.csv'))

# - to runtime Log:
print(paste("--- wdll_DataModel.R ENDED ON:", 
            Sys.time(), sep = " "))
