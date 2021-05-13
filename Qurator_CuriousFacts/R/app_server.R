### ---------------------------------------------------------------------------
### --- Qurator: Curious Facts
### --- Version 1.0.0
### --- 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Analytics (WA)
### --- https://wikidata-analytics.wmflabs.org/
### ---
### --- WA is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WA is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WA If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # - endPointURL
  endPointURL <- 
    'https://query.wikidata.org/bigdata/namespace/wdq/sparql?query='
  
  ### --- Data
  # - get current Qurator Curious Facts:
  withProgress(message = 'The Dashboard is loading data.', 
               detail = "Please be patient.", value = 0, {
                 
                 dataM1 <- data.table::fread(system.file("_data", "dataM1.csv", package = "CuriousFacts"),
                                             header = T, quote =)
                 infoM1 <- data.table::fread(system.file("_data", "infoM1.csv", package = "CuriousFacts"), header = T)
                 infoM1$V1 <- NULL

                 incProgress(amount = .25, message = "M1 loaded.")
                 dataM2 <- data.table::fread(system.file("_data", "dataM2.csv", package = "CuriousFacts"),
                                             header = T)
                 infoM2 <- data.table::fread(system.file("_data", "infoM2.csv", package = "CuriousFacts"), header = T)
                 infoM2$V1 <- NULL
                 # - de-duplicate dataM2
                 dataM2 <- dataM2[!duplicated(dataM2[, c('item', 'property')]), ]

                 incProgress(amount = .25, message = "M2 loaded.")
                 dataM3 <- data.table::fread(system.file("_data", "dataM3.csv", package = "CuriousFacts"),
                                             header = T)
                 infoM3 <- data.table::fread(system.file("_data", "infoM3.csv", package = "CuriousFacts"), header = T)
                 infoM3$V1 <- NULL
                 
                 # - lists
                 incProgress(amount = .25, message = "M3 loaded.")
                 dataSet <- list(dataM1, dataM2, dataM3)
                 infoSet <- list(infoM1, infoM2, infoM3)
                 # - clean
                 rm(list = c('dataM1', 'dataM2', 'dataM3'))
                 
                 incProgress(amount = .25, message = "DONE.")
                 
               })
  
  ### --- First Fact
  # - select dataset
  cD <- sample(1:length(dataSet), 1)
  dS <- dplyr::select(dataSet[[cD]], 
                      explanation, 
                      establishedOn)
  iS <- infoSet[[cD]]
  # - the latest snapshot only:
  iS <- iS[1, ]
  
  # - select random fact
  rf <- sample(1:dim(dS)[1], 1)
  fact <- dS$explanation[rf]
  fact <- gsub('""', '"', fact)
  establishedOn <- dS$establishedOn[rf]
  
  ### --- First Fact
  output$fact <- renderText({
    
    # - format output
    factFindQ <- stringr::str_extract_all(fact, "Q[[:digit:]]+")[[1]]
    for (i in 1:length(factFindQ)) {
      fact <- gsub(paste0('(', factFindQ[i], ')'),
                   paste0('<a href="https://www.wikidata.org/wiki/\\1" target="_blank">',
                          factFindQ[i], 
                          "</a>"),
                          fact)
    }
    factFindP <- stringr::str_extract_all(fact, "P[[:digit:]]+")[[1]]
    for (i in 1:length(factFindP)) {
      fact <- gsub(paste0('(', factFindP[i], ')'),
                   paste0('<a href="https://www.wikidata.org/wiki/Property:\\1" target="_blank">',
                          factFindP[i], 
                          "</a>"),
                   fact)
    }

    out <- paste0('<p style="font-size:120%;"align="left">',
                  fact, '</p><br>', 
                  '<p style="font-size:120%;"align="left"> This fact was established on: <b>', 
                  establishedOn, '</b>, and is based on the <b>', 
                  iS$wdDumpSnapshot, '</b> snapshot in hdfs of the Wikidata JSON dump. 
                  Edits made after that date are not taken into account.')
    return(out)
    
  })
  
  ### --- First Image
  output$factImage <- renderText({
    item <- dataSet[[cD]]$item[rf]
    query <- paste0('SELECT ?image {wd:', 
                    item, 
                    ' wdt:P18 ?image .}')
    res <- httr::GET(url = paste0(endPointURL, URLencode(query)))
    if (res$status_code == 200) {
      src <- jsonlite::fromJSON(rawToChar(res$content), simplifyDataFrame = T)
      src <- src$results$bindings$image$value
      
      srcimg <- tryCatch({tail(strsplit(src, split = "/")[[1]], 1)}, 
                         error = function(condition) {
                           NULL  
                         })
      if(is.null(srcimg)) {
        return(
          NULL
        )
      }
      srcimg <- paste0("https://magnus-toolserver.toolforge.org/commonsapi.php?image=", 
                       srcimg, 
                       "&thumbwidth=300")
      srcimg <- httr::GET(srcimg)
      srcimg <- XML::xmlToList(XML::xmlParse(srcimg))
      
      if (length(!is.null(srcimg$file$urls$thumbnail)) > 0) {
        return(paste0('<img src="', URLencode(srcimg$file$urls$thumbnail),'">'))   
      } else {
        return(NULL)
      }
    } else {
      return(
        '<p style="font-size:150%;"align="left"></p>'
      )
    }
    
  })
  
  ### --- Present Facts Loop
  observeEvent(input$button, {
    
    # - select dataset
    cD <- sample(1:length(dataSet), 1)
    dS <- dplyr::select(dataSet[[cD]], 
                        explanation, 
                        establishedOn)
    iS <- infoSet[[cD]]
    # - the latest snapshot only:
    iS <- iS[1, ]
    
    # - select random fact
    rf <- sample(1:dim(dS)[1], 1)
    fact <- dS$explanation[rf]
    fact <- gsub('""', '"', fact)
    establishedOn <- dS$establishedOn[rf]
    
    output$fact <- renderText({
      
      # - format output
      factFindQ <- stringr::str_extract_all(fact, "Q[[:digit:]]+")[[1]]
      for (i in 1:length(factFindQ)) {
        fact <- gsub(paste0('(', factFindQ[i], ')'),
                     paste0('<a href="https://www.wikidata.org/wiki/\\1" target="_blank">',
                            factFindQ[i], 
                            "</a>"),
                     fact)
      }
      factFindP <- stringr::str_extract_all(fact, "P[[:digit:]]+")[[1]]
      for (i in 1:length(factFindP)) {
        fact <- gsub(paste0('(', factFindP[i], ')'),
                     paste0('<a href="https://www.wikidata.org/wiki/Property:\\1" target="_blank">',
                            factFindP[i], 
                            "</a>"),
                     fact)
      }
      out <- paste0('<p style="font-size:120%;"align="left">',
                    fact, '</p><hr>', 
                    '<p style="font-size:120%;"align="left"> This fact was established on: <b>', 
                    establishedOn, "</b>, and is based on the <b>", 
                    iS$wdDumpSnapshot, "</b> snapshot in hdfs of the Wikidata JSON dump. 
                    Edits made after that date are not taken into account.")
      return(out)
      
    })
    
    output$factImage <- renderText({
      item <- dataSet[[cD]]$item[rf]
      query <- paste0('SELECT ?image {wd:', 
                      item, 
                      ' wdt:P18 ?image .}')
      res <- httr::GET(url = paste0(endPointURL, URLencode(query)))
      if (res$status_code == 200) {
        src <- jsonlite::fromJSON(rawToChar(res$content), simplifyDataFrame = T)
        src <- src$results$bindings$image$value
        
        srcimg <- tryCatch({tail(strsplit(src, split = "/")[[1]], 1)}, 
                           error = function(condition) {
                             NULL  
                           })
        if(is.null(srcimg)) {
          return(NULL)
        }
        srcimg <- paste0("https://magnus-toolserver.toolforge.org/commonsapi.php?image=", 
                         srcimg, 
                         "&thumbwidth=300")
        srcimg <- httr::GET(srcimg)
        srcimg <- XML::xmlToList(XML::xmlParse(srcimg))
        
        if (length(!is.null(srcimg$file$urls$thumbnail)) > 0) {
          return(paste0('<img src="',URLencode(srcimg$file$urls$thumbnail),'">'))   
        } else {
          return(NULL)
        }
      } else {
        return(
          '<p style="font-size:150%;"align="left"></p>'
        )
      }
      
    })
    
  })
  
}
