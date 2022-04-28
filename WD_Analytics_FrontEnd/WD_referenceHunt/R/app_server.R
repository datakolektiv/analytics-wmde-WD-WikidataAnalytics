### ---------------------------------------------------------------------------
### --- WD Game: Reference Treasure Hunt
### --- Version 1.0.0
### --- 2020.
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
  
  ### --- Wikidata Logo
  output$logo <- renderUI({
    tags$img(src =
               'https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Wikidata-logo-en.svg/200px-Wikidata-logo-en.svg.png')
  })
  
  ### --- GET DATA
  
  # - get current data files + pre-process:
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    ### --- Download matches
    incProgress(1/5, detail = "Accepted matches.")
    acceptedFrame <- read.csv('https://wd-ref-island.toolforge.org/stats.php?dump=ACCEPTED', 
                              check.names = F, 
                              stringsAsFactors = F)
    incProgress(2/5, detail = "Rejected matches.")
    rejectedFrame <- read.csv('https://wd-ref-island.toolforge.org/stats.php?dump=REJECTED', 
                              check.names = F, 
                              stringsAsFactors = F)
    ### --- Process matches
    incProgress(3/5, detail = "Process matches.")
    # - remove duplicates
    acceptedFrame <- acceptedFrame[!duplicated(acceptedFrame), ]
    rejectedFrame <- rejectedFrame[!duplicated(rejectedFrame), ]
    # - determine datatype
    acceptedFrame$datatype <- sapply(acceptedFrame$`Value (JSON)`, function(x) {
      as.character(names(jsonlite::fromJSON(x))[1])
    })
    acceptedFrame$datatype <- as.character(acceptedFrame$datatype)
    acceptedFrame$datatype[acceptedFrame$datatype == "character(0)"] <- NA
    acceptedFrame <- acceptedFrame[complete.cases(acceptedFrame), ]
    rejectedFrame$datatype <- sapply(rejectedFrame$`Value (JSON)`, function(x) {
      as.character(names(jsonlite::fromJSON(x))[1])
    })
    rejectedFrame$datatype <- as.character(rejectedFrame$datatype)
    rejectedFrame$datatype[rejectedFrame$datatype == "character(0)"] <- NA
    rejectedFrame <- rejectedFrame[complete.cases(rejectedFrame), ]
    ### --- WDQS for External Identifiers
    incProgress(4/5, detail = "Contact WDQS for External Identifiers.")
    
  })
  
  ### --- ANALYTICS
  
  # - per external ID
  
  # - per datatype
  acceptedRate_datatype <- acceptedFrame %>% 
    dplyr::select(datatype) %>% 
    dplyr::group_by(datatype) %>% 
    dplyr::summarise(count = dplyr::n())
  rejectedRate_datatype <- rejectedFrame %>% 
    dplyr::select(datatype) %>% 
    dplyr::group_by(datatype) %>% 
    dplyr::summarise(count = dplyr::n())
  datatype_ratio <- dplyr::left_join(acceptedRate_datatype,
                                     rejectedRate_datatype,
                                     by = "datatype")
  colnames(datatype_ratio)[2:3] <- c("accepted", "rejected")
  datatype_ratio$accepted[is.na(datatype_ratio$accepted)] <- 0
  datatype_ratio$rejected[is.na(datatype_ratio$rejected)] <- 0
  datatype_ratio$ratio <- datatype_ratio$accepted/datatype_ratio$rejected
  datatype_ratio$percent_accepted <- datatype_ratio$accepted/(datatype_ratio$accepted + datatype_ratio$rejected)*100
  datatype_ratio$total_decisions <- datatype_ratio$accepted + datatype_ratio$rejected
  datatype_ratio$ratio <- round(datatype_ratio$ratio, 2)
  datatype_ratio$percent_accepted <- round(datatype_ratio$percent_accepted, 2)
  datatype_ratio <- dplyr::arrange(datatype_ratio, desc(total_decisions))
  
  # - per property
  acceptedRate_property <- acceptedFrame %>% 
    dplyr::select(Property) %>% 
    dplyr::group_by(Property) %>% 
    dplyr::summarise(count = dplyr::n())
  rejectedRate_property <- rejectedFrame %>% 
    dplyr::select(Property) %>% 
    dplyr::group_by(Property) %>% 
    dplyr::summarise(count = dplyr::n())
  property_ratio <- dplyr::left_join(acceptedRate_property,
                                     rejectedRate_property,
                                     by = "Property")
  colnames(property_ratio)[2:3] <- c("accepted", "rejected")
  property_ratio$accepted[is.na(property_ratio$accepted)] <- 0
  property_ratio$rejected[is.na(property_ratio$rejected)] <- 0
  property_ratio$ratio <- property_ratio$accepted/property_ratio$rejected
  property_ratio$percent_accepted <- 
    property_ratio$accepted/(property_ratio$accepted + property_ratio$rejected)*100
  property_ratio$total_decisions <- property_ratio$accepted + property_ratio$rejected
  property_ratio$ratio <- round(property_ratio$ratio, 2)
  property_ratio$percent_accepted <- round(property_ratio$percent_accepted, 2)
  property_ratio <- dplyr::arrange(property_ratio, desc(total_decisions))
  
  # - per Property * Item * Extracted Data * Source
  aFrame <- acceptedFrame %>% 
    dplyr::select(Item, Property, `Extracted Data (JSON)`, `Source URL`)
  aFrame$Choice <- 'Accepted'
  rFrame <- rejectedFrame %>% 
    dplyr::select(Item, Property, `Extracted Data (JSON)`, `Source URL`)
  rFrame$Choice <- 'Rejected'
  full_ratio <- rbind(aFrame, rFrame) %>% 
    dplyr::arrange(Choice)
  
  ### --- TABLES
  
  ### --- output$datatype_ratio
  output$datatype_ratio <- DT::renderDataTable({
    DT::datatable(datatype_ratio, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  # - Download: datatype_ratio
  output$datatype_ratio_download <- downloadHandler(
    filename = function() {
      'datatype_ratio.csv'},
    content = function(file) {
      write.csv(datatype_ratio,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### --- output$full_ratio
  output$full_ratio <- DT::renderDataTable({
    DT::datatable(full_ratio, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  # - Download: full_ratio
  output$full_ratio_download <- downloadHandler(
    filename = function() {
      'Item-Property_Value.csv'},
    content = function(file) {
      write.csv(full_ratio,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### --- output$property_ratio
  output$property_ratio <- DT::renderDataTable({
    DT::datatable(property_ratio, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  # - Download: property_ratio
  output$property_ratio_download <- downloadHandler(
    filename = function() {
      'property_ratio.csv'},
    content = function(file) {
      write.csv(property_ratio,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
}
