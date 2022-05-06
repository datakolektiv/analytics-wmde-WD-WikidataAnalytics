### ---------------------------------------------------------------------------
### --- WDCM Geo
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
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
  
  ### --- functions
  get_WDCM_table <- function(url_dir, filename, row_names) {
    read.csv(URLencode(paste0(url_dir, filename)), 
             header = T, 
             stringsAsFactors = F,
             check.names = F)
  }
  
  ### --- Fetch data files
  publicDir <- 'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/geo/'
  url <- 'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/geo/'
  
  # - download ML and ETL files:
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    
    # - list files:
    incProgress(1/5, detail = "Access data repository.")
    
    ### --- List files
    page <- as.character(httr::GET(url))
    
    incProgress(2/5, detail = "Fetch timestamp.")
    
    ### --- Fetch update info
    timestamp <- stringr::str_extract(page, 
                             "[[:digit:]]+-[[:digit:]]+-[[:digit:]]+")
    
    incProgress(3/5, detail = "List data files.")
    
    ### --- Fetch datasets
    links <- stringr::str_extract_all(page, "<a href=.+>.+</a>")
    links <- sapply(links, function(x) {stringr::str_extract_all(x, ">.+<")})
    links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
    links <- links[3:length(links)]
    categories <- vector(mode = "list", length = length(links))
    
    incProgress(4/5, detail = "Download data files.")
    
    for (i in 1:length(links)) {
      categories[[i]] <- get_WDCM_table(publicDir, links[i])
    }
    
    incProgress(5/5, detail = "Finalize.")
    
    names(categories) <- stringr::str_to_title(sapply(links, function(x) {
      strsplit(strsplit(x, split = ".", fixed = T)[[1]][1],
               split = "_",
               fixed = T)[[1]][3]
    }))
    
  })
  
  ### --- output: updateInfo
  output$updateInfo <- renderText({
    return(paste("<p align=right>Last update: <i>", timestamp, "</i></p>", sep = ""))
  })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Maps
  ### ------------------------------------------
  
  ### --- SELECT: update select 'selectCategory'
  updateSelectizeInput(session,
                       'selectCategory',
                       "Select Semantic Category:",
                       choices = names(categories),
                       selected = names(categories)[round(runif(1, 1, length(categories)))],
                       server = TRUE)
  
  ### --- LEAFLET MAP:
  points <- eventReactive(input$selectCategory, {
    if (is.null(input$selectCategory) | (input$selectCategory == "")) {
      return(NULL)
    } else {
      outCat <- categories[[which(names(categories) %in% input$selectCategory)]]
      outCat$lon <- as.numeric(outCat$lon)
      outCat$lat <- as.numeric(outCat$lat)
      return(outCat)
    }
  }, ignoreNULL = FALSE)
  
  output$wdcmMap <- leaflet::renderLeaflet({
    if (is.null(points())) {
      return(NULL) 
    } else {
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(data = points(),
                            popup = (paste('<b>', points()$label, '</b><br>',
                                           '<a href="https://www.wikidata.org/wiki/',
                                           points()$eu_entity_id,
                                           '" target = "_blank">',
                                           points()$eu_entity_id, '</a><br>',
                                           'Usage: ', points()$eu_count, sep = "")
                   ),
                   clusterOptions = leaflet::markerClusterOptions()
        )
    }
  }) %>% withProgress(message = 'Generating map',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Data
  ### ------------------------------------------
  
  ### --- output$mapData
  output$mapData <- DT::renderDataTable({
    DT::datatable(points(),
              options = list(
                pageLength = 20,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- download map data
  # - Download: tabulations_projectsChart
  output$mapDataCSV <- downloadHandler(
    filename = function() {
      'WDCM_Data.csv'},
    content = function(file) {
      write.csv(points(),
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
}
