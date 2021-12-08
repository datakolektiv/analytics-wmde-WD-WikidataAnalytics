### ---------------------------------------------------------------------------
### --- WD Pageviews per Namespace
### --- Version 1.0.0
### --- 2019.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  ### --- Wikidata Logo
  output$logo <- renderUI({
    tags$img(src =
               'https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Wikidata-logo-en.svg/200px-Wikidata-logo-en.svg.png')
    })
  
  ### --- Data
  # - WMF Analytics public data page
  pubPage <- 
    'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wikidata/WD_PageviewsPerType/'
  # - get update stamp:
  h <- curl::new_handle()
  curl::handle_setopt(h,
                copypostfields = "WD_percentUsageDashboard");
  curl::handle_setheaders(h,
                    "Cache-Control" = "no-cache"
  )
  timestamp <- 
    curl::curl_fetch_memory(pubPage)
  timestamp <- rawToChar(timestamp$content)
  timestamp <- stringr::str_extract_all(timestamp, "[[:digit:]]+.+[[:digit:]]+")[[1]][3]
  timestamp <- gsub("<.+", "", timestamp)
  timestamp <- trimws(timestamp, which = "right")
  timestamp <- paste0("Updated: ", timestamp, " UTC")
  # - get current data file:
  filename <- 
    paste0(pubPage, 'WD_pageviewsPerType.Rds')
  withProgress(message = 'Downloading data', 
               detail = "Please be patient.", 
               value = 0, {
                 dataSet <- readRDS(gzcon(url(filename)))
                 dataSet <- tidyr::spread(dataSet,
                                          key = agent_type,
                                          value = pageviews,
                                          fill = 0)
               })
  
  ### --- output: timestamp
  output$timestamp <- renderText({
    paste0('<p style="font-size:80%;"align="left"><b>', 
           timestamp,
           "</b></p>"
    )
  })
  
  ### --- constants
  desktopColors <- c("darkgreen", "lightgreen")
  mobileColors <- c("darkblue", "lightblue")
  
  ### --- function: preProcessData()
  preProcessData <- function(data, 
                             access, 
                             namespace) {
    # - dataset
    data <- data[grepl(access, data$access_method) & (data$namespace_id == namespace), ]
    averagePV_user <- round(mean(data$user, na.rm = T))
    averagePV_spider <- round(mean(data$spider, na.rm = T))
    averagePV_global <- round(mean(c(data$user, data$spider), na.rm = T))
    data$access_method <- NULL
    data$namespace <- paste0(data$namespace, " (", data$namespace_id, ")")
    data$namespace_id <- NULL
    data <- dplyr::select(data,
                          timestamp,
                          user, spider
    )
    ix <- data$timestamp
    data$timestamp <- NULL
    data <- xts::xts(data, order.by = ix)
    # - mainTitle
    if (namespace == 0) {
      lead <- "Item (0):"
    } else if (namespace == 120) {
      lead <- "Property (120):"
    } else if (namespace == 146) {
      lead <- "Lexeme (146):"
    } else {
      lead <- "EntitySchema (640):"
    }
    mainTitle <- paste0(
      lead, " ", stringr::str_to_title(access), " Pageviews, ", 
      " Daily average: User = ", averagePV_user, 
      ", Spider = ", averagePV_spider, "<br> Total Average = ", averagePV_global)
    # - out
    out <- list(data = data, mainTitle = mainTitle)
    return(out)
  }
  
  
  ### --- pre-process data for "dygraphDesktop_Item0"
  dyData01 <- preProcessData(data = dataSet, access = "desktop", namespace = 0)
  ### --- call module "dygraphDesktop_Item0"
  mod_dygraph_server("dygraphDesktop_Item0", dyData01$data , mainTitle = dyData01$mainTitle, desktopColors)
  
  ### --- pre-process data for "dygraphMobile_Item0"
  dyData02 <- preProcessData(data = dataSet, access = "mobile", namespace = 0)
  ### --- call module "dygraphMobile_Item0"
  mod_dygraph_server("dygraphMobile_Item0", dyData02$data , mainTitle = dyData02$mainTitle, mobileColors)
  
  ## --- pre-process data for "dygraphDesktop_Property120"
  dyData03 <- preProcessData(data = dataSet, access = "desktop", namespace = 120)
  ### --- call module "dygraphDesktop_Property120"
  mod_dygraph_server("dygraphDesktop_Property120", dyData03$data , mainTitle = dyData03$mainTitle, desktopColors)
  
  ## --- pre-process data for "dygraphMobile__Property120"
  dyData04 <- preProcessData(data = dataSet, access = "mobile", namespace = 120)
  ### --- call module "dygraphMobile__Property120"
  mod_dygraph_server("dygraphMobile__Property120", dyData04$data , mainTitle = dyData04$mainTitle, mobileColors)
  
  ## --- pre-process data for "dygraphDesktop_Lexeme146"
  dyData05 <- preProcessData(data = dataSet, access = "desktop", namespace = 146)
  ### --- call module "dygraphDesktop_Lexeme146"
  mod_dygraph_server("dygraphDesktop_Lexeme146", dyData05$data , mainTitle = dyData05$mainTitle, desktopColors)
  
  ## --- pre-process data for "dygraphMobile_Lexeme146"
  dyData06 <- preProcessData(data = dataSet, access = "mobile", namespace = 146)
  ### --- call module "dygraphMobile_Lexeme146"
  mod_dygraph_server("dygraphMobile_Lexeme146", dyData06$data , mainTitle = dyData06$mainTitle, mobileColors)
  
  ## --- pre-process data for "dygraphDesktop_EntitySchema640"
  dyData07 <- preProcessData(data = dataSet, access = "desktop", namespace = 640)
  ### --- call module "dygraphDesktop_EntitySchema640"
  mod_dygraph_server("dygraphDesktop_EntitySchema640", dyData07$data , mainTitle = dyData07$mainTitle, desktopColors)
  
  ## --- pre-process data for "dygraphMobile_EntitySchema640"
  dyData08 <- preProcessData(data = dataSet, access = "mobile", namespace = 640)
  ### --- call module "dygraphMobile_EntitySchema640"
  mod_dygraph_server("dygraphMobile_EntitySchema640", dyData08$data , mainTitle = dyData08$mainTitle, mobileColors)
  
}
