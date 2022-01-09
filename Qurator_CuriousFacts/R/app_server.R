### ---------------------------------------------------------------------------
### --- Qurator: Curious Facts
### --- Version 1.0.0
### --- 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {
  # - application server logic 
  
  ### --- constants
  # - endPointURL
  endPointURL <-
    'https://query.wikidata.org/bigdata/namespace/wdq/sparql?query='
  # - shared data directory
  dataDir <- "data/"
  
  ### --- Data
  # - get current Qurator Curious Facts:
  withProgress(message = "The Dashboard is loading data.", 
               detail = "Please be patient.", value = 0, {
                 
                 dataM1 <- data.table::fread(
                   paste0(dataDir, "dataM1.csv"),
                   header = TRUE
                   )
                 dataM1$explanation <- 
                   gsub('\"\"\"', '', dataM1$explanation)
                 infoM1 <- data.table::fread(
                   paste0(dataDir, "infoM1.csv"),
                   header = TRUE
                   )
                 infoM1$V1 <- NULL

                 incProgress(amount = .25, message = "M1 loaded.")
                 
                 dataM2 <- data.table::fread(
                   paste0(dataDir, "dataM2.csv"),
                   header = T
                   )
                 dataM2$explanation <- 
                   gsub('\"\"\"', '', dataM2$explanation)
                 infoM2 <- data.table::fread(
                   paste0(dataDir, "infoM2.csv"),
                   header = T
                   )
                 infoM2$V1 <- NULL
                 # - de-duplicate dataM2
                 dataM2 <- 
                   dataM2[!duplicated(dataM2[, c('item', 'property')]), ]
                  
                 incProgress(amount = .25, message = "M2 loaded.")
                 
                 dataM3 <- data.table::fread(
                   paste0(dataDir, "dataM3.csv"),
                   header = T
                   )
                 dataM3$explanation <- 
                   gsub('\"\"', '\"', dataM3$explanation)
                 infoM3 <- data.table::fread(
                   paste0(dataDir, "infoM3.csv"), 
                   header = T
                   )
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
  establishedOn <- dS$establishedOn[rf]
  
  ### --- First Fact
  output$fact <- renderText({

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
    res <- httr::GET(url = 
                       paste0(endPointURL, URLencode(query)))
    
    if (res$status_code == 200) {
      src <- jsonlite::fromJSON(
        rawToChar(res$content), 
        simplifyDataFrame = T)
      src <- src$results$bindings$image$value
      
      srcimg <- tryCatch({
        tail(strsplit(src, split = "/")[[1]], 1)},
        error = function(condition) {
          NULL
        })
      
      if(is.null(srcimg)) {
        return(
          '<p style="font-size:150%;"align="left"></p>'
        )
      }
      
      # - crop image 
      srcimg <- paste0("https://magnus-toolserver.toolforge.org/commonsapi.php?image=", 
                       srcimg, 
                       "&thumbwidth=300")
      srcimg <- httr::GET(srcimg)
      srcimg <- XML::xmlToList(XML::xmlParse(srcimg))
      
      if (length(!is.null(srcimg$file$urls$thumbnail)) > 0) {
        return(
          paste0('<p><img src="', 
                 URLencode(srcimg$file$urls$thumbnail),'">',
                 '<br>',
                 '<a href ="',
                 srcimg$file$urls$description, '">',
                 srcimg$file$title, 
                 '</a><br>',
                 srcimg$licenses$license$name, 
                 '</p>'
          )
        )  
      } else {
        paste0('<img src="', 
               URLencode(srcimg$file$urls$thumbnail),'">')
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
    establishedOn <- dS$establishedOn[rf]
    
    output$fact <- renderText({
      
      out <- paste0('<p style="font-size:120%;"align="left">',
                    fact, '</p><hr>', 
                    '<p style="font-size:120%;"align="left"> This fact was established on: <b>', 
                    establishedOn, 
                    "</b>, and is based on the <b>", 
                    iS$wdDumpSnapshot, 
                    "</b> snapshot in hdfs of the Wikidata JSON dump. 
                    Edits made after that date are not taken into account.")
      return(out)
      
    })
    
    output$factImage <- renderText({
      
      item <- dataSet[[cD]]$item[rf]
      query <- paste0('SELECT ?image {wd:', 
                      item, 
                      ' wdt:P18 ?image .}')
      res <- httr::GET(url = 
                         paste0(endPointURL, URLencode(query)))
      
      if (res$status_code == 200) {
        src <- jsonlite::fromJSON(
          rawToChar(res$content), 
          simplifyDataFrame = T)
        src <- src$results$bindings$image$value
        
        srcimg <- tryCatch({
          tail(strsplit(src, split = "/")[[1]], 1)},
          error = function(condition) {
            NULL
          })
        
        if(is.null(srcimg)) {
          return(
            '<p style="font-size:150%;"align="left"></p>'
          )
        }
        
        # - crop image 
        srcimg <- paste0("https://magnus-toolserver.toolforge.org/commonsapi.php?image=", 
                         srcimg, 
                         "&thumbwidth=300")
        srcimg <- httr::GET(srcimg)
        srcimg <- XML::xmlToList(XML::xmlParse(srcimg))
        
        if (length(!is.null(srcimg$file$urls$thumbnail)) > 0) {
          return(
            paste0('<p><img src="', 
                   URLencode(srcimg$file$urls$thumbnail),'">',
                   '<br>',
                   '<a href ="',
                   srcimg$file$urls$description, '">',
                   srcimg$file$title, 
                   '</a><br>',
                   srcimg$licenses$license$name, 
                   '</p>'
            )
          )  
        } else {
          paste0('<img src="', 
                 URLencode(srcimg$file$urls$thumbnail),'">')
        } 
      } else {
        return(
          '<p style="font-size:150%;"align="left"></p>'
        )
      }
      
    })
    
  })
  
}
