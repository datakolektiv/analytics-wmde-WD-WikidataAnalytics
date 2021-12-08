### ---------------------------------------------------------------------------
### --- WD Pageviews per Namespace
### --- Version 1.0.0
### --- 2019.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

#' dygraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr

mod_dygraph_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             shinycssloaders::withSpinner(dygraphs::dygraphOutput(ns("dygraph")))
      )
    )
  )
}
    
#' dygraph Server Functions
#'
#' @noRd 
mod_dygraph_server <- function(id, data, mainTitle, seriesColor) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # - output$dygraph
      output$dygraph <- dygraphs::renderDygraph({
        dygraphs::dygraph(data, main = mainTitle) %>%
          dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>%
          dygraphs::dyOptions(colors = seriesColor,
                    titleHeight = 20,
                    fillGraph = TRUE, fillAlpha = 0.4, 
                    drawPoints = TRUE, pointSize = 2, 
                    maxNumberWidth = 40, 
                    labelsKMB = TRUE) %>% 
          dygraphs::dyHighlight(highlightCircleSize = 3, 
                      highlightSeriesBackgroundAlpha = 0.2,
                      hideOnMouseOut = TRUE) %>% 
          dygraphs::dyRangeSelector(height = 25, strokeColor = "")
      })
      
    }
  )
}