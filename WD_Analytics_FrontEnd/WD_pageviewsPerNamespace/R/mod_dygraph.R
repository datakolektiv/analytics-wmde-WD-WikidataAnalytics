### ---------------------------------------------------------------------------
### --- WD Pageviews per Namespace
### --- Version 1.0.0
### --- 2019.
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