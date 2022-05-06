### ---------------------------------------------------------------------------
### --- WD Inequality
### --- Version 1.0.0
### --- 2021.
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

  ### --- GET DATA
  dataFile <- 
    'https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/Wikidata/WD_Inequality/HooverUpdate.csv'
  
  # - get current data files + pre-process:
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    ### --- Download matches
    incProgress(1/1, detail = "WD Edits Inequality Dataset.")
    hooverFrame <- read.csv(dataFile,
                            check.names = F,
                            stringsAsFactors = F)
    colnames(hooverFrame) <- c('Hoover Index', 'Measurement', 'Snapshot')
    hooverFrame <- hooverFrame[, c('Measurement', 'Snapshot', 'Hoover Index')]
    # - Fix measurement names as per https://phabricator.wikimedia.org/T270109#6899631
    hooverFrame$Measurement[hooverFrame$Measurement == "current_year"] <- 
      "this year"
    hooverFrame$Measurement[hooverFrame$Measurement == "current_snapshot"] <- 
      "over last month"
    hooverFrame$Measurement[hooverFrame$Measurement == "history_to_current_snapshot"] <- 
      "all time"
  })
  
  ### --- TABLES
  
  ### --- output$hooverFrame
  output$hooverFrame <- DT::renderDataTable({
    dataset <- hooverFrame
    dataset$`Hoover Index` <- round(dataset$`Hoover Index`, 3)
    DT::datatable(dataset, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  # - Download: hooverFrame_download
  output$hooverFrame_download <- downloadHandler(
    filename = function() {
      'WD_Inequality.csv'},
    content = function(file) {
      write.csv(hooverFrame,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### --- CHARTS
  output$hooverIndexPlot <- plotly::renderPlotly({
    g <- ggplot2::ggplot(data = hooverFrame,
                         ggplot2::aes(x = Snapshot,
                                      y = `Hoover Index`,
                                      group = Measurement,
                                      color = Measurement)
                         ) +
      ggplot2::geom_line(size = .25) +
      ggplot2::geom_point(size = 1.5) + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border = ggplot2::element_blank()) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8,
                                                         angle = 90,
                                                         hjust = 0.95,
                                                         vjust = 0.2))
    plotly::ggplotly(g,
                     tooltip = c("x","y", "label"),
                     originalData = T) %>%
      plotly::config(displayModeBar = FALSE) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
}
