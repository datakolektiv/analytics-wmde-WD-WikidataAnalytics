### ---------------------------------------------------------------------------
### --- WD Human vs Bot Edits
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
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  ### --- publicDir
  publicDir <- 
    'https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/Wikidata/WD_HumanEdits/'
  
  ### --- functions
  get_WDCM_table <- function(url_dir, filename, row_names) {
    read.csv(paste0(url_dir, filename), 
             header = T, 
             stringsAsFactors = F,
             check.names = F)
  }
  
  # - get update stamp:
  h <- curl::new_handle()
  curl::handle_setopt(h,
                      copypostfields = "WD_humanEditsPerClass")
  curl::handle_setheaders(h,
                          "Cache-Control" = "no-cache"
                          )
  timestamp <- 
    curl::curl_fetch_memory(publicDir)
  timestamp <- rawToChar(timestamp$content)
  timestamp <- stringr::str_extract_all(timestamp, "[[:digit:]]+.+[[:digit:]]+")[[1]][3]
  timestamp <- gsub("<.+", "", timestamp)
  timestamp <- trimws(timestamp, which = "right")
  timestamp <- paste0("Updated: ", timestamp, " UTC")
  
  # - get current data file:
  filename <- 'WD_HumanEdits.csv'
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    dataSet <- get_WDCM_table(publicDir, filename)
    dataSet[, 1] <- NULL
    dataSet$wd_class <- gsub('""', '"', dataSet$wd_class) 
  })
  
  ### --- timestamp
  output$timestamp <- renderText({
    paste0('<p style="font-size:90%;"align="left"><b>',
           timestamp, '</b></p>'
    )
  })
  
  ### --- Wikidata Logo
  output$logo <- renderUI({
    tags$img(src =
               'https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Wikidata-logo-en.svg/200px-Wikidata-logo-en.svg.png')
  })
  
  ### --- TABLE
  ### --- output$overviewDT
  output$overviewDT <- DT::renderDataTable({
    dSet <- dataSet
    colnames(dSet) <- c('Class', 'Label', 'Items', 'Human Edited Items', 
                        '% Items Touched', 'Median Unique Editors', 'Human Edits', 
                        'Bot Edits', 'Total Edits', 'Human/Bot Edits Ratio', 
                        '% Human Edits', '% Bot Edits')
    DT::datatable(dSet, 
                  options = list(
                    escape = F,
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE,
                  escape = F
    )
  }, 
  server = T)
  
  ### --- human_to_bot_ratio
  output$human_to_bot_ratio <- renderPlot({
    # - distribution: human_to_bot_ratio
    ggplot2::ggplot(dataSet,
                    ggplot2::aes(x = log(human_to_bot_ratio))) +
      ggplot2::geom_density(color = "darkblue",
                            fill = "lightblue",
                            size = .05,
                            alpha = .5) + 
      ggplot2::geom_vline(xintercept = 0, size = .15, linetype = "dashed") + 
      ggplot2::xlab("log(Human to Bot Edits Ratio)") + 
      ggplot2::ggtitle(label = "Human to Bots Edit Ratio (per Class) in Wikidata",
                       subtitle = "Note: Classes with 0 bot edits are not accounted for (Ratio = Infinite).") + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border = ggplot2::element_blank()) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14)) + 
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- median_unique_editors
  output$median_unique_editors <- renderPlot({
    histUnique <- hist(dataSet$median_unique_editors, 
                       plot = F)
    bins <- numeric()
    for (i in 2:length(histUnique$breaks)) {
      bins[i-1] <- paste0("(", histUnique$breaks[i-1], 
                          " - ",
                          histUnique$breaks[i], ")")
    }
    histUnique <- data.frame(mids = histUnique$mids, 
                             counts = histUnique$counts, 
                             bins = paste0(bins, ": ", histUnique$counts), 
                             stringsAsFactors = F)
    histUnique$bins <- ifelse(histUnique$counts == 0, 
                              "",
                              histUnique$bins)
    histUnique$counts[histUnique$counts == 0] <- NA
    ggplot2::ggplot(histUnique,
                    ggplot2::aes(x = log(mids),
                                 y = log(counts),
                                 label = bins)) +
      ggplot2::geom_smooth(method = "lm",
                           size = .1,
                           color = "darkred",
                           linetype = "dashed") + 
      ggplot2::geom_path(size = .15, color = "red") + 
      ggplot2::geom_point(size = 2, color = "red") + 
      ggplot2::geom_point(size = 1.5, color = "white") + 
      ggrepel::geom_text_repel(size = 3.5, min.segment.length = 10) + 
      ggplot2::xlab("log(Median Unique Human Editors per Class)") + 
      ggplot2::ggtitle("Median Unique Human Editors (per Class) in Wikidata") + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border = ggplot2::element_blank()) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8,
                                                         hjust = 0.95,
                                                         vjust = 0.2))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- median_unique_editors_interactive
  output$median_unique_editors_interactive <- plotly::renderPlotly({
    histUnique <- hist(dataSet$median_unique_editors, 
                       plot = F)
    bins <- numeric()
    for (i in 2:length(histUnique$breaks)) {
      bins[i-1] <- paste0("(", histUnique$breaks[i-1], 
                          " - ",
                          histUnique$breaks[i], ")")
    }
    histUnique <- data.frame(mids = histUnique$mids, 
                             counts = histUnique$counts, 
                             bins = paste0(bins, ": ", histUnique$counts), 
                             stringsAsFactors = F)
    histUnique$bins <- ifelse(histUnique$counts == 0, 
                              "",
                              histUnique$bins)
    histUnique$counts[histUnique$counts == 0] <- NA
    g <- ggplot2::ggplot(histUnique,
                    ggplot2::aes(x = log(mids),
                                 y = log(counts),
                                 label = bins)) +
      ggplot2::geom_smooth(method = "lm",
                           size = .1,
                           color = "darkred",
                           linetype = "dashed") + 
      ggplot2::geom_path(size = .15, color = "red") + 
      ggplot2::geom_point(size = 2, color = "red") + 
      ggplot2::xlab("log(Median Unique Human Editors per Class)") + 
      ggplot2::ggtitle("Median Unique Human Editors (per Class) in Wikidata") + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border = ggplot2::element_blank()) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8,
                                                         hjust = 0.95,
                                                         vjust = 0.2))
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- proportion of items touched
  output$proportion_items_touched <- renderPlot({
    ggplot2::ggplot(dataSet,
                    ggplot2::aes(x = percent_items_touched/100)) +
      ggplot2::geom_density(color = "darkorange",
                            fill = "orange",
                            size = .15,
                            alpha = .5) + 
      ggplot2::xlab("Proportion") + 
      ggplot2::ggtitle("Proportion of Items Ever Touched by Human Editors (per Class) in Wikidata") + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border = ggplot2::element_blank()) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- total number of edits per class
  output$total_edits_class <- renderPlot({
    # - distribution: total number of edits
    histUnique <- hist(dataSet$total_edits,
                       breaks = c(0, 1, 2, 3, 4, 5, 10, 15, 20, 30, 40, 50, 100, 
                                  200, 300, 400, 500, 
                                  1000, 2000, 3000, 4000, 5000, 
                                  1e4, 2e4, 3e4, 4e4, 5e4, 1e5, 
                                  5e5, 1e6, 2e6, 3e6 , max(dataSet$total_edits)),
                       plot = F)
    bins <- numeric()
    for (i in 2:length(histUnique$breaks)) {
      bins[i-1] <- paste0(histUnique$breaks[i-1], 
                          " - ",
                          histUnique$breaks[i])
    }
    histUnique <- data.frame(mids = histUnique$mids, 
                             counts = histUnique$counts, 
                             bins = bins, 
                             stringsAsFactors = F)
    histUnique$bins <- ifelse(histUnique$counts == 0, 
                              "",
                              histUnique$bins)
    histUnique$counts[histUnique$counts == 0] <- NA
    ggplot2::ggplot(histUnique,
                    ggplot2::aes(x = log(mids),
                                 y = counts,
                                 label = counts)) +
      ggplot2::geom_path(size = .15, color = "darkred") + 
      ggplot2::geom_point(size = 2, color = "darkred") + 
      ggplot2::geom_point(size = 1.5, color = "white") +
      ggrepel::geom_text_repel(size = 3.5) + 
      ggplot2::scale_x_continuous(breaks = log(histUnique$mids),
                                  labels = histUnique$bins) +
      ggplot2::xlab("Edits Range") + 
      ggplot2::ylab("Classes") + 
      ggplot2::ggtitle("Total Edits per Wikidata Class") + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border = ggplot2::element_blank()) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         size = 8,
                                                         hjust = 0.95,
                                                         vjust = 0.2))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- total number of edits per class Interactive
  output$total_edits_class_interactive <- plotly::renderPlotly({
    # - distribution: total number of edits
    histUnique <- hist(dataSet$total_edits,
                       breaks = c(0, 1, 2, 3, 4, 5, 10, 15, 20, 30, 40, 50, 100, 
                                  200, 300, 400, 500, 
                                  1000, 2000, 3000, 4000, 5000, 
                                  1e4, 2e4, 3e4, 4e4, 5e4, 1e5, 
                                  5e5, 1e6, 2e6, 3e6 , max(dataSet$total_edits)),
                       plot = F)
    bins <- numeric()
    for (i in 2:length(histUnique$breaks)) {
      bins[i-1] <- paste0(histUnique$breaks[i-1], 
                          " - ",
                          histUnique$breaks[i])
    }
    histUnique <- data.frame(mids = histUnique$mids, 
                             counts = histUnique$counts, 
                             bins = bins, 
                             stringsAsFactors = F)
    histUnique$bins <- ifelse(histUnique$counts == 0, 
                              "",
                              histUnique$bins)
    histUnique$counts[histUnique$counts == 0] <- NA
    g <- ggplot2::ggplot(histUnique,
                    ggplot2::aes(x = log(mids),
                                 y = counts,
                                 label = counts)) +
      ggplot2::geom_path(size = .15, color = "darkred") + 
      ggplot2::geom_point(size = 2, color = "darkred") + 
      ggplot2::scale_x_continuous(breaks = log(histUnique$mids),
                                  labels = histUnique$bins) +
      ggplot2::xlab("Edits Range") + 
      ggplot2::ylab("Classes") + 
      ggplot2::ggtitle("Total Edits per Wikidata Class") + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border = ggplot2::element_blank()) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         size = 8,
                                                         hjust = 0.95,
                                                         vjust = 0.2))
    plotly::ggplotly(g, 
                     tooltip = c("x","y"),
                     originalData = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
}
