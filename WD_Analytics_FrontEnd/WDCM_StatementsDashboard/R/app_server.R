### ---------------------------------------------------------------------------
### --- WDCM Statements
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
#' @import dplyr
#' @import ggplot2
#' @import DT
#' @import plotly
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  ### --- functions
  get_WDCM_table <- function(url_dir, filename, row_names = F) {
    if (row_names == T) {
      read.csv(URLencode(paste0(url_dir, filename)), 
               header = T, 
               stringsAsFactors = F,
               check.names = F, 
               row.names = 1)
    } else {
      read.csv(URLencode(paste0(url_dir, filename)), 
               header = T, 
               stringsAsFactors = F,
               check.names = F)
    }
  }
  
  ### --- Config
  etlDir <- 'https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/'
  
  ### --- Set decimal points and disable scientific notation
  options(digits = 3, scipen = 999) 
  
  ### --- DATA
  
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    
    ### --- fetch wd_statements_propertiesSet.csv
    propertiesSet <- 
      get_WDCM_table(etlDir, 
                     'wd_statements_propertiesSet.csv', 
                     row_names = 1)
    
    ### --- fetch wd_statements_items_usage_top10000.csv
    items_in_claims <-
      get_WDCM_table(etlDir, 
                     'wd_statements_items_usage_top10000.csv', 
                     row_names = 1)
    
    ### --- fetch wd_statements_C_aspect_reuse_properties.csv
    properties_reuse <- 
      get_WDCM_table(etlDir, 
                     'wd_statements_C_aspect_reuse_properties.csv', 
                     row_names = 1)
    # - join with propertiesSet
    propertiesSet <- dplyr::left_join(
      propertiesSet, 
      properties_reuse, 
      by = "property"
    )
    rm(properties_reuse)
    propertiesSet$C_reuse[is.na(propertiesSet$C_reuse)] <- 0
    
    ### --- fetch wd_statements_C_aspect_reuse_items_top10000.csv	
    items_reuse <- 
      get_WDCM_table(etlDir, 
                     'wd_statements_C_aspect_reuse_items_top10000.csv', 
                     row_names = 1)
    
    ### --- Fetch update timestamp
    update <- get_WDCM_table(etlDir, 
                             'updateTimestamp.csv', 
                             row_names = 1)
    
    ### --- Fetch wmf.wikidata_entity snapshot
    snapshot <- get_WDCM_table(etlDir,
                               'currentSnap.csv',
                               row_names = 1)
    
  })
  
  ### ----------------------------------
  ### --- OUTPUTS
  ### ----------------------------------
  
  ### --- output: updateString
  output$updateString <- renderText({
    date <- update$timestamp[1]
    return(paste('<p style="font-size:80%;"align="right"><b>Updated: </b><i>', 
                 date, '</i></p>', sep = ""))
  })
  ### --- output: currentDump
  output$currentDump <- renderText({
    snapshot <- snapshot$x[1]
    return(paste('<p style="font-size:80%;"align="right"><b>Wikidata JSON dump used: </b><i>', 
                 snapshot, '</i></p>', sep = ""))
  })
  
  ### ----------------------------------
  ### --- TAB: Properties
  ### ----------------------------------
  
  ### --- output$property_use_in_claims
  output$property_use_in_claims <- plotly::renderPlotly({
    dataSet <- propertiesSet %>% 
      dplyr::select(property, en_label, used_in_claims) %>% 
      dplyr::arrange(dplyr::desc(used_in_claims)) %>% 
      head(100) %>% 
      dplyr::filter(used_in_claims > 0)
    dataSet$label <- paste0(dataSet$en_label, " (", 
                            dataSet$property, ")")
    dataSet$label <- factor(dataSet$label, 
                            levels = dataSet$label[order(-dataSet$used_in_claims)])
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = label, 
                                               y = used_in_claims)) +
      ggplot2::geom_line(size = .25, color = "indianred", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "indianred") + 
      ggplot2::ylab('Usage in claims (log scale)') + 
      ggplot2::scale_y_continuous(trans='log2') + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  ### --- output$property_use_in_qualifiers
  output$property_use_in_qualifiers <- plotly::renderPlotly({
    dataSet <- propertiesSet %>% 
      dplyr::select(property, en_label, used_in_qualifiers) %>% 
      dplyr::arrange(dplyr::desc(used_in_qualifiers)) %>% 
      head(100) %>% 
      dplyr::filter(used_in_qualifiers > 0)
    dataSet$label <- paste0(dataSet$en_label, " (", 
                            dataSet$property, ")")
    dataSet$label <- factor(dataSet$label, 
                            levels = dataSet$label[order(-dataSet$used_in_qualifiers)])
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = label, 
                                               y = used_in_qualifiers)) +
      ggplot2::geom_line(size = .25, color = "darkgreen", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "darkgreen") + 
      ggplot2::ylab('Usage in qualifiers (log scale)') + 
      ggplot2::scale_y_continuous(trans='log2') + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  ### --- output$property_use_in_references
  output$property_use_in_references <- plotly::renderPlotly({
    dataSet <- propertiesSet %>% 
      dplyr::select(property, en_label, used_in_references) %>% 
      dplyr::arrange(dplyr::desc(used_in_references)) %>% 
      head(100) %>% 
      dplyr::filter(used_in_references > 0)
    dataSet$label <- paste0(dataSet$en_label, " (", 
                            dataSet$property, ")")
    dataSet$label <- factor(dataSet$label, 
                            levels = dataSet$label[order(-dataSet$used_in_references)])
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = label, 
                                               y = used_in_references)) +
      ggplot2::geom_line(size = .25, color = "cadetblue3", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "cadetblue3") + 
      ggplot2::ylab('Usage in references (log scale)') + 
      ggplot2::scale_y_continuous(trans='log2') + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  ### --- output$property_num_references
  output$property_num_references <- plotly::renderPlotly({
    dataSet <- propertiesSet %>% 
      dplyr::select(property, en_label, num_references) %>% 
      dplyr::arrange(dplyr::desc(num_references)) %>% 
      head(100) %>% 
      dplyr::filter(num_references > 0)
    dataSet$label <- paste0(dataSet$en_label, " (", 
                            dataSet$property, ")")
    dataSet$label <- factor(dataSet$label, 
                            levels = dataSet$label[order(-dataSet$num_references)])
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = label, 
                                               y = num_references)) +
      ggplot2::geom_line(size = .25, color = "darkorange", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "darkorange") + 
      ggplot2::ylab('Number of references (log scale)') + 
      ggplot2::scale_y_continuous(trans='log2') + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  ### --- output$property_c_reuse
  output$property_c_reuse <- plotly::renderPlotly({
    dataSet <- propertiesSet %>% 
      dplyr::select(property, en_label, C_reuse) %>% 
      dplyr::arrange(dplyr::desc(C_reuse)) %>%
      head(100) %>% 
      dplyr::filter(C_reuse > 0)
    dataSet$label <- paste0(dataSet$en_label, " (", 
                            dataSet$property, ")")
    dataSet$label <- factor(dataSet$label, 
                            levels = dataSet$label[order(-dataSet$C_reuse)])
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = label, 
                                               y = C_reuse)) +
      ggplot2::geom_line(size = .25, color = "darkorange", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "darkorange") + 
      ggplot2::ylab('Reuse (C Aspect,  log scale)') + 
      ggplot2::scale_y_continuous(trans='log2') + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  
  ### --- output$propertiesSet
  output$propertiesSet <- DT::renderDataTable({
    propertiesSetFrame <- propertiesSet
    colnames(propertiesSetFrame) <- c('Property', 
                                      'Label', 
                                      'Used in claims', 
                                      'Used in references',
                                      'Used in qualifiers',
                                      'Num. of references', 
                                      'Reuse')
    propertiesSetFrame$Property <- paste0(
      '<a href="https://www.wikidata.org/wiki/Property:',
      propertiesSetFrame$Property, 
      '" target="_blank">', 
      propertiesSetFrame$Property, 
      "</a>")
    DT::datatable(propertiesSetFrame,
              options = list(
                pageLength = 50,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE,
              escape = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  
  ### ----------------------------------
  ### --- TAB: Items
  ### ----------------------------------
  
  ### --- output$items_use_in_properties
  output$items_use_in_properties <- plotly::renderPlotly({
    dataSet <- items_in_claims %>% 
      dplyr::arrange(dplyr::desc(used_in_properties)) %>% 
      head(100)
    colnames(dataSet) <- c('Item', 
                           'Used as value', 
                           'Label')
    dataSet$label <- paste0(dataSet$Label, " (", 
                            dataSet$Item, ")")
    dataSet$label <- factor(dataSet$label, 
                            levels = dataSet$label[order(-dataSet$`Used as value`)])
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = label, 
                                               y = `Used as value`)) +
      ggplot2::geom_line(size = .25, color = "cadetblue3", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "cadetblue3") + 
      ggplot2::ylab('Usage as value (log scale)') + 
      ggplot2::scale_y_continuous(trans='log2') + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  ### --- output$items_reuse
  output$items_reuse <- plotly::renderPlotly({
    dataSet <- items_reuse %>% 
      dplyr::arrange(dplyr::desc(C_reuse)) %>% 
      head(100)
    colnames(dataSet) <- c('Item', 
                           'Label', 
                           'Reuse')
    dataSet$label <- ifelse(nchar(dataSet$Label) > 20, 
                            paste0(substr(dataSet$Label, 1, 20), "..."), 
                            dataSet$Label)
    dataSet$label <- paste0(dataSet$label, " (", 
                            dataSet$Item, ")")
    dataSet$label <- factor(dataSet$label, 
                            levels = dataSet$label[order(-dataSet$Reuse)])
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = label, 
                                               y = Reuse, 
                                               label = Label)) +
      ggplot2::geom_line(size = .25, color = "indianred", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "indianred") + 
      ggplot2::ylab('Reuse (log scale)') + 
      ggplot2::scale_y_continuous(trans='log2') + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  ### --- output$items_wd_set
  output$items_wd_set <- DT::renderDataTable({
    items_wd_set <- items_in_claims
    colnames(items_wd_set) <- c('Item',
                                'Used in properties',
                                'Label')
    items_wd_set <- items_wd_set[, c('Item', 'Label', 'Used in properties')]
    items_wd_set$Item <- paste0(
      '<a https://www.wikidata.org/wiki/',
      items_wd_set$Item, 
      '" target="_blank">', 
      items_wd_set$Item, 
      "</a>")
    DT::datatable(items_wd_set,
              options = list(
                pageLength = 50,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE,
              escape = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$items_reuse_set
  output$items_reuse_set <- DT::renderDataTable({
    items_reuse_set <- items_reuse
    colnames(items_reuse_set) <- c('Item',
                                   'Label',
                                   'Reuse')
    items_reuse_set$Item <- paste0(
      '<a https://www.wikidata.org/wiki/',
      items_reuse_set$Item, 
      '" target="_blank">', 
      items_reuse_set$Item, 
      "</a>")
    DT::datatable(items_reuse_set,
              options = list(
                pageLength = 50,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE,
              escape = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
}
