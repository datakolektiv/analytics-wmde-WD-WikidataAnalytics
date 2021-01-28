### ---------------------------------------------------------------------------
### --- WD Languages Landscape
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
    read.csv(paste0(url_dir, filename), 
             header = T, 
             stringsAsFactors = F,
             check.names = F)
  }
  
  ### --- Config File
  params <- XML::xmlParse(system.file("app/www/WD_LanguagesLandscape_Config.xml", 
                                 package = "WDLanguagesLandscape"))
  params <- XML::xmlToList(params)
  apiPrefix <- params$general$apiPrefix
  
  ### --- Fetch DATAMODEL TERMS
  ### --- Fetch data files
  withProgress(message = 'Downloading datasets.', detail = "Please be patient.", value = 0, {
    
    ### --- Fetch Datamodel Terms: EVERYTHING
    incProgress(1/12, detail = "Datamodel terms.")
    labels <- get_WDCM_table(params$general$publishedDatamodelDir,
                             'DM_Terms_Labels.csv',
                             row_names = F)
    labels[, 1] <- NULL
    descriptions <- get_WDCM_table(params$general$publishedDatamodelDir,
                                   'DM_Terms_Descriptions.csv',
                                   row_names = F)
    descriptions[, 1] <- NULL
    aliases <- get_WDCM_table(params$general$publishedDatamodelDir,
                              'DM_Terms_Aliases.csv',
                              row_names = F)
    aliases[, 1] <- NULL
    
    ### --- Fetch Datamodel Terms: ASTRONOMY
    incProgress(1/12, detail = "Datamodel terms.")
    labels_ASTRONOMY <- get_WDCM_table(params$general$publishedDatamodelDir,
                             'DM_Terms_Labels_ASTRONOMY.csv',
                             row_names = F)
    labels_ASTRONOMY[, 1] <- NULL
    w <- which(labels_ASTRONOMY$language == '')
    if (length(w) > 0) {
      labels_ASTRONOMY <- labels_ASTRONOMY[-w, ]
    }
    descriptions_ASTRONOMY <- get_WDCM_table(params$general$publishedDatamodelDir,
                                   'DM_Terms_Descriptions_ASTRONOMY.csv',
                                   row_names = F)
    descriptions_ASTRONOMY[, 1] <- NULL
    w <- which(descriptions_ASTRONOMY$language == '')
    if (length(w) > 0) {
      descriptions_ASTRONOMY <- descriptions_ASTRONOMY[-w, ]
    }
    aliases_ASTRONOMY <- get_WDCM_table(params$general$publishedDatamodelDir,
                              'DM_Terms_Aliases_ASTRONOMY.csv',
                              row_names = F)
    aliases_ASTRONOMY[, 1] <- NULL
    w <- which(aliases_ASTRONOMY$language == '')
    if (length(w) > 0) {
      aliases_ASTRONOMY <- aliases_ASTRONOMY[-w, ]
    }
    
    ### --- Fetch Datamodel Terms: SCIENTIFICPAPERS
    incProgress(1/12, detail = "Datamodel terms.")
    labels_SCIENTIFICPAPERS <- get_WDCM_table(params$general$publishedDatamodelDir,
                                       'DM_Terms_Labels_SCIENTIFICPAPERS.csv',
                                       row_names = F)
    labels_SCIENTIFICPAPERS[, 1] <- NULL
    w <- which(labels_SCIENTIFICPAPERS$language == '')
    if (length(w) > 0) {
      labels_SCIENTIFICPAPERS <- labels_SCIENTIFICPAPERS[-w, ]
    }
    descriptions_SCIENTIFICPAPERS <- get_WDCM_table(params$general$publishedDatamodelDir,
                                             'DM_Terms_Descriptions_SCIENTIFICPAPERS.csv',
                                             row_names = F)
    descriptions_SCIENTIFICPAPERS[, 1] <- NULL
    w <- which(descriptions_SCIENTIFICPAPERS$language == '')
    if (length(w) > 0) {
      descriptions_SCIENTIFICPAPERS <- descriptions_SCIENTIFICPAPERS[-w, ]
    }
    aliases_SCIENTIFICPAPERS <- get_WDCM_table(params$general$publishedDatamodelDir,
                                        'DM_Terms_Aliases_SCIENTIFICPAPERS.csv',
                                        row_names = F)
    aliases_SCIENTIFICPAPERS[, 1] <- NULL
    w <- which(aliases_SCIENTIFICPAPERS$language == '')
    if (length(w) > 0) {
      aliases_SCIENTIFICPAPERS <- aliases_SCIENTIFICPAPERS[-w, ]
    }
    
    ### --- Fetch Datamodel Terms: EVERYTHINGMINUS
    incProgress(1/12, detail = "Datamodel terms.")
    labels_EVERYTHINGMINUS <- get_WDCM_table(params$general$publishedDatamodelDir,
                                              'DM_Terms_Labels_EVERYTHINGMINUS.csv',
                                              row_names = F)
    labels_EVERYTHINGMINUS[, 1] <- NULL
    w <- which(labels_EVERYTHINGMINUS$language == '')
    if (length(w) > 0) {
      labels_EVERYTHINGMINUS <- labels_EVERYTHINGMINUS[-w, ]
    }
    descriptions_EVERYTHINGMINUS <- get_WDCM_table(params$general$publishedDatamodelDir,
                                                    'DM_Terms_Descriptions_EVERYTHINGMINUS.csv',
                                                    row_names = F)
    descriptions_EVERYTHINGMINUS[, 1] <- NULL
    w <- which(descriptions_EVERYTHINGMINUS$language == '')
    if (length(w) > 0) {
      descriptions_EVERYTHINGMINUS <- descriptions_EVERYTHINGMINUS[-w, ]
    }
    aliases_EVERYTHINGMINUS <- get_WDCM_table(params$general$publishedDatamodelDir,
                                               'DM_Terms_Aliases_EVERYTHINGMINUS.csv',
                                               row_names = F)
    aliases_EVERYTHINGMINUS[, 1] <- NULL
    w <- which(aliases_EVERYTHINGMINUS$language == '')
    if (length(w) > 0) {
      aliases_EVERYTHINGMINUS <- aliases_EVERYTHINGMINUS[-w, ]
    }
    
    ### --- Process Datamodel Terms: EVERYTHING
    labelsEVERYTHING <- labels %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    labelsEVERYTHING$averageM <- labelsEVERYTHING$average/1000000
    
    aliasesEVERYTHING <- aliases %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    aliasesEVERYTHING$averageM <- aliasesEVERYTHING$average/1000000
    
    descriptionsEVERYTHING <- descriptions %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    descriptionsEVERYTHING$averageM <- descriptionsEVERYTHING$average/1000000
    
    ### --- Process Datamodel Terms: ASTRONOMY
    labels_ASTRONOMY <- labels_ASTRONOMY %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    labels_ASTRONOMY$averageM <- labels_ASTRONOMY$average/1000000
    
    aliases_ASTRONOMY <- aliases_ASTRONOMY %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    aliases_ASTRONOMY$averageM <- aliases_ASTRONOMY$average/1000000
    
    descriptions_ASTRONOMY <- descriptions_ASTRONOMY %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    descriptions_ASTRONOMY$averageM <- descriptions_ASTRONOMY$average/1000000
    
    ### --- Process Datamodel Terms: SCIENTIFICPAPERS
    labels_SCIENTIFICPAPERS <- labels_SCIENTIFICPAPERS %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    labels_SCIENTIFICPAPERS$averageM <- labels_SCIENTIFICPAPERS$average/1000000
    
    aliases_SCIENTIFICPAPERS <- aliases_SCIENTIFICPAPERS %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    aliases_SCIENTIFICPAPERS$averageM <- aliases_SCIENTIFICPAPERS$average/1000000
    
    descriptions_SCIENTIFICPAPERS <- descriptions_SCIENTIFICPAPERS %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    descriptions_SCIENTIFICPAPERS$averageM <- descriptions_SCIENTIFICPAPERS$average/1000000
    
    ### --- Process Datamodel Terms: EVERYTHINGMINUS
    labels_EVERYTHINGMINUS <- labels_EVERYTHINGMINUS %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    labels_EVERYTHINGMINUS$averageM <- labels_EVERYTHINGMINUS$average/1000000
    
    aliases_EVERYTHINGMINUS <- aliases_EVERYTHINGMINUS %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    aliases_EVERYTHINGMINUS$averageM <- aliases_EVERYTHINGMINUS$average/1000000
    
    descriptions_EVERYTHINGMINUS <- descriptions_EVERYTHINGMINUS %>% 
      dplyr::select(snapshot, count, numitems) %>% 
      dplyr::group_by(snapshot) %>% 
      dplyr::summarise(average = sum(count)/mean(numitems)) %>% 
      dplyr::arrange(snapshot)
    descriptions_EVERYTHINGMINUS$averageM <- descriptions_EVERYTHINGMINUS$average/1000000
    
    
    ### --- Fetch ontology structure
    incProgress(2/12, detail = "Ontology structure.")
    ontologyStructure <- get_WDCM_table(params$general$publishedDataDir,
                                        'WD_Languages_OntologyStructure.csv',
                                        row_names = F)
    ontologyStructure[, 1] <- NULL
    
    ### --- Fetch used languages
    incProgress(3/12, detail = "Used languages.")
    usedLanguages <- get_WDCM_table(params$general$publishedDataDir,
                                    'WD_Languages_UsedLanguages.csv',
                                    row_names = F)
    usedLanguages[, 1] <- NULL
    usedLanguages$label <- paste0(tolower(usedLanguages$description), " (", 
                                  usedLanguages$languageURI, ")")
    # - Fetch Jaccard Similarity
    incProgress(4/12, detail = "Jaccard similarity.")
    simData <- get_WDCM_table(params$general$publishedDataDir,
                              'WD_Languages_Jaccard_Similarity.csv',
                              row_names = F)
    simData[, 1] <- NULL
    simData$avg_reuse_per_item <- simData$reuse/simData$num_items_reused
    simData$prop_items_used <- simData$num_items_reused/simData$item_count
    
    ### --- fetch language status files
    incProgress(5/12, detail = "UNESCO Status - Sitelinks.")
    status_UNESCO_Sitelinks <- get_WDCM_table(params$general$publishedDataDir,
                                              URLencode('WD_Vis_UNESCO Language Status_Sitelinks.csv'),
                                              row_names = F)
    status_UNESCO_Sitelinks[, 1] <- NULL
    incProgress(6/12, detail = "Ethnologue Status - Sitelinks.")
    status_Ethnologue_Sitelinks <- get_WDCM_table(params$general$publishedDataDir,
                                                  URLencode('WD_Vis_EthnologueLanguageStatus_Sitelinks.csv'),
                                                  row_names = F)
    status_Ethnologue_Sitelinks[, 1] <- NULL
    incProgress(7/12, detail = "UNESCO Status - Items")
    status_UNESCO_numItems <- get_WDCM_table(params$general$publishedDataDir,
                                             URLencode('WD_Vis_UNESCO Language Status_NumItems.csv'),
                                             row_names = F)
    status_UNESCO_numItems[, 1] <- NULL
    incProgress(8/12, detail = "Ethnologue Status - Items")
    status_Ethnologue_numItems <- get_WDCM_table(params$general$publishedDataDir,
                                                 URLencode('WD_Vis_EthnologueLanguageStatus_NumItems.csv'),
                                                 row_names = F)
    status_Ethnologue_numItems[, 1] <- NULL
    incProgress(9/12, detail = "UNESCO Status - Re-use")
    status_UNESCO_reuse <- get_WDCM_table(params$general$publishedDataDir,
                                          URLencode('WD_Vis_UNESCO Language Status_ItemReuse.csv'),
                                          row_names = F)
    status_UNESCO_reuse[, 1] <- NULL
    w_d <- which(duplicated(status_UNESCO_reuse$`Language Code`))
    if (length(w_d) > 0) {
      status_UNESCO_reuse <- status_UNESCO_reuse[-w_d, ]
    }
    
    incProgress(10/12, detail = "Ethnologue Status - Re-use")
    status_Ethnologue_reuse <- get_WDCM_table(params$general$publishedDataDir,
                                              URLencode('WD_Vis_EthnologueLanguageStatus_ItemReuse.csv'),
                                              row_names = F)
    status_Ethnologue_reuse[, 1] <- NULL
    
    
    # - fetch languagesCount as dataSet
    incProgress(11/12, detail = "Language counts.")
    dataSet <- get_WDCM_table(params$general$publishedDataDir,
                              URLencode('wd_languages_count.csv'),
                              row_names = F)
    dataSet[, 1] <- NULL
    dataSet$avg_reuse_per_item <- dataSet$reuse/dataSet$num_items_reused
    dataSet$prop_items_used <- dataSet$num_items_reused/dataSet$item_count
    
    # - update string
    incProgress(12/12, detail = "Update timestamp.")
    updateString <- get_WDCM_table(params$general$publishedDataDir,
                                   URLencode('WDLanguagesUpdateString.txt'),
                                   row_names = F)
    updateString <- colnames(updateString)
    
  }) ### --- Fetch data files (ENDS)
  
  ### - create ontology nodes and edges
  # - nodes
  n1 <- paste0(ontologyStructure$itemLabel, " (", ontologyStructure$item, ")")
  n2 <- paste0(ontologyStructure$superClassLabel, " (", ontologyStructure$superClass, ")")
  ns <- unique(c(n1, n2))
  nodes <- data.frame(id = 1:length(ns),
                      label = ns,
                      shadow = T,
                      stringsAsFactors = F)
  nodes$color <- 'lightgrey'
  nodes <- dplyr::left_join(nodes, 
                            dplyr::select(usedLanguages, 
                                          item_count, reuse, num_items_reused, 
                                          EthnologueLanguageStatus, UNESCOLanguageStatus, 
                                          numSitelinks, label),
                            by = "label")
  nodes <- nodes[!duplicated(nodes), ]
  wDuplicated <- nodes$id[which(duplicated(nodes$id))]
  if (length(wDuplicated) > 0) {
    fixDuplicates <- lapply(wDuplicated, function(x) {
      d <- nodes[nodes$id %in% x, ]
      d <- apply(d, 2, function(y) {
        paste(unique(y), collapse = ", ")
      })
      d <- as.data.frame(t(d))
      d
    })
    fixDuplicates <- data.table::rbindlist(fixDuplicates, fill = T, use.names = T)
    eliminate <- which(nodes$id %in% wDuplicated)
    if (length(eliminate > 0)) {
      nodes <- nodes[-eliminate, ]
      nodes <- rbind(nodes, fixDuplicates)
    }
  }
  nodes$item_count <- as.numeric(levels(nodes$item_count))[nodes$item_count]
  nodes$reuse <- as.numeric(levels(nodes$reuse))[nodes$reuse]
  nodes$num_items_reused <- as.numeric(levels(nodes$num_items_reused))[nodes$num_items_reused]
  nodes$numSitelinks <- as.numeric(levels(nodes$numSitelinks))[nodes$numSitelinks]
  nodes$title <- paste0('<p style="font-family:helvetica;font-size:75%;"><b>', nodes$label, '</b><br>',
                        ifelse(!is.na(nodes$item_count), paste0('Num.labels: ', nodes$item_count, '<br>'), ""),
                        ifelse(!is.na(nodes$reuse), paste0('Reuse statistic: ', nodes$reuse, '<br>'), ""),
                        ifelse(!is.na(nodes$num_items_reused), paste0('% of items reused: ', 
                                                                      round(as.numeric(nodes$num_items_reused)/as.numeric(nodes$item_count)*100, 2), '<br>'), ""),
                        ifelse(!is.na(nodes$numSitelinks), paste0('Num.sitelinks: ', nodes$numSitelinks, '<br>'), ""),
                        ifelse(!is.na(nodes$UNESCOLanguageStatus), paste0('UNESCO status: ', nodes$UNESCOLanguageStatus, '<br>'), ""),
                        ifelse(!is.na(nodes$EthnologueLanguageStatus), paste0('Ethnologue status: ', nodes$EthnologueLanguageStatus, '.<br>'), ""),
                        '</p>')
  nodes$id <- 1:length(nodes$id)
  # - edges
  conceptsStruct <- data.frame(
    from = sapply(paste0(ontologyStructure$itemLabel, " (", ontologyStructure$item, ")"), function(x) {
      nodes$id[which(nodes$label %in% x)]
    }),
    to = sapply(paste0(ontologyStructure$superClassLabel, " (", ontologyStructure$superClass, ")"), function(x) {
      nodes$id[which(nodes$label %in% x)]
    }),
    arrows = 'to',
    label = ontologyStructure$relation,
    dashes = ifelse(ontologyStructure$relation == 'P361', T, F),
    font.size = 10,
    stringsAsFactors = F)
  
  ### --- DATA (ENDS)
  
  ### ------------------------------------------
  ### --- Update String
  ### ------------------------------------------
  
  ### --- GENERAL: Update String
  output$updateString <- renderText({
    paste0('<p style="font-size:80%;"align="right"><b>', updateString, '</b></p>')
  })
  
  output$average_labels <- renderText({
    paste0('<p style="font-size:120%;"align="right"><b>',
           "Avg. labels/item: ",
           avg_labels, '</b></p>')
  })
  output$average_descriptions <- renderText({
    paste0('<p style="font-size:120%;"align="right"><b>',
           "Avg. descriptions/item: ",
           avg_descriptions, '</b></p>')
  })
  output$average_aliases <- renderText({
    paste0('<p style="font-size:120%;"align="right"><b>',
           "Avg. aliases/item: ",
           avg_aliases, '</b></p>')
  })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel datamodel
  ### ------------------------------------------
  
  ### --- EVERYTHING
  
  output$datamodelLabels <- dygraphs::renderDygraph({
    data <- dplyr::select(labelsEVERYTHING, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Labels") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "blue",
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
  
  output$datamodelAliases <- dygraphs::renderDygraph({
    data <- dplyr::select(aliasesEVERYTHING, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Aliases") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "orange",
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
  
  output$datamodelDescriptions <- dygraphs::renderDygraph({
    data <- dplyr::select(descriptionsEVERYTHING, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Descriptions") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "red",
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
  
  ### --- ASTRONOMY
  
  output$datamodelLabels_ASTRONOMY <- dygraphs::renderDygraph({
    data <- dplyr::select(labels_ASTRONOMY, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Labels") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "blue",
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
  
  output$datamodelAliases_ASTRONOMY <- dygraphs::renderDygraph({
    data <- dplyr::select(aliases_ASTRONOMY, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Aliases") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "orange",
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
  
  output$datamodelDescriptions_ASTRONOMY <- dygraphs::renderDygraph({
    data <- dplyr::select(descriptions_ASTRONOMY, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Descriptions") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "red",
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
  
  ### --- SCIENTIFIC PAPERS
  
  output$datamodelLabels_SCIENTIFICPAPERS <- dygraphs::renderDygraph({
    data <- dplyr::select(labels_SCIENTIFICPAPERS, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Labels") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "blue",
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
  
  output$datamodelAliases_SCIENTIFICPAPERS <- dygraphs::renderDygraph({
    data <- dplyr::select(aliases_SCIENTIFICPAPERS, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Aliases") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "orange",
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
  
  output$datamodelDescriptions_SCIENTIFICPAPERS <- dygraphs::renderDygraph({
    data <- dplyr::select(descriptions_SCIENTIFICPAPERS, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Descriptions") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "red",
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
  
  ### --- EVERYTHING MINUS (ASTRONOMY + SCIENTIFIC PAPERS)
  
  output$datamodelLabels_EVERYTHINGMINUS <- dygraphs::renderDygraph({
    data <- dplyr::select(labels_EVERYTHINGMINUS, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Labels") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "blue",
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
  
  output$datamodelAliases_EVERYTHINGMINUS <- dygraphs::renderDygraph({
    data <- dplyr::select(aliases_EVERYTHINGMINUS, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Aliases") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "orange",
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
  
  output$datamodelDescriptions_EVERYTHINGMINUS <- dygraphs::renderDygraph({
    data <- dplyr::select(descriptions_EVERYTHINGMINUS, 
                          snapshot,
                          average)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Descriptions") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(colors = "red",
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
  
  
  ### --- SELECTED LANGUAGE
  
  # - select language for dataModelLang_ dygraphs
  updateSelectizeInput(session,
                       'languages',
                       choices = unique(labels$language),
                       selected = 'en',
                       server = TRUE)
  
  output$dataModelLang_Labels <- dygraphs::renderDygraph({
    data <- labels %>% 
      dplyr::filter(language %in% input$languages) %>%
      dplyr::select(snapshot, count)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Labels") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(stackedGraph = T, 
                colors = "blue",
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
  
  output$dataModelLang_Aliases <- dygraphs::renderDygraph({
    data <- aliases %>% 
      dplyr::filter(language %in% input$languages) %>%
      dplyr::select(snapshot, count)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Aliases") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(stackedGraph = T, 
                colors = "orange",
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
  
  output$dataModelLang_Descriptions <- dygraphs::renderDygraph({
    data <- descriptions %>% 
      dplyr::filter(language %in% input$languages) %>%
      dplyr::select(snapshot, count)
    ix <- as.Date(data$snapshot)
    data$snapshot <- NULL
    data <- xts::xts(data, order.by = ix)
    dygraphs::dygraph(data, main = "Descriptions") %>% 
      dygraphs::dyLegend(show = "follow", hideOnMouseOut = TRUE) %>% 
      dygraphs::dyOptions(stackedGraph = T, 
                colors = "red",
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
  
  ### ------------------------------------------
  ### --- TAB: tabPanel ontology
  ### ------------------------------------------
  
  output$ontologyGraph <- plotly::renderPlotly({
    
    withProgress(message = 'Generating Network.', detail = "Please be patient.", value = 0, {
      
      incProgress(1/7, detail = "Prepare data structures.")
      
      oS <- data.frame(Outgoing = paste0(ontologyStructure$itemLabel, " (", ontologyStructure$item, ")"),
                       Incoming = paste0(ontologyStructure$superClassLabel, " (", ontologyStructure$superClass, ")"),
                       stringsAsFactors = F)
      
      idNet <- data.frame(from = oS$Outgoing,
                          to = oS$Incoming,
                          stringsAsFactors = F)
      idNet <- igraph::graph.data.frame(idNet,
                                vertices = NULL,
                                directed = T)
      
      # - Layout
      incProgress(2/7, detail = "Rendering graph. Please be patient.")
      L <- igraph::layout_with_kk(idNet, dim = 2)
      L <- as.data.frame(L)
      
      # - Attributes
      vs <- igraph::V(idNet)
      L$name <- vs$name
      es <- as.data.frame(igraph::get.edgelist(idNet))
      Nv <- length(vs)
      Ne <- dim(es)[1]
      Xn <- L[,1]
      Yn <- L[,2]
      vsnames <- data.frame(id = vs$name, 
                            stringsAsFactors = F)
      vsnames_count <- c(paste0(ontologyStructure$itemLabel, " (", ontologyStructure$item, ")"),
                         paste0(ontologyStructure$superClassLabel, " (", ontologyStructure$superClass, ")"))
      vsnames_count <- as.data.frame(table(vsnames_count))
      vsnames <- dplyr::left_join(vsnames, vsnames_count, 
                                  by = c("id" = "vsnames_count"))
      ul <- dplyr::select(usedLanguages, 
                          label, 
                          item_count,
                          reuse,
                          num_items_reused, 
                          numSitelinks)
      w_dul <- which(duplicated(ul$label))
      if (length(w_dul) > 0) {
        ul <- ul[-w_dul, ]
      }
      vsnames <- dplyr::left_join(vsnames,
                                  ul, 
                                  by = c("id" = "label"))
      
      # - {plotly}
      incProgress(3/7, detail = "Rendering graph. Please be patient.")
      network <- plotly::plot_ly(x = ~Xn, 
                         y = ~Yn, 
                         mode = "markers", 
                         text = paste0(vsnames$id, "<br><b>Relations:</b> ", vsnames$Freq, "<br>", 
                                       ifelse(!is.na(vsnames$item_count), paste0('Num.labels: ', vsnames$item_count, '<br>'), ""),
                                       ifelse(!is.na(vsnames$reuse), paste0('Reuse statistic: ', vsnames$reuse, '<br>'), ""),
                                       ifelse(!is.na(vsnames$num_items_reused), paste0('% of items reused: ', 
                                                                                       round(as.numeric(vsnames$num_items_reused)/as.numeric(vsnames$item_count)*100, 2), '<br>'), ""),
                                       ifelse(!is.na(vsnames$numSitelinks), paste0('Num.sitelinks: ', vsnames$numSitelinks, '.'), "")
                         ), 
                         size = log(vsnames$Freq),
                         sizes = c(10, 300),
                         hoverinfo = "text")
      edge_shapes <- list()
      incProgress(4/7, detail = "Rendering graph. Please be patient.")
      for (i in 1:Ne) {
        v0 <- es[i, ]$V1
        v1 <- es[i, ]$V2
        edge_shape = list(
          type = "line",
          line = list(color = "#030303", width = 0.3),
          x0 = Xn[which(L$name == v0)],
          y0 = Yn[which(L$name == v0)],
          x1 = Xn[which(L$name == v1)],
          y1 = Yn[which(L$name == v1)]
        )
        edge_shapes[[i]] <- edge_shape
      }
      axis <- list(title = "", 
                   showgrid = FALSE, 
                   showticklabels = FALSE, 
                   zeroline = FALSE)
      incProgress(5/7, detail = "Rendering graph. Please be patient.")
      p <- plotly::layout(
        network,
        shapes = edge_shapes,
        xaxis = axis,
        yaxis = axis
      )
      incProgress(6/7, detail = "Rendering graph. Please be patient.")
      plotly::ggplotly(p) %>%
        plotly::config(displayModeBar = TRUE,
                       displaylogo = FALSE,
                       collaborate = FALSE,
                       modeBarButtonsToRemove = list(
                         'lasso2d',
                         'select2d',
                         'toggleSpikelines',
                         'hoverClosestCartesian',
                         'hoverCompareCartesian',
                         'autoScale2d'
                       ))
    })
  })
  
  
  ### ------------------------------------------
  ### --- TAB: tabPanel languageclass
  ### ------------------------------------------
  
  ### --- SELECT: update select 'selectCategory'
  updateSelectizeInput(session,
                       'selectLanguage',
                       "Select Language/Class:",
                       choices = ns,
                       selected = ns[round(runif(1, 1, length(ns)))],
                       server = TRUE)
  
  ### --- visNetwork::visNetwork: languageClassGraph w. P31/P279/P361
  # - output$languageClassGraph
  output$languageClassGraph <- visNetwork::renderVisNetwork({
    # - what is selected?
    selectedNode <- nodes$id[which(nodes$label %in% input$selectLanguage)]
    # - edges
    graph_edges <- conceptsStruct %>% 
      dplyr::filter(from %in% selectedNode | to %in% selectedNode)
    # - nodes
    graph_nodes <- nodes %>% 
      dplyr::filter(id %in% unique(c(graph_edges$from, graph_edges$to)))
    # - repeat for links of selected:
    graph_edges2 <- conceptsStruct %>% 
      dplyr::filter(from %in% unique(graph_nodes$id))
    graph_nodes2 <- nodes %>% 
      dplyr::filter(id %in% unique(c(graph_edges2$from, graph_edges2$to)))
    # - bind
    graph_nodes <- rbind(graph_nodes, graph_nodes2)
    graph_nodes <- graph_nodes[!duplicated(graph_nodes), ]
    graph_edges <- rbind(graph_edges, graph_edges2)
    graph_edges <- graph_edges[!duplicated(graph_edges), ]
    # - color selected:
    graph_nodes$color[graph_nodes$id == selectedNode] <- 'blue'
    # - render network:
    visNetwork::visNetwork(nodes = graph_nodes,
               edges = graph_edges,
               width = "2000px",
               height = "1500px") %>%
      visNetwork::visPhysics(stabilization = FALSE) %>% 
      visNetwork::visNodes(scaling = list(min = 20, max = 65))
  }) %>% withProgress(message = 'Generating graph.',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  
  ### ------------------------------------------
  ### --- TAB: tabPanel similarity
  ### ------------------------------------------
  
  output$similarityGraph <- plotly::renderPlotly({
    
    withProgress(message = 'Generating Network.', detail = "Please be patient.", value = 0, {
      
      incProgress(1/7, detail = "Prepare data structures.")
      
      # - similarity matrix
      simMatrix <- as.matrix(simData[, 1:(dim(simData)[1])])
      rownames(simMatrix) <- colnames(simMatrix)
      nns <- vector(mode = "list", length = dim(simMatrix)[1])
      for (i in 1:(dim(simMatrix)[1])) {
        lan <- simMatrix[i, ]
        from = names(lan)[i]
        ix <- setdiff(1:length(lan), i)
        lan <- lan[ix]
        w <- which.max(lan)
        nns[[i]] <- data.frame(outgoing = from, 
                               incoming = names(lan)[w], 
                               stringsAsFactors = F)
      }
      nns <- data.table::rbindlist(nns)
      
      oS <- data.frame(Outgoing = nns$outgoing,
                       Incoming = nns$incoming,
                       stringsAsFactors = F)
      
      idNet <- data.frame(from = oS$Outgoing,
                          to = oS$Incoming,
                          stringsAsFactors = F)
      idNet <- igraph::graph.data.frame(idNet,
                                vertices = NULL,
                                directed = T)
      
      # - colors
      cc <- igraph::cluster_edge_betweenness(igraph::as.undirected(idNet)) 
      cc <- cc$membership
      cc <- colors()[cc]
      
      
      # - Layout
      incProgress(2/7, detail = "Rendering graph. Please be patient.")
      L <- igraph::layout_with_kk(idNet, dim = 2)
      L <- as.data.frame(L)
      
      # - Attributes
      vs <- igraph::V(idNet)
      L$name <- vs$name
      es <- as.data.frame(igraph::get.edgelist(idNet))
      Nv <- length(vs)
      Ne <- dim(es)[1]
      Xn <- L[,1]
      Yn <- L[,2]
      vsnames <- data.frame(id = vs$name, 
                            stringsAsFactors = F)
      ul <- dplyr::select(usedLanguages,
                          language, item_count, reuse, num_items_reused,
                          numSitelinks, label)
      ul <- ul[!duplicated(ul), ]
      # - fix duplicates in ul
      wDuplicated <- ul$language[which(duplicated(ul$language))]
      if (length(wDuplicated) > 0) {
        fixDuplicates <- lapply(wDuplicated, function(x) {
          d <- ul[ul$language %in% x, ]
          d <- apply(d, 2, function(y) {
            paste(unique(y), collapse = ", ")
          })
          d <- as.data.frame(t(d))
          d
        })
      }
      fixDuplicates <- data.table::rbindlist(fixDuplicates, fill = T, use.names = T)
      eliminate <- which(ul$language %in% wDuplicated)
      if (length(eliminate > 0)) {
        ul <- ul[-eliminate, ]
        ul <- rbind(ul, fixDuplicates)
      }
      vsnames <- dplyr::left_join(vsnames,
                                  ul, 
                                  by = c("id" = "language"))
      vsnames$item_count <- as.numeric(levels(vsnames$item_count))[vsnames$item_count]
      vsnames$reuse <- as.numeric(levels(vsnames$reuse))[vsnames$reuse]
      vsnames$num_items_reused <- as.numeric(levels(vsnames$num_items_reused))[vsnames$num_items_reused]
      vsnames$numSitelinks <- as.numeric(levels(vsnames$numSitelinks))[vsnames$numSitelinks]
      # - {plotly}
      incProgress(3/7, detail = "Rendering graph. Please be patient.")
      network <- plotly::plot_ly(x = ~Xn, 
                         y = ~Yn, 
                         mode = "markers", 
                         text = paste0(vsnames$id, "<br>", 
                                       vsnames$label, "<br>",
                                       ifelse(!is.na(vsnames$item_count), paste0('Num.labels: ', vsnames$item_count, '<br>'), ""),
                                       ifelse(!is.na(vsnames$reuse), paste0('Reuse statistic: ', vsnames$reuse, '<br>'), ""),
                                       ifelse(!is.na(vsnames$num_items_reused), paste0('% of items reused: ', 
                                                                                       round(as.numeric(vsnames$num_items_reused)/as.numeric(vsnames$item_count)*100, 2), '<br>'), ""),
                                       ifelse(!is.na(vsnames$numSitelinks), paste0('Num.sitelinks: ', vsnames$numSitelinks, '.'), "")
                         ), 
                         size = ifelse(!is.na(vsnames$item_count), vsnames$item_count, 10),
                         sizes = c(10, 300),
                         color = cc,
                         hoverinfo = "text")
      edge_shapes <- list()
      incProgress(4/7, detail = "Rendering graph. Please be patient.")
      for (i in 1:Ne) {
        v0 <- es[i, ]$V1
        v1 <- es[i, ]$V2
        edge_shape = list(
          type = "line",
          line = list(color = "#030303", width = 0.3),
          x0 = Xn[which(L$name == v0)],
          y0 = Yn[which(L$name == v0)],
          x1 = Xn[which(L$name == v1)],
          y1 = Yn[which(L$name == v1)]
        )
        edge_shapes[[i]] <- edge_shape
      }
      axis <- list(title = "", 
                   showgrid = FALSE, 
                   showticklabels = FALSE, 
                   zeroline = FALSE)
      incProgress(5/7, detail = "Rendering graph. Please be patient.")
      p <- plotly::layout(
        network,
        shapes = edge_shapes,
        xaxis = axis,
        yaxis = axis
      )
      incProgress(6/7, detail = "Rendering graph. Please be patient.")
      plotly::ggplotly(p) %>%
        plotly::config(displayModeBar = TRUE,
                       displaylogo = FALSE,
                       collaborate = FALSE,
                       modeBarButtonsToRemove = list(
                         'lasso2d',
                         'select2d',
                         'toggleSpikelines',
                         'hoverClosestCartesian',
                         'hoverCompareCartesian',
                         'autoScale2d'
                       ))  %>%
        plotly::hide_legend()
    })
  })
  
  
  output$similarityGraph_Static <- renderPlot({
    
    withProgress(message = 'Generating Network.', detail = "Please be patient.", value = 0, {
      
      incProgress(1/3, detail = "Prepare data structures.")
      
      # - similarity matrix
      simMatrix <- as.matrix(simData[, 1:(dim(simData)[1])])
      rownames(simMatrix) <- colnames(simMatrix)
      nns <- vector(mode = "list", length = dim(simMatrix)[1])
      for (i in 1:(dim(simMatrix)[1])) {
        lan <- simMatrix[i, ]
        from = names(lan)[i]
        ix <- setdiff(1:length(lan), i)
        lan <- lan[ix]
        w <- which.max(lan)
        nns[[i]] <- data.frame(outgoing = from, 
                               incoming = names(lan)[w], 
                               stringsAsFactors = F)
      }
      nns <- data.table::rbindlist(nns)
      
      # - Visualize w. {igraph}
      connected <- igraph::graph.data.frame(nns[, c('outgoing', 'incoming')], directed = T)
      
      incProgress(2/3, detail = "Compute graph layout.")
      
      # - layout
      cc <- igraph::cluster_edge_betweenness(igraph::as.undirected(connected)) 
      
      # - plot w. {igraph}
      
      incProgress(3/3, detail = "Render graphics.")
      
      par(mai=c(rep(0,4)))
      plot(cc, connected,
           edge.width = .75,
           edge.color = "grey40",
           edge.arrow.size = 0.1,
           edge.curved = 0.5,
           vertex.color = NULL,
           vertex.label.color = "black",
           vertex.label.family = "sans",
           # vertex.label.font = "Helvetica",
           vertex.label.cex = 1,
           vertex.size = 0)
      
    })
  })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Status
  ### ------------------------------------------
  
  # - file: WD_Vis_UNESCO Language Status_Sitelinks.csv
  output$status_UNESCO_Sitelinks <- plotly::renderPlotly({
    pFrame <- status_UNESCO_Sitelinks
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = `UNESCO Language Status`,
                    y = Sitelinks,
                    color = `UNESCO Language Status`,
                    label = Language,
                    size = Sitelinks,
                    alpha = Sitelinks)) +
      ggplot2::geom_jitter(width = .1, fill = "white") +
      ggplot2::ggtitle("UNESCO Language Status and Sitelinks") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "size")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: WD_Vis_EthnologueLanguageStatus_Sitelinks.csv
  output$status_Ethnologue_Sitelinks <- plotly::renderPlotly({
    pFrame <- status_Ethnologue_Sitelinks
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = `Ethnologue Language Status`,
                    y = Sitelinks,
                    color = `Ethnologue Language Status`,
                    label = Language,
                    size = Sitelinks,
                    alpha = Sitelinks)) +
      ggplot2::geom_jitter(width = .1, fill = "white") +
      ggplot2::ggtitle("Ethnologue Language Status and Sitelinks") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "size")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: WD_Vis_UNESCO Language Status_NumItems.csv
  output$status_UNESCO_numItems <- plotly::renderPlotly({
    pFrame <- status_UNESCO_numItems
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = `UNESCO Language Status`,
                    y = log(Labels),
                    color = `UNESCO Language Status`,
                    label = Language,
                    size = Labels,
                    alpha = log(Labels))) +
      ggplot2::geom_jitter(width = .1, fill = "white") +
      ggplot2::ggtitle("UNESCO Language Status and Labels") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "size")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: WD_Vis_Ethnologue Language Status_NumItems.csv
  output$status_Ethnologue_numItems <- plotly::renderPlotly({
    pFrame <- status_Ethnologue_numItems
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = `Ethnologue Language Status`,
                    y = log(Labels),
                    color = `Ethnologue Language Status`,
                    size = Labels,
                    label = Language,
                    alpha = log(Labels))) +
      ggplot2::geom_jitter(width = .1, fill = "white") +
      ggplot2::ggtitle("Ethnologue Language Status and Labels") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "size")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: WD_Vis_UNESCO Language Status_ItemReuse.csv
  output$status_UNESCO_reuse <- plotly::renderPlotly({
    pFrame <- status_UNESCO_reuse
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = `UNESCO Language Status`,
                    y = log(Reuse),
                    text = paste0("WDCM Reuse statistic: ", Reuse),
                    color = `UNESCO Language Status`,
                    size = log(`Items Reused`),
                    label = Language,
                    alpha = log(`Items Reused`)/max(log(`Items Reused`)))) +
      ggplot2::geom_jitter(width = .1, fill = "white") +
      ggplot2::ggtitle("UNESCO Language Status and Item Reuse") + 
      ggplot2::scale_size_area(max_size = 4) +
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "text")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: WD_Vis_Ethnologue Language Status_ItemReuse.csv
  output$status_Ethnologue_reuse <- plotly::renderPlotly({
    pFrame <- status_Ethnologue_reuse
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = `Ethnologue Language Status`,
                    y = log(Reuse),
                    text = paste0("WDCM Reuse statistic: ", Reuse),
                    color = `Ethnologue Language Status`,
                    size = log(`Items Reused`),
                    label = Language,
                    alpha = log(`Items Reused`/Items)/max(log(`Items Reused`/Items)))) +
      ggplot2::geom_jitter(width = .1, fill = "white") +
      ggplot2::ggtitle("Ethnologue Language Status and Item Reuse") + 
      ggplot2::scale_size_area(max_size = 4) +
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "text")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Usage
  ### ------------------------------------------
  
  # - file: wd_languages_count.csv
  output$labels_Reuse <- plotly::renderPlotly({
    pFrame <- dataSet
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = item_count,
                    y = reuse,
                    text = paste0("WDCM Reuse statistic: ", reuse),
                    color = reuse,
                    size = reuse,
                    label = language,
                    alpha = reuse)) + 
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(labels = scales::comma) + 
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::scale_colour_gradient(low = "tomato", high = "firebrick", aesthetics = "colour") + 
      ggplot2::scale_size_area(max_size = 4) +
      ggplot2::xlab("Number of items per language") + 
      ggplot2::ylab("Total item reuse per language") +
      ggplot2::theme_minimal() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::ggtitle("Item Count vs WDCM Reuse Statistics") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "text")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: wd_languages_count.csv
  output$log_labels_Reuse <- plotly::renderPlotly({
    pFrame <- dataSet
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = log(item_count),
                    y = log(reuse),
                    label = language,
                    text = paste0("WDCM Reuse statistic: ", reuse),
                    color = reuse,
                    size = reuse,
                    alpha = reuse)) + 
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(labels = scales::comma) + 
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::scale_colour_gradient(low = "tomato", high = "firebrick", aesthetics = "colour") + 
      ggplot2::scale_size_area(max_size = 4) +
      ggplot2::xlab("Number of items per language (log)") + 
      ggplot2::ylab("Total item reuse per language (log)") +
      ggplot2::theme_minimal() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::ggtitle("Item Count vs WDCM Reuse Statistics (log space)") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "text")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: wd_languages_count.csv
  output$itemCount_avgReuse <- plotly::renderPlotly({
    pFrame <- dataSet
    g <- ggplot2::ggplot(data = pFrame,
                ggplot2::aes(x = log(item_count),
                    y = log(avg_reuse_per_item),
                    label = language,
                    text = paste0("avg. WDCM Reuse statistic: ", round(avg_reuse_per_item, 2)),
                    color = avg_reuse_per_item,
                    size = avg_reuse_per_item,
                    alpha = log(avg_reuse_per_item))) + 
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(labels = scales::comma) + 
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::scale_colour_gradient(low = "lightblue", high = "cadetblue4", aesthetics = "colour") + 
      ggplot2::scale_size_area(max_size = 4) +
      ggplot2::xlab("Number of items per language (log)") + 
      ggplot2::ylab("Average item reuse per language (log)") +
      ggplot2::theme_minimal() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::ggtitle("Item Count vs Average Reuse (log space)") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "text")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
  # - file: wd_languages_count.csv
  output$itemCount_propReused <- plotly::renderPlotly({
    pFrame <- dataSet
    g <- ggplot2::ggplot(data = dataSet,
                ggplot2::aes(x = log(item_count),
                    y = prop_items_used*100,
                    label = language,
                    text = paste0("% of items reused: ", round(prop_items_used*100, 2)),
                    color = prop_items_used,
                    size = prop_items_used,
                    alpha = prop_items_used)) + 
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(labels = scales::comma) + 
      ggplot2::scale_y_continuous(labels = scales::comma, limits=c(0, 100)) + 
      ggplot2::scale_colour_gradient(low = "yellow", high = "orange", aesthetics = "colour") + 
      ggplot2::scale_size_area(max_size = 4) +
      ggplot2::xlab("Number of items per language (log)") + 
      ggplot2::ylab("% of items reused per language") +
      ggplot2::theme_minimal() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::ggtitle("Item Count vs % of Items Reused (log space)") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = .95, size = 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = .95, size = 11)) + 
      ggplot2::theme(axis.title.y =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(axis.title.x =  ggplot2::element_text(hjust = .5, size = 11)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("label", "x", "text")) %>% 
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     collaborate = FALSE,
                     modeBarButtonsToRemove = list(
                       'lasso2d',
                       'select2d',
                       'toggleSpikelines',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'autoScale2d'
                     ))
  })
  
}
