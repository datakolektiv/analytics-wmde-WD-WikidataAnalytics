### ---------------------------------------------------------------------------
### --- WDCM (T)itles
### --- Version 1.0.0
### --- 2020.
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
    'https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/Titles/'
  
  ### --- Init Dashboard: download data
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    
    ### --- Update string:
    incProgress(1/5, detail = "Update timestamp.")
    updateString <- readLines(paste0(publicDir, 
                                     'wikipediaTitlesUpdateString.txt')
    )
    updateString <- paste0(updateString, ' UTC')
    
    ### --- List files from public dir
    incProgress(2/5, detail = "List files.")
    filenames <- httr::GET(publicDir)
    filenames <- rawToChar(filenames$content)
    filenames <- stringr::str_extract_all(filenames, ">([[:alnum:]]|[[:punct:]]|\\s)+(\\.csv|\\.Rds)")[[1]]
    filenames <- gsub("^>", "", filenames)
    
    ### --- Constants
    # - WDCM categories
    incProgress(3/5, detail = "Categories.")
    lF <- filenames[grepl("_themeDescription", filenames)]
    wdcmCategories <- gsub("wdcmdo_", "", lF)
    wdcmCategories <- gsub("_themeDescription.csv", "", wdcmCategories)
    # - WDCM wikies
    incProgress(4/5, detail = "Wikies.")
    wdcmWikies <- read.csv(paste0(publicDir, "Wikipedia_wdcmSUsage_CategoryOverview.csv"),
                           header = T,
                           check.names = F,
                           row.names = 1,
                           stringsAsFactors = F)
    wdcmSelectedWikies <- rownames(wdcmWikies)
    # - wiki x topics matrices
    incProgress(5/5, detail = "Wiki x Topic matrices.")
    lF <- filenames[grepl("_wikitopic", filenames)]
    wikiTopic <- vector(mode = "list", length = length(lF))
    for (i in 1:length(lF)) {
      wikiTopic[[i]] <- read.csv(paste0(publicDir, URLencode(lF[i])),
                                 header = T,
                                 row.names = 1,
                                 check.names = F,
                                 stringsAsFactors = F)
    }
    names(wikiTopic) <- sapply(lF, function(x) {
      strsplit(x, split = "_")[[1]][1]
    })
    
  }) # - end download data
  
  ### --------------------------------------------------------------------
  ### --- TAB: Category View
  ### --------------------------------------------------------------------
  
  ### --- GENERAL: Update String
  output$updateString <- renderText({
    paste0('<p style="font-size:80%;"align="right"><b>', updateString, '</b></p>')
  })
  
  ### --- SELECT: update select 'selectCategory'
  updateSelectizeInput(session,
                       'selectCategory',
                       choices = wdcmCategories,
                       selected = wdcmCategories[1],
                       server = TRUE)
  
  ### ---  when user selects a category
  selectedCategory <- reactive({input$selectCategory})
  
  ### ------ 
  ### --- SubTAB: Theme Description
  ### ------ 
  
  ### --- reactive themeDescription
  themeDescription <- eventReactive(input$selectCategory,
                                    {
                                      lF <- filenames[grepl("_themeDescription", filenames)]
                                      tD <- read.csv(paste0(publicDir, URLencode(lF[which(grepl(input$selectCategory, lF))])),
                                                     header = T,
                                                     check.names = F,
                                                     row.names = 1,
                                                     stringsAsFactors = F)
                                      
                                      colnames(tD)[2] <- 'Distinctive Classes'
                                      colnames(tD)[3] <- 'Typical Items'
                                      tD$`Distinctive Classes` <- gsub('"+', '"', tD$`Distinctive Classes`)
                                      tD$`Typical Items` <- gsub('"+', '"', tD$`Typical Items`)
                                      return(tD)
                                    })
  
  ### --- output$themeDescription
  output$themeDescription_DT <- DT::renderDataTable({
    DT::datatable(themeDescription(),
                  escape = F,
                  selection = 'none',
                  options = list(
                    pageLength = 20,
                    dom = 't',
                    ordering = F,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### ------ 
  ### --- SubTAB: Themes/Items
  ### ------ 
  
  ### --- SELECT: update select 'selectTheme_Items'
  output$selectTheme_Items <-
    renderUI({
      if ((is.null(themeDescription())) | (length(themeDescription()) == 0)) {
        selectizeInput(inputId = 'selectTheme_Items',
                       label = "Select theme:",
                       choices = NULL,
                       selected = NULL)
      } else {
        cH <- themeDescription()$Theme
        selectizeInput(inputId = 'selectTheme_Items',
                       label = "Select theme:",
                       choices = cH,
                       selected = cH[1])
      }
    })
  
  ### --- OUTPUT output$themeDistributionItems_SelectedCategory
  output$themeItems_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  ### --- OUTPUT output$themeDistributionItems
  output$themeDistributionItems <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
      lF <- filenames[grepl('itemtopic.csv', filenames)]
      plotFrame <- read.csv(paste0(publicDir, URLencode(lF[which(grepl(selectedCategory(), lF))])),
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
      
      incProgress(0.33, detail = "Computing")
      
      plotFrame$eu_entity_id <- NULL
      colnames(plotFrame)[which(grepl("^top", colnames(plotFrame)))] <- 
        gsub("topic", "Theme ", colnames(plotFrame)[which(grepl("^top", colnames(plotFrame)))])
      plotFrame <- dplyr::select(plotFrame,
                                 as.character(input$selectTheme_Items), 'eu_label')
      colnames(plotFrame)[1] <- 'Score'
      plotFrame <- dplyr::arrange(plotFrame, desc(Score)) %>% 
        head(50)
      plotFrame$eu_label <- factor(plotFrame$eu_label, 
                                   levels = plotFrame$eu_label[order(plotFrame$Score)])
      incProgress(0.66, detail = "Generating Chart")
      
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = eu_label, 
                                                   y = Score, 
                                                   label = eu_label)) +
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") +
        ggplot2::geom_point(size = 1, color = "white") +
        ggrepel::geom_label_repel(size = 3, 
                                  segment.size = .25, 
                                  show.legend = FALSE) +
        ggplot2::xlab("\nItem") + ggplot2::ylab("Item Weight (0-1)\n") +
        ggplot2::ggtitle("Top Items in Semantic Theme") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, vjust = -1)) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold")) + 
        ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0.5, size = 13))
      return(g)
      incProgress(1, detail = "Done")
    })
    
  })
  
  ### --- OUTPUT output$themeDistributionItems_interactive
  output$themeDistributionItems_interactive <- plotly::renderPlotly({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
      lF <- filenames[grepl('itemtopic.csv', filenames)]
      plotFrame <- read.csv(paste0(publicDir, 
                                   URLencode(lF[which(grepl(selectedCategory(), lF))])),
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
      
      incProgress(0.33, detail = "Computing")
      
      plotFrame$eu_entity_id <- NULL
      colnames(plotFrame)[which(grepl("^top", colnames(plotFrame)))] <- 
        gsub("topic", "Theme ", colnames(plotFrame)[which(grepl("^top", colnames(plotFrame)))])
      plotFrame <- dplyr::select(plotFrame,
                                 as.character(input$selectTheme_Items), 'eu_label')
      colnames(plotFrame)[1] <- 'Score'
      plotFrame <- dplyr::arrange(plotFrame, desc(Score)) %>% 
        head(50)
      plotFrame$eu_label <- factor(plotFrame$eu_label, 
                                   levels = plotFrame$eu_label[order(plotFrame$Score)])
      incProgress(0.66, detail = "Generating Chart")
      
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = eu_label, 
                                                   y = Score, 
                                                   label = eu_label)) +
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") +
        ggplot2::geom_text(size = 3, 
                           show.legend = FALSE) +
        ggplot2::xlab("\nItem") + ggplot2::ylab("Item Weight (0-1)\n") +
        ggplot2::ggtitle("Top Items in Semantic Theme") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, vjust = -1)) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold")) + 
        ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0.5, size = 13))
      incProgress(1, detail = "Done")
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T)
    })
    
  })
  
  ### ------ 
  ### --- SubTAB: Distribution: Items
  ### ------
  
  ### --- OUTPUT output$themeDistributionItems_SelectedCategory
  output$themeDistributionItems_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  
  ### --- OUTPUT output$themeDistributionItems_Full
  output$themeDistributionItems_Full <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
      lF <- filenames[grepl('itemtopic.csv', filenames)]
      plotFrame <- read.csv(paste0(publicDir, URLencode(lF[which(grepl(selectedCategory(), lF))])),
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
      
      incProgress(0.33, detail = "Computing")
      
      plotFrame$eu_entity_id <- NULL
      plotFrame$eu_label <- NULL
      plotFrame <- plotFrame %>% 
        tidyr::gather(key = "Theme", 
                      value = "Score", 
                      dplyr::starts_with('topic'))
      plotFrame$Theme <- gsub("[[:alpha:]]+", "Theme ", plotFrame$Theme)
      plotFrame$Theme <- factor(plotFrame$Theme, 
                                levels = unique(plotFrame$Theme)[order(
                                  as.numeric(
                                    stringr::str_extract(unique(plotFrame$Theme), "[[:digit:]]+")
                                  )
                                )]
      )
      incProgress(0.66, detail = "Generating Chart")
      
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Score)) + 
        ggplot2::geom_histogram(bins = 50, fill = "deepskyblue") +
        ggplot2::facet_wrap(~ Theme, scales = "free") +  
        ggplot2::ylab("Number of items\n") + ggplot2::xlab("\nItem Weight (0-1)") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_x_continuous(labels = scales::comma) +
        ggplot2::facet_wrap(~Theme, ncol = 2, scales = 'free') + 
        ggplot2::ggtitle("The Distribution of Item Weight") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, hjust = 1)) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold")) + 
        ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0.5, size = 13))
      incProgress(1, detail = "Done")
      return(g)
      
    })
  })
  
  
  ### ------ 
  ### --- SubTAB: Theme/Projects
  ### ------ 
  
  ### --- SELECT: update select 'selectTheme_Project'
  output$selectTheme_Project <-
    renderUI({
      if ((is.null(themeDescription())) | (length(themeDescription()) == 0)) {
        selectizeInput(inputId = 'selectTheme_Project',
                       label = "Select theme:",
                       choices = NULL,
                       selected = NULL)
      } else {
        cH <- themeDescription()$Theme
        selectizeInput(inputId = 'selectTheme_Project',
                       label = "Select theme:",
                       choices = cH,
                       selected = cH[1])
      }
    })
  
  ### --- OUTPUT output$themeProjects_SelectedCategory
  output$themeProjects_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  ### --- OUTPUT output$themeDistributionProjects
  output$themeDistributionProjects <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
      lF <- filenames[grepl('themeWiki.csv', filenames)]
      plotFrame <- read.csv(paste0(publicDir, URLencode(lF[which(grepl(selectedCategory(), lF))])),
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
      
      incProgress(0.33, detail = "Computing")
      
      plotFrame <- dplyr::filter(plotFrame, 
                                 Theme %in% as.character(input$selectTheme_Project)) %>% 
        dplyr::arrange(desc(Score))
      plotFrame$Wikipedia <- factor(plotFrame$Wikipedia, 
                                    levels = plotFrame$Wikipedia[order(-plotFrame$Score)])
      
      incProgress(0.66, detail = "Generating Chart")
      
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Wikipedia, y = Score, label = Wikipedia)) + 
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") +
        ggplot2::geom_point(size = 1, color = "white") +
        ggrepel::geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
        ggplot2::ylab("Project Weight (0-1)\n") + ggplot2::xlab("\nWikipedia") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::ggtitle("Top Wikipedias in Semantic Theme") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold")) + 
        ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0.5, size = 13))
      incProgress(1, detail = "Done")
      return(g)
    })
  })
  
  ### --- OUTPUT output$themeDistributionProjects_interactive
  output$themeDistributionProjects_interactive <- plotly::renderPlotly({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
      lF <- filenames[grepl('themeWiki.csv', filenames)]
      plotFrame <- read.csv(paste0(publicDir, URLencode(lF[which(grepl(selectedCategory(), lF))])),
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
      
      incProgress(0.33, detail = "Computing")
      
      plotFrame <- dplyr::filter(plotFrame, 
                                 Theme %in% as.character(input$selectTheme_Project)) %>% 
        dplyr::arrange(desc(Score))
      plotFrame$Wikipedia <- factor(plotFrame$Wikipedia, 
                                    levels = plotFrame$Wikipedia[order(-plotFrame$Score)])
      
      incProgress(0.66, detail = "Generating Chart")
      
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Wikipedia, y = Score, label = Wikipedia)) + 
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") +
        ggplot2::geom_text(size = 3, show.legend = FALSE) +
        ggplot2::ylab("Project Weight (0-1)\n") + ggplot2::xlab("\nWikipedia") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::ggtitle("Top Wikipedias in Semantic Theme") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold")) + 
        ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0.5, size = 13))
      incProgress(1, detail = "Done")
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T)
    })
  })
  
  ### ------ 
  ### --- SubTAB: Distribution:Projects
  ### ------ 
  
  ### --- OUTPUT output$themeDistributionProjects_SelectedCategory
  output$themeDistributionProjects_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  ### --- OUTPUT output$themeDistributionProjects_Full
  output$themeDistributionProjects_Full <- renderPlot({
    
    withProgress(message = 'Generating Plot', detail = "Loading Data", value = 0, {
      
      lF <- filenames[grepl('wikitopic.csv', filenames)]
      plotFrame <- read.csv(paste0(publicDir, URLencode(lF[which(grepl(selectedCategory(), lF))])),
                            header = T,
                            check.names = F,
                            row.names = 1,
                            stringsAsFactors = F)
      
      incProgress(0.33, detail = "Computing")
      
      plotFrame <- plotFrame %>% 
        tidyr::gather(key = "Theme", 
                      value = "Score", 
                      dplyr::starts_with('topic'))
      plotFrame$Theme <- gsub("[[:alpha:]]+", "Theme ", plotFrame$Theme)
      plotFrame$Theme <- factor(plotFrame$Theme, 
                                levels = unique(plotFrame$Theme)[order(
                                  as.numeric(
                                    stringr::str_extract(unique(plotFrame$Theme), "[[:digit:]]+")
                                  )
                                )]
      )
      incProgress(0.66, detail = "Generating Chart")
      
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Score)) + 
        ggplot2::geom_histogram(bins = 50, fill = "deepskyblue") +
        ggplot2::facet_wrap(~ Theme, scales = "free") +  
        ggplot2::ylab("Number of Wikipedias\n") + ggplot2::xlab("\nProject Weight (0-1)") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_x_continuous(labels = scales::comma) +
        ggplot2::facet_wrap(~Theme, ncol = 2, scales = 'free') +
        ggplot2::ggtitle("The Distribution of Project Weight in Semantic Themes") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, hjust = 1)) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold")) + 
        ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0.5, size = 13))
      incProgress(1, detail = "Done")
      return(g)
    })
  })
  
  ### ------ 
  ### --- SubTAB: Items:Graph
  ### ------ 
  
  ### --- OUTPUT output$itemsGraph_SelectedCategory
  output$itemsGraph_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$itemsGraph
  output$itemsGraph <- visNetwork::renderVisNetwork({
    
    withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
      
      incProgress(1/3, detail = "Read nodes.")
      nodes <- read.csv(paste0(publicDir, URLencode(paste0(input$selectCategory, 
                                                           "_itemNodes.csv"))),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
      nodes$color <- NULL
      nodes$shape <- 'square'
      
      incProgress(2/3, detail = "Read edges.")
      edges <- read.csv(paste0(publicDir, URLencode(paste0(input$selectCategory, 
                                                           "_itemEdges.csv"))),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
      edges$width <- (edges$width - 2)/2
      
      incProgress(3/3, detail = "Render graph.")
      visNetwork::visNetwork(nodes = nodes,
                             edges = edges,
                             width = "100%",
                             height = "100%") %>%
        visNetwork::visEvents(type = "once",
                              startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
        visNetwork::visPhysics(enabled = T, maxVelocity = 1) %>% 
        visNetwork::visNodes(scaling = list(label = list(enabled = T))) %>% 
        visNetwork::visOptions(highlightNearest = TRUE, selectedBy = "label") 
      
    })
    
  })
  
  
  ### ------ 
  ### --- SubTAB: Projects:Graph
  ### ------ 
  
  ### --- OUTPUT output$projectsGraph_SelectedCategory
  output$projectsGraph_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$projectsGraph
  output$projectsGraph <- visNetwork::renderVisNetwork({
    
    withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
      
      incProgress(1/3, detail = "Read nodes.")
      nodes <- read.csv(paste0(publicDir, URLencode(paste0(input$selectCategory, 
                                                           "_projectNodes.csv"))),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
      nodes$shape <- 'square'
      nodes$color <- NULL
      
      incProgress(1/2, detail = "Read edges")
      edges <- read.csv(paste0(publicDir, URLencode(paste0(input$selectCategory, 
                                                           "_projectEdges.csv"))),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
      edges$width <- (edges$width - 2)/2
      nodes$color <- NULL
      
      incProgress(3/3, detail = "Render graph.")
      visNetwork::visNetwork(nodes = nodes,
                             edges = edges,
                             width = "100%",
                             height = "100%") %>%
        visNetwork::visEvents(type = "once",
                              startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
        visNetwork::visPhysics(enabled = T, maxVelocity = 1) %>% 
        visNetwork::visNodes(scaling = list(label = list(enabled = T))) %>% 
        visNetwork::visOptions(highlightNearest = TRUE, selectedBy = "label")
      
    })
    
  })
  
  ### ------ 
  ### --- SubTAB: Items:Hierarchy
  ### ------ 
  
  ### --- OUTPUT output$projectsHierarchy_SelectedCategory
  output$itemsHierarchy_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$output$itemsHierarchy
  output$itemsHierarchy <- renderPlot({
    
    cluster <- readRDS(gzcon(url(paste0(publicDir, 
                                        URLencode(paste0(input$selectCategory, 
                                                         "_itemClusters.Rds"))))))
    cluster$labels <- gsub("\\(Q[[:digit:]]+\\)", "", cluster$labels)
    plot(ape::as.phylo(cluster), type = "radial", tip.color = 'black',
         direction = "leftwards",
         edge.color = "deepskyblue4", edge.width = 1.6,
         cex = .75,
         lab4ut = 'axial',
         label.offset = .05, 
         no.margin = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### ------ 
  ### --- SubTAB: Projects:Hierarchy
  ### ------ 
  
  ### --- OUTPUT output$projectsHierarchy_SelectedCategory
  output$projectsHierarchy_SelectedCategory <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected category: </b>', input$selectCategory, '</p>')
  })
  
  # - output$projectsHierarchy
  output$projectsHierarchy <- renderPlot({
    
    cluster <- readRDS(gzcon(url(paste0(publicDir, 
                                        URLencode(paste0(input$selectCategory, 
                                                         "_projectClusters.Rds"))))))
    plot(ape::as.phylo(cluster), type = "radial", tip.color = 'black',
         direction = "leftwards",
         edge.color = "deepskyblue4", edge.width = 1.6,
         cex = .95,
         lab4ut = 'axial',
         label.offset = .05, 
         no.margin = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  
  ### --------------------------------------------------------------------
  ### --- TAB: Wiki View
  ### --------------------------------------------------------------------
  
  ### ------ 
  ### --- SubTAB: Wiki
  ### ------
  
  ### --- SELECT: update select 'selectWiki'
  updateSelectizeInput(session,
                       'selectWiki',
                       choices = wdcmSelectedWikies,
                       selected = 'enwiki',
                       server = TRUE)
  
  ### ---  when user selects a category
  selectedWiki <- reactive({input$selectWiki})
  
  ### --- OUTPUT output$wiki_CategoryDistribution
  output$wiki_CategoryDistribution <- renderPlot({
    pFrame <- t(wdcmWikies[rownames(wdcmWikies) %in% selectedWiki(), ])
    pFrame <- data.frame(category = rownames(pFrame), 
                         count = pFrame[, 1],
                         stringsAsFactors = F)
    pFrame$percent <- round(pFrame$count/sum(pFrame$count)*100, 2)
    pFrame$label <- paste0(pFrame$percent, "%")
    pFrame$category <- paste0(pFrame$category, " (", pFrame$label, ")")
    ggplot2::ggplot(pFrame, ggplot2::aes(x = "", 
                                         y = percent, 
                                         label = label, 
                                         group = category, 
                                         fill = category)) + 
      ggplot2::geom_bar(width = 1, stat = "identity") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::scale_fill_brewer(palette = "Set3") +
      ggplot2::theme_minimal() + 
      ggplot2::theme(legend.title = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text =  ggplot2::element_text(size = 12)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) + 
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  })
  
  
  ### --- OUTPUT output$wiki_Neighbourhood
  output$wiki_Neighbourhood <- renderPlot({
    
    simData <- read.csv(paste0(publicDir, paste0(selectedWiki(), 
                                                 "_localSimilarity.csv")),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
    similarWikies <- names(sort(
      simData[rownames(simData) %in% selectedWiki(), ], 
      decreasing = F)[1:11])
    simData <- simData[rownames(simData) %in% similarWikies, similarWikies]
    simData$wiki <- rownames(simData)
    simData <- simData %>% 
      tidyr::gather(key = neighbour, 
                    value = similarity, 
                    -wiki) %>% 
      dplyr::filter(wiki != neighbour) %>% 
      dplyr::group_by(wiki) %>% 
      dplyr::arrange(wiki, similarity) %>% 
      dplyr::top_n(-3)
    # - igraph
    simData <- data.frame(ougoing = simData$wiki,
                          incoming = simData$neighbour,
                          stringsAsFactors = F)
    simData <- igraph::graph.data.frame(simData, directed = T)
    # - plot w. {igraph}
    igraph::V(simData)$color <- ifelse(names(igraph::V(simData)) == selectedWiki(), "red", "white")
    plot(simData,
         edge.width = .75,
         edge.color = "grey40",
         edge.arrow.size = 0.35,
         vertex.size = 5,
         vertex.label.color = "black",
         vertex.label.font = 1,
         vertex.label.family = "sans",
         vertex.label.cex = 1,
         vertex.label.dist = .45,
         edge.curved = 0.5,
         margin = c(rep(0,4)))
  })
  
  ### --- OUTPUT output$wiki_CategoryProfiles
  output$wiki_CategoryProfiles <- renderPlot({
    
    simData <- read.csv(paste0(publicDir, paste0(selectedWiki(), 
                                                 "_localSimilarity.csv")),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
    
    similarWikies <- names(sort(
      simData[rownames(simData) %in% selectedWiki(), ], 
      decreasing = F)[1:11])
    pFrame <- wdcmWikies[rownames(wdcmWikies) %in% similarWikies, ]
    pFrame$wiki <- rownames(pFrame)
    pFrame <- pFrame %>% 
      tidyr::gather(key = category,
                    value = items,
                    -wiki)
    pFrame$color <- ifelse(pFrame$wiki %in% selectedWiki(), "red", "grey40")
    pFrame$label <- character(length(pFrame$wiki))
    pFrame$label[pFrame$wiki %in% selectedWiki()] <- 
      pFrame$items[pFrame$wiki %in% selectedWiki()]
    ggplot2::ggplot(pFrame, ggplot2::aes(x = category, 
                                         y = log10(items), 
                                         color = wiki, 
                                         group = wiki, 
                                         label = label)) + 
      ggplot2::geom_path() + 
      ggrepel::geom_text_repel(size = 3, 
                               show.legend = F) + 
      ggplot2::scale_color_brewer(palette = "Spectral") +
      ggplot2::xlab("\nWDCM Semantic Category") + ggplot2::ylab("log(Item Usage)") +
      ggplot2::theme_minimal() + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + 
      ggplot2::theme(legend.title = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text =  ggplot2::element_text(size = 12))
    
  })
  
  ### --- OUTPUT output$wiki_SimilarityProfile
  output$wiki_SimilarityProfile <- renderPlot({
    
    simData <- read.csv(paste0(publicDir, paste0(selectedWiki(), 
                                                 "_localSimilarity.csv")),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
    simData <- simData[rownames(simData) %in% selectedWiki(), ]
    simData <- simData[, !(colnames(simData) %in% selectedWiki())]
    simData <- data.frame(wiki = colnames(simData), 
                          similarity = as.numeric(simData[1, ]), 
                          stringsAsFactors = F)
    ggplot2::ggplot(simData, ggplot2::aes(x = similarity)) +
      ggplot2::geom_histogram(binwidth = .1,
                              colour = "white", 
                              fill = "deepskyblue", 
                              alpha = .5) + 
      ggplot2::xlab("\nSimilarity (0 - 1)") + ggplot2::ylab("Num. of Wikies") + 
      ggplot2::scale_x_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
      ggplot2::theme_minimal() + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.95, vjust = 0.2)) + 
      ggplot2::theme(legend.title = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text =  ggplot2::element_text(size = 12))
    
  })
  
  ### ------ 
  ### --- SubTAB: Wiki:Similarity
  ### ------
  
  ### --- OUTPUT output$projectsGraph_SelectedCategory
  output$wikiGraph_SelectedWiki <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected Wikipedia: </b>', input$selectWiki, '</p>')
  })
  
  # - output$wikiGraph
  output$wikiGraph <- visNetwork::renderVisNetwork({
    
    withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
      
      incProgress(1/3, detail = "Read nodes.")
      nodes <- read.csv(paste0(publicDir, paste0(input$selectWiki, 
                                                 "_wikiSimNodes.csv")),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
      nodes$color[nodes$color != "red"] <- "white" 
      
      incProgress(2/3, detail = "Read edges.")
      edges <- read.csv(paste0(publicDir, paste0(input$selectWiki, 
                                                 "_wikiSimEdges.csv")),
                        header = T,
                        check.names = F,
                        row.names = 1,
                        stringsAsFactors = F)
      edges$width <- (edges$width - 2)/2
      edges$color <- "grey40"
      
      incProgress(3/3, detail = "Render graph.")
      visNetwork::visNetwork(nodes = nodes,
                             edges = edges,
                             width = "100%",
                             height = "100%") %>%
        visNetwork::visEvents(type = "once",
                              startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
        visNetwork::visPhysics(enabled = T, maxVelocity = 1) %>% 
        visNetwork::visNodes(scaling = list(label = list(enabled = T))) %>% 
        visNetwork::visOptions(highlightNearest = TRUE, selectedBy = "label")
      
    })
    
  })
  
  
  ### ------ 
  ### --- SubTAB: Wiki:Topics
  ### ------
  
  ### --- OUTPUT output$projectsGraph_SelectedCategory
  output$wiki_TopicProfile_SelectedWiki <- renderText({
    paste0('<p style="font-size:100%;"><b>Selected Wikipedia: </b>', input$selectWiki, '</p>')
  })
  
  ### --- OUTPUT output$wiki_TopicProfile
  output$wiki_TopicProfile <- renderPlot({
    
    collectTopics <- lapply(wikiTopic, function(x) {
      x[which(rownames(x) %in% selectedWiki()), ]
    })
    collectTopics <- data.table::rbindlist(collectTopics, use.names = T, fill = T)
    collectTopics[is.na(collectTopics)] <- 0
    collectTopics$category <- names(wikiTopic)
    collectTopics <- collectTopics %>% 
      tidyr::gather(key = Topic, 
                    value = Probability, 
                    -category)
    collectTopics$label <- sapply(collectTopics$Probability, function(x) {
      if (x == 0) {""
      } else {
        round(x, 3)
      }
    })
    collectTopics$Topic <- gsub("topic([[:digit:]]+)", "Theme \\1", collectTopics$Topic)
    collectTopics$Topic <- factor(collectTopics$Topic, 
                                  levels = paste0("Theme ", 
                                                  order(as.numeric(
                                                    unlist(
                                                      stringr::str_extract_all(
                                                        unique(collectTopics$Topic), 
                                                        "[[:digit:]]+")
                                                    )
                                                  )
                                                  )
                                  )
    )
    
    # - visualize w. ggplot2::ggplot2
    ggplot2::ggplot(collectTopics,
                    ggplot2::aes(x = Topic, 
                                 y = Probability, 
                                 label = label)
    ) +
      ggplot2::geom_line(color = "black", group = 1, size = .35) + 
      ggplot2::geom_point(size = 1.5, color = "black") + 
      ggplot2::geom_point(size = 1, color = "white") +
      ggrepel::geom_text_repel(size = 3) +
      ggplot2::facet_wrap(~ category, 
                          ncol = 3, 
                          scales = "free_y") +
      ggplot2::xlab('Topic') + ggplot2::ylab('Probability') +
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 13)) + 
      ggplot2::theme(panel.background = ggplot2::element_rect(color = "white", fill = "aliceblue")) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- OUTPUT output$wiki_TopicProfile_interactive
  output$wiki_TopicProfile_interactive <- plotly::renderPlotly({
    
    collectTopics <- lapply(wikiTopic, function(x) {
      x[which(rownames(x) %in% selectedWiki()), ]
    })
    collectTopics <- data.table::rbindlist(collectTopics, use.names = T, fill = T)
    collectTopics[is.na(collectTopics)] <- 0
    collectTopics$category <- names(wikiTopic)
    collectTopics <- collectTopics %>% 
      tidyr::gather(key = Topic, 
                    value = Probability, 
                    -category)
    collectTopics$label <- sapply(collectTopics$Probability, function(x) {
      if (x == 0) {""
      } else {
        round(x, 3)
      }
    })
    collectTopics$Topic <- gsub("topic([[:digit:]]+)", "Theme \\1", collectTopics$Topic)
    collectTopics$Topic <- factor(collectTopics$Topic, 
                                  levels = paste0("Theme ", 
                                                  order(as.numeric(
                                                    unlist(
                                                      stringr::str_extract_all(
                                                        unique(collectTopics$Topic), 
                                                        "[[:digit:]]+")
                                                    )
                                                  )
                                                  )
                                  )
    )
    
    # - visualize w. ggplot2::ggplot2
    g <- ggplot2::ggplot(collectTopics,
                         ggplot2::aes(x = Topic, 
                                      y = Probability, 
                                      label = label)
    ) +
      ggplot2::geom_line(color = "darkblue", group = 1, size = .35) + 
      ggplot2::geom_point(size = 1.5, color = "darkblue") + 
      ggplot2::facet_wrap(~ category, 
                          ncol = 3, 
                          scales = "free_y") +
      ggplot2::xlab('Topic') + ggplot2::ylab('Probability') +
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                         size = 10, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 13)) + 
      ggplot2::theme(panel.background = ggplot2::element_rect(color = "white", 
                                                              fill = "white"))
    plotly::ggplotly(g, 
                     tooltip = c("x","y"),
                     originalData = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
}
