### ---------------------------------------------------------------------------
### --- WDCM Overview
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
### --- https://wikidata-analytics.wmflabs.org/
### ---
### --- WDCM is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDCM is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDCM If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {
  
  # Application server logic 
  
  ### --- dirTree
  etl_dir <- 'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/'
  ml_dir <- 'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/ml/'
  updatePath <- 'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/WDCM_MainReport.csv'
  
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
  
  # - projectType() to determine project type
  projectType <- function(projectName) {
    unname(sapply(projectName, function(x) {
      if (grepl("commons", x, fixed = T)) {"Commons"
      } else if (grepl("mediawiki|meta|species|wikidata", x)) {"Other"
      } else if (grepl("wiki$", x)) {"Wikipedia"
      } else if (grepl("quote$", x)) {"Wikiquote"
      } else if (grepl("voyage$", x)) {"Wikivoyage"
      } else if (grepl("news$", x)) {"Wikinews"
      } else if (grepl("source$", x)) {"Wikisource"
      } else if (grepl("wiktionary$", x)) {"Wiktionary"
      } else if (grepl("versity$", x)) {"Wikiversity"
      } else if (grepl("books$", x)) {"Wikibooks"
      } else {"Other"}
    }))
  }
  
  ### --- DATA
  
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    
    ### --- fetch wdcm2_project
    wdcmProject <- get_WDCM_table(etl_dir, 'wdcm_project.csv')
    wdcmProject$projectype <- projectType(wdcmProject$eu_project)
    incProgress(1/6, detail = "Please be patient.")
    # - determine how many project types are present
    # - and assign Brewer colors
    lengthProjectColor <- length(unique(wdcmProject$projectype))
    projectTypeColor <- RColorBrewer::brewer.pal(lengthProjectColor, "Set1")
    
    ### --- fetch wdcm2_project_category_2dmap
    wdcm2_project_category_2dmap <-
      get_WDCM_table(ml_dir,
                     'wdcm_project_category_2dmap.csv',
                     row_names = T)
    colnames(wdcm2_project_category_2dmap)[3] <- "eu_project"
    wdcm2_project_category_2dmap <- dplyr::left_join(wdcm2_project_category_2dmap,
                                                     wdcmProject,
                                                     by = "eu_project")
    labelSet <- unlist(lapply(unique(wdcm2_project_category_2dmap$projecttype), 
                              function(x){
                                w <- which(wdcm2_project_category_2dmap$projecttype %in% x)
                                lS <- dplyr::arrange(wdcm2_project_category_2dmap[w, ], desc(eu_count))[1:5, ]
                                lS$eu_project
                              }))
    labelSetSmall1 <- unlist(lapply(unique(wdcm2_project_category_2dmap$projecttype), 
                                    function(x){
                                      w <- which(wdcm2_project_category_2dmap$projecttype %in% x)
                                      lS <- dplyr::arrange(wdcm2_project_category_2dmap[w, ], desc(eu_count))[1, ]
                                      lS$eu_project
                                    }))
    labelSetSmall3 <- unlist(lapply(unique(wdcm2_project_category_2dmap$projecttype), 
                                    function(x){
                                      w <- which(wdcm2_project_category_2dmap$projecttype %in% x)
                                      lS <- dplyr::arrange(wdcm2_project_category_2dmap[w, ], desc(eu_count))[1:3, ]
                                      lS$eu_project
                                    }))
    wdcm2_project_category_2dmapReduceLabels <- wdcm2_project_category_2dmap
    wdcm2_project_category_2dmapReduceLabels$eu_project[which(!(wdcm2_project_category_2dmapReduceLabels$eu_project %in% labelSet))] <- ""
    colnames(wdcm2_project_category_2dmap)[c(3, 5, 6)] <- 
      c('Project', 'Usage', 'Project Type')
    wdcm2_project_category_2dmap$projectTypeColor <- 
      sapply(wdcm2_project_category_2dmap$`Project Type`, function(x) {
        projectTypeColor[which(sort(unique(wdcm2_project_category_2dmap$`Project Type`)) %in% x)]
        })
    
    ### --- fetch wdcm2_category
    wdcmCategory <- get_WDCM_table(etl_dir, 'wdcm_category.csv', row_names = T)
    incProgress(5/6, detail = "Please be patient.") 
    
    ### ---fetch wdcm2_category_project_2dmap
    wdcm2_category_project_2dmap <- 
      get_WDCM_table(ml_dir, 
                     'wdcm2_category_project_2dmap.csv', 
                     row_names = T)
    wdcm2_category_project_2dmap <- dplyr::left_join(wdcm2_category_project_2dmap,
                                              wdcmCategory,
                                              by = "category")
    colnames(wdcm2_category_project_2dmap)[3:4] <- c('Category', 'Usage')
    
    ### --- fetch wdcm2_project_category
    wdcmProjectCategory <- get_WDCM_table(etl_dir, 'wdcm_project_category.csv', row_names = T)
    wdcmProjectCategory$type <- projectType(wdcmProjectCategory$eu_project)
    colnames(wdcmProjectCategory) <- c('Project', 'Category', 'Usage', 'Project Type')
    wdcmProjectCategory <- wdcmProjectCategory[complete.cases(wdcmProjectCategory), ]
    
    ### --- Fetch update info
    update <- read.csv(updatePath, 
                       header = T,
                       check.names = F,
                       stringsAsFactors = F,
                       row.names = 1)
    
  })
  
  ### --- OUTPUTS
  
  ### --- output: updateString
  output$updateString <- renderText({
    date <- update[max(which(grepl("Orchestra END", update$Step))), ]$Time
    date <- paste0(date, " UTC")
    return(paste('<p style="font-size:80%;"align="right"><b>Last update: </b><i>', date, '</i></p>', sep = ""))
  })
  
  ### ----------------------------------
  ### --- TAB: Overview
  ### ----------------------------------
  
  ### --- output$overviewPlot
  output$overviewPlot <- renderPlot({
    ggplot2::ggplot(wdcm2_project_category_2dmapReduceLabels, 
                    ggplot2::aes(x = D1, y = D2,
                                 color = projecttype,
                                 label = eu_project)) +
      ggplot2::geom_point(ggplot2::aes(size = eu_count), shape = 21) +
      ggplot2::scale_size(name = "Usage",
                          breaks = ggplot2::waiver(),
                          labels = scales::comma,
                          limits = NULL,
                          range = c(.5, 30),
                          trans = "identity",
                          guide = "legend") + 
      ggplot2::scale_colour_manual(values = projectTypeColor, name = "Project Type") +
      ggrepel::geom_text_repel(size = 5, fontface = 'bold', segment.size = .25, show.legend = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(color = "white", fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text = ggplot2::element_text(size = 14)) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 15))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$overviewPlotDynamic
  output$overviewPlotDynamic <- rbokeh::renderRbokeh({
    outFig <- rbokeh::figure(width = 1400, height = 900, logo = NULL) %>%
      rbokeh::ly_points(D1, D2,
                        data = wdcm2_project_category_2dmap,
                        size = log(Usage),
                        color = 'Project Type',
                        hover = list(Project, Usage)) %>% 
      rbokeh::x_axis(visible = F) %>% 
      rbokeh::y_axis(visible = F) %>% 
      rbokeh::theme_grid(which = c("x", "y"), 
                 grid_line_color = "white") %>% 
      rbokeh::theme_plot(outline_line_alpha = 0) %>% 
      rbokeh::set_palette(discrete_color = rbokeh::pal_color(projectTypeColor))
    outFig
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$usageTendencyPlot
  output$usageTendencyPlot <- renderPlot({
    ggplot2::ggplot(wdcm2_category_project_2dmap, ggplot2::aes(x = D1, 
                                             y = D2, 
                                             label = Category)) +
      ggplot2::scale_color_discrete(guide = FALSE) +
      ggplot2::geom_point(ggplot2::aes(size = Usage), fill = "cadetblue1", 
                 color = "cadetblue4", shape = 21) +
      ggplot2::scale_size(name = "Usage", 
                 breaks = ggplot2::waiver(), 
                 labels = scales::comma,
                 limits = NULL, 
                 range = c(2, 20), 
                 trans = "identity", 
                 guide = "legend") + 
      ggplot2::theme_bw() +
      ggrepel::geom_text_repel(size = 4, show.legend = FALSE) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 12))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectRankFrequencyPlot
  output$projectRankFrequencyPlot <- renderPlot({
    rank <- order(wdcmProject$eu_count)
    frequency <- wdcmProject$eu_count[rank]
    project <- wdcmProject$eu_project[rank]
    dataSet <- data.frame(Rank = rank,
                          Frequency = frequency, 
                          Project = project,
                          stringsAsFactors = F)
    dataSet$Project[which(!(dataSet$Project %in% labelSetSmall1))] <- ""
    dataSet <- dataSet[order(-dataSet$Frequency), ]
    ggplot2::ggplot(dataSet, ggplot2::aes(x = Rank, 
                        y = Frequency,
                        label = Project)) +
      ggplot2::geom_path(size = .25, color = "darkblue") + 
      ggplot2::geom_point(size = 1, color = "darkblue") + 
      ggplot2::geom_point(size = .65, color = "white") + 
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::xlab("Project Usage Rank") + ggplot2::ylab("Project Usage") + 
      ggrepel::geom_text_repel(size = 3, segment.size = .15, show.legend = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 12))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectRankFrequencyPlot_interactive
  output$projectRankFrequencyPlot_interactive <- plotly::renderPlotly({
    rank <- order(wdcmProject$eu_count)
    frequency <- wdcmProject$eu_count[rank]
    project <- wdcmProject$eu_project[rank]
    dataSet <- data.frame(Rank = rank,
                          Frequency = frequency, 
                          Project = project,
                          stringsAsFactors = F)
    dataSet <- dataSet[order(-dataSet$Frequency), ]
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = Rank, 
                                          y = Frequency,
                                          label = Project)) +
      ggplot2::geom_path(size = .25, color = "cadetblue3") + 
      ggplot2::geom_point(size = 1, color = "cadetblue3") + 
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::xlab("Project Usage Rank") + ggplot2::ylab("Project Usage") + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 12))
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
    
  })
  
  ### --- output$projectLogRankLogFrequencyPlot
  output$projectLogRankLogFrequencyPlot <- renderPlot({
    rank <- order(wdcmProject$eu_count)
    frequency <- wdcmProject$eu_count[rank]
    project <- wdcmProject$eu_project[rank]
    dataSet <- data.frame(Rank = log(rank),
                          Frequency = log(frequency), 
                          Project = project,
                          stringsAsFactors = F)
    dataSet$Project[which(!(dataSet$Project %in% labelSetSmall3))] <- ""
    dataSet <- dataSet[order(-dataSet$Frequency), ]
    ggplot2::ggplot(dataSet, ggplot2::aes(x = Rank, 
                        y = Frequency,
                        label = Project)) +
      ggplot2::geom_path(size = .25, color = "red") + 
      ggplot2::geom_smooth(size = .25, method = "lm", color = "red") + 
      ggplot2::geom_point(size = 1, color = "red") + 
      ggplot2::geom_point(size = .65, color = "white") + 
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::xlab("log(Project Usage Rank)") + ggplot2::ylab("log(Project Usage)") + 
      ggrepel::geom_text_repel(size = 3, segment.size = .15, show.legend = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 12))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectLogRankLogFrequencyPlot_interactive
  output$projectLogRankLogFrequencyPlot_interactive <- plotly::renderPlotly({
    rank <- order(wdcmProject$eu_count)
    frequency <- wdcmProject$eu_count[rank]
    project <- wdcmProject$eu_project[rank]
    dataSet <- data.frame(Rank = log(rank),
                          Frequency = log(frequency), 
                          Project = project,
                          stringsAsFactors = F)
    dataSet <- dataSet[order(-dataSet$Frequency), ]
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = Rank, 
                                          y = Frequency,
                                          label = Project)) +
      ggplot2::geom_path(size = .25, color = "red") + 
      ggplot2::geom_smooth(size = .25, method = "lm", color = "red") + 
      ggplot2::geom_point(size = 1, color = "red") + 
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::xlab("log(Project Usage Rank)") + ggplot2::ylab("log(Project Usage)") + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 12))
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
})
  
  ### --- output$projectCategoryCross
  output$projectCategoryCross <- renderPlot({
    dataSet <- wdcmProjectCategory
    dataSet$Project <- NULL
    dataSet <- dataSet %>% 
      dplyr::group_by(`Project Type`, Category) %>%
      dplyr::summarise(Usage = sum(Usage)) %>% 
      as.data.frame()
    dataSet$`Project Type` <- factor(dataSet$`Project Type`, 
                                     levels = unique(dataSet$`Project Type`))
    dataSet$Category <- factor(dataSet$Category, 
                               levels = unique(dataSet$Category))
    ggplot2::ggplot(dataSet, ggplot2::aes(x = Category, 
                        y = Usage,
                        fill = `Project Type`,
                        color = `Project Type`)) +
      ggplot2::geom_bar(stat = "identity", width = .35) +
      ggplot2::scale_fill_manual(values = projectTypeColor, name = "Project Type") +
      ggplot2::scale_color_manual(values = projectTypeColor) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::xlab("Category") + ggplot2::ylab("Usage") + 
      ggplot2::facet_grid(`Project Type` ~ ., scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 11, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(strip.background = ggplot2::element_blank()) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 11))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectCategoryCross_interactive
  output$projectCategoryCross_interactive <- plotly::renderPlotly({
    dataSet <- wdcmProjectCategory
    dataSet$Project <- NULL
    dataSet <- dataSet %>% 
      dplyr::group_by(`Project Type`, Category) %>%
      dplyr::summarise(Usage = sum(Usage)) %>% 
      as.data.frame()
    dataSet$`Project Type` <- factor(dataSet$`Project Type`, 
                                     levels = unique(dataSet$`Project Type`))
    dataSet$Category <- factor(dataSet$Category, 
                               levels = unique(dataSet$Category))
    g <- ggplot2::ggplot(dataSet, ggplot2::aes(x = Category, 
                                          y = Usage,
                                          fill = `Project Type`,
                                          color = `Project Type`)) +
      ggplot2::geom_bar(stat = "identity", width = .35) +
      ggplot2::scale_fill_manual(values = projectTypeColor, name = "Project Type") +
      ggplot2::scale_color_manual(values = projectTypeColor) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::xlab("Category") + ggplot2::ylab("Usage") + 
      ggplot2::facet_grid(`Project Type` ~ ., scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(strip.background = ggplot2::element_blank()) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 8))
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 1)})
  })
  
  ### --- output$projectVolume
  output$projectVolume <- renderPlot({
    minQ <- input$volumeSlider[1]/100
    maxQ <- input$volumeSlider[2]/100
    wSel <- which(wdcmProject$eu_count <= quantile(wdcmProject$eu_count, maxQ) & 
                    wdcmProject$eu_count >= quantile(wdcmProject$eu_count, minQ))
    dataSet <- wdcmProject[wSel, ] %>% 
      dplyr::arrange(desc(eu_count)) %>% as.data.frame()
    colnames(dataSet) <- c('Project', 'Usage', 'Project Type')
    if (dim(dataSet)[1] > 30) {
      dataSet <- dataSet[1:30, ]
    }
    dataSet$Project <- factor(dataSet$Project)
    dataSet <- dataSet[order(dataSet$Usage), ]
    ggplot2::ggplot(dataSet, ggplot2::aes(x = reorder(Project, Usage), 
                        y = Usage,
                        fill = `Project Type`)) +
      ggplot2::geom_bar(stat = "identity", width = .2) + 
      ggplot2::xlab("Project") + ggplot2::ylab("Usage") + 
      ggplot2::scale_fill_manual(values = projectTypeColor[which(sort(unique(wdcmProject$projectype)) %in% dataSet$`Project Type`)], 
                        name = "Project Type") +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 11, hjust = 1)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectCategoryDT::datatable
  output$projectCategoryDataTable <- DT::renderDT({
    DT::datatable(wdcmProjectCategory,
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
  
  ### --- output$projectDT::datatable
  output$projectDataTable <- DT::renderDT({
    dataSet <- wdcmProject
    colnames(dataSet) <- c('Project', 'Usage', 'Project Type')
    DT::datatable(dataSet,
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
  
}
