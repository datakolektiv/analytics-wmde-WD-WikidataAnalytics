### ---------------------------------------------------------------------------
### --- WD Usage and Coverage
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
#' @import DT
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
  
  ### --- Wikidata Logo
  output$logo <- renderUI({
    tags$img(src =
               'https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Wikidata-logo-en.svg/200px-Wikidata-logo-en.svg.png')
  })
  
  # - get update stamp:
  h <- curl::new_handle()
  curl::handle_setopt(h,
                copypostfields = "WD_percentUsageDashboard");
  curl::handle_setheaders(h,
                    "Cache-Control" = "no-cache"
  )
  timestamp <- curl::curl_fetch_memory('https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/Wikidata/WD_percentUsage/')
  timestamp <- rawToChar(timestamp$content)
  timestamp <- stringr::str_extract_all(timestamp, "[[:digit:]]+.+[[:digit:]]+")[[1]][3]
  timestamp <- gsub("<.+", "", timestamp)
  timestamp <- trimws(timestamp, which = "right")
  timestamp <- paste0("Updated: ", timestamp, " UTC")
  
  # - get current data file:
  publicDir <- 'https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/Wikidata/WD_percentUsage/'
  filename <- 'wdUsage_ProjectStatistics.csv'
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    dataSet <- get_WDCM_table(publicDir, filename)
    dataSet[, 1] <- NULL
  })
  dataSet <- dplyr::arrange(dataSet, desc(percentWDuse))
  colnames(dataSet) <- c('Project', 
                         'N. of Articles', 
                         'N. of Articles that use WD',
                         'N. of Articles w. Sitelinks',
                         '% of Articles that use WD',
                         '% of Articles w. Sitelinks',
                         'Project Type')
  dataSet <- dplyr::filter(dataSet, !(Project %in% 'wikidatawiki'))
  
  overalPercentUsage <- round(sum(dataSet$`N. of Articles that use WD`)/sum(dataSet$`N. of Articles`), 6) * 100
  overalPercentCoverage <- round(sum(dataSet$`N. of Articles w. Sitelinks`)/sum(dataSet$`N. of Articles`), 6) * 100
  
  
  ### --- output: overall % of pages
  ### --- that use WD across the projects
  output$overall <- renderText({
    paste0('<p style="font-size:90%;"align="left"><b>Wikidata usage</b> is found in <b> ',
           overalPercentUsage,
           "%</b> of articles across the WMF projects considered.</p>"
    )
  })
  
  ### --- output: overall % of pages w. sitelinks
  output$coverage <- renderText({
    paste0('<p style="font-size:90%;"align="left"><b>Wikidata Sitelinks</b> are found in <b> ',
           overalPercentCoverage,
           "%</b> of articles across the WMF projects considered.</p>"
    )
  })
  
  ### --- timestamp
  output$timestamp <- renderText({
    paste0('<p style="font-size:80%;"align="left"><b>', 
           timestamp,
           "</b></p>"
    )
  })
  
  ### --- TABLES
  
  ### --- output$overviewDT
  output$overviewDT <- DT::renderDataTable({
    DT::datatable(dataSet, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  ### --- output$overviewDT
  output$overviewDT_projectType <- DT::renderDataTable({
    d <- dataSet %>% 
      dplyr::select(`Project Type`, 
             `N. of Articles`,
             `N. of Articles that use WD`, 
             `N. of Articles w. Sitelinks`) %>% 
      dplyr::group_by(`Project Type`) %>% 
      dplyr::summarise(`N. of Articles` = sum(`N. of Articles`),
                `N. of Articles that use WD` = sum(`N. of Articles that use WD`), 
                `N. of Articles w. Sitelinks` = sum(`N. of Articles w. Sitelinks`))
    
    d$`% of Articles that use WD` = round(d$`N. of Articles that use WD`/d$`N. of Articles`*100, 2)
    d$`% of Articles w. Sitelinks` = round(d$`N. of Articles w. Sitelinks`/d$`N. of Articles`*100, 2)
    d <- dplyr::arrange(d, desc(`% of Articles that use WD`))
    DT::datatable(d, 
                  options = list(
                    pageLength = 100,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ),
                  rownames = FALSE
    )
  })
  
  ### --- Percent WD Usage per ProjectType
  output$percentProjectType <- renderPlot({
    plotFrame <- dataSet %>% 
      dplyr::select(`Project Type`, `N. of Articles`, `N. of Articles that use WD`) %>%
      dplyr::group_by(`Project Type`) %>% 
      dplyr::summarise(TotalUsage = sum(`N. of Articles that use WD`), 
                       Usage = sum(`N. of Articles`))
    plotFrame$PercentWDUSage <- round(plotFrame$TotalUsage/plotFrame$Usage*100, 2)
    plotFrame$Label <- paste0(plotFrame$TotalUsage, "(", plotFrame$PercentWDUSage, "%)")
    ggplot2::ggplot(plotFrame, ggplot2::aes(x = `Project Type`, y = log10(TotalUsage),
                          color = `Project Type`,
                          fill = `Project Type`,
                          label = Label)) +
      ggplot2::geom_bar(width = .2, stat = 'identity') +
      ggrepel::geom_label_repel(size = 3.5, 
                       segment.size = .25, 
                       show.legend = FALSE, 
                       fill = "white") +
      ggplot2::ggtitle("Total Wikidata Usage [(S)itelinkes excluded]") + 
      ggplot2::ylab("log10(Total WD Usage)") + 
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) + 
      ggplot2::theme(legend.position = "top") + 
      ggplot2::theme(panel.border = ggplot2::element_blank())
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- Percent WD Usage per ProjectType Interactive
  output$percentProjectType_interactive <- plotly::renderPlotly({
    plotFrame <- dataSet %>% 
      dplyr::select(`Project Type`, `N. of Articles`, `N. of Articles that use WD`) %>%
      dplyr::group_by(`Project Type`) %>% 
      dplyr::summarise(TotalUsage = sum(`N. of Articles that use WD`), 
                       Usage = sum(`N. of Articles`))
    plotFrame$PercentWDUSage <- round(plotFrame$TotalUsage/plotFrame$Usage*100, 2)
    plotFrame$Label <- paste0(plotFrame$TotalUsage, "(", plotFrame$PercentWDUSage, "%)")
    g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = `Project Type`, y = log10(TotalUsage),
                                            color = `Project Type`,
                                            fill = `Project Type`,
                                            label = Label)) +
      ggplot2::geom_bar(width = .2, stat = 'identity') +
      ggplot2::ggtitle("Total Wikidata Usage [(S)itelinkes excluded]") + 
      ggplot2::ylab("log10(Total WD Usage)") + 
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) + 
      ggplot2::theme(legend.position = "top") + 
      ggplot2::theme(panel.border = ggplot2::element_blank())
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label", "fill"),
                     originalData = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- Top WD Usage per Project
  output$wdUsagePerProject <- renderPlot({
    plotFrame <- dataSet %>% 
      dplyr::select(Project, `N. of Articles that use WD`) %>% 
      dplyr::arrange(desc(`N. of Articles that use WD`)) %>%
      head(20)
    plotFrame$Project <- factor(plotFrame$Project, 
                                levels = plotFrame$Project[order(-plotFrame$`N. of Articles that use WD`)])
    ggplot2::ggplot(plotFrame, ggplot2::aes(x = Project, y = `N. of Articles that use WD`,
                          label = Project)) +
      ggplot2::geom_path(size = .15, color = "darkblue", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "darkblue") +
      ggplot2::geom_point(size = 1, color = "white") + 
      ggrepel::geom_label_repel(size = 3.5, 
                       segment.size = .25, 
                       show.legend = FALSE, 
                       fill = "white") +
      ggplot2::ggtitle("Number of pages using Wikidata [(S)itelinkes excluded]") + 
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::ylab("Number of pages using WD") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 11)) + 
      ggplot2::theme(panel.border = ggplot2::element_blank())
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- Top WD Usage per Project Interactive
  output$wdUsagePerProject_interactive <- plotly::renderPlotly({
    plotFrame <- dataSet %>% 
      dplyr::select(Project, `N. of Articles that use WD`) %>% 
      dplyr::arrange(desc(`N. of Articles that use WD`)) %>%
      head(20)
    plotFrame$Project <- factor(plotFrame$Project, 
                                levels = plotFrame$Project[order(-plotFrame$`N. of Articles that use WD`)])
    g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Project, y = `N. of Articles that use WD`,
                                            label = Project)) +
      ggplot2::geom_path(size = .15, color = "darkblue", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "darkblue") +
      ggplot2::ggtitle("Number of pages using Wikidata [(S)itelinkes excluded]") + 
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::ylab("Number of pages using WD") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 11)) + 
      ggplot2::theme(panel.border = ggplot2::element_blank())
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- Top WD Usage Proportion per Project
  output$wdUsagePropPerProject <- renderPlot({
    plotFrame <- dataSet %>% 
      dplyr::select(Project, `% of Articles that use WD`) %>% 
      dplyr::arrange(desc(`% of Articles that use WD`)) %>%
      head(20)
    plotFrame$Project <- factor(plotFrame$Project, 
                                levels = plotFrame$Project[order(-plotFrame$`% of Articles that use WD`)])
    ggplot2::ggplot(plotFrame, ggplot2::aes(x = Project, y = `% of Articles that use WD`,
                          label = Project)) +
      ggplot2::geom_path(size = .15, color = "darkblue", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "darkblue") +
      ggplot2::geom_point(size = 1, color = "white") + 
      ggrepel::geom_label_repel(size = 3.5, 
                       segment.size = .25, 
                       show.legend = FALSE, 
                       fill = "white") +
      ggplot2::ggtitle("Percent of pages that use WD [(S)itelinkes excluded]") + 
      ggplot2::ylab("% pages") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 11)) + 
      ggplot2::theme(panel.border = ggplot2::element_blank())
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- Top WD Usage Proportion per Project Interactive
  output$wdUsagePropPerProject_interactive <- plotly::renderPlotly({
    plotFrame <- dataSet %>% 
      dplyr::select(Project, `% of Articles that use WD`) %>% 
      dplyr::arrange(desc(`% of Articles that use WD`)) %>%
      head(20)
    plotFrame$Project <- factor(plotFrame$Project, 
                                levels = plotFrame$Project[order(-plotFrame$`% of Articles that use WD`)])
    g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Project, y = `% of Articles that use WD`,
                                            label = Project)) +
      ggplot2::geom_path(size = .15, color = "darkblue", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "darkblue") +
      ggplot2::ggtitle("Percent of pages that use WD [(S)itelinkes excluded]") + 
      ggplot2::ylab("% pages") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 11)) + 
      ggplot2::theme(panel.border = ggplot2::element_blank())
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "label"),
                     originalData = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  
  ### --- WD Usage Proportion per ProjectType
  output$wdUsagePropPerProjectType_Polar <- renderPlot({
    plotFrame <- dataSet %>%
      dplyr::select(`Project Type`, `N. of Articles`, `N. of Articles that use WD`) %>%
      dplyr::group_by(`Project Type`) %>%
      dplyr::summarise(`N. of Articles` = sum(`N. of Articles`), 
                       `N. of Articles that use WD` = sum(`N. of Articles that use WD`))
    plotFrame$Percent <- round(
      plotFrame$`N. of Articles that use WD`/sum(plotFrame$`N. of Articles that use WD`)*100, 3)
    plotFrame$`Project Type` <- factor(plotFrame$`Project Type`,
                                       levels = plotFrame$`Project Type`[order(-plotFrame$Percent)])
    ggplot2::ggplot(plotFrame, ggplot2::aes(x = "", y = Percent,
                          label = paste0(`Project Type`, "(", Percent, "%)"),
                          color = `Project Type`, 
                          fill = `Project Type`)) +
      ggplot2::geom_bar(stat = "identity", width = .2) + 
      ggplot2::coord_polar("y", start = 0) +
      ggrepel::geom_label_repel(size = 3.5, 
                       segment.size = .25, 
                       show.legend = FALSE, 
                       fill = "white") +
      ggplot2::ggtitle("Percent WD usage across Project Types [(S)itelinkes excluded]") + 
      ggplot2::ylab("% of WD Usage") + ggplot2::xlab("") + 
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = .5)) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11)) + 
      ggplot2::theme(panel.border = ggplot2::element_blank())
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  
}
