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

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinydashboard::dashboardPage(skin = "black",
                  
                  ### --- dashboarHeader
                  ### --------------------------------
                  
                  shinydashboard::dashboardHeader(
                    # - Title
                    title = "WDCM: Overview",
                    titleWidth = 230
                  ), 
                  ### ---- END dashboardHeader
                  
                  ### --- dashboardSidebar
                  ### --------------------------------
                  
                  shinydashboard::dashboardSidebar(
                    sidebarMenu(
                      id = "tabsWDCM",
                      menuItem(text = "Overview", 
                               tabName = "overview", 
                               icon = icon("barcode"),
                               selected = TRUE,
                               menuSubItem('Similarity Map',
                                           tabName = 'similarityMap',
                                           icon = icon('line-chart')
                               ),
                               menuSubItem('Map Highlights',
                                           tabName = 'mapHighlights',
                                           icon = icon('line-chart')
                               )
                               
                      ),
                      menuItem(text = "WD Usage Tendency", 
                               tabName = "usageTendency", 
                               icon = icon("barcode")
                      ),
                      menuItem(text = "WD Usage Distribution", 
                               tabName = "wdUsage", 
                               icon = icon("barcode"),
                               selected = TRUE,
                               menuSubItem('Rank-Frequency',
                                           tabName = 'usageDistribution',
                                           icon = icon('line-chart')
                               ),
                               menuSubItem('Log(Rank)-Log(Frequency)',
                                           tabName = 'usageDistributionLog',
                                           icon = icon('line-chart')
                               )
                               
                      ),
                      menuItem(text = "Client Types",
                               tabName = "clientTypes",
                               icon = icon("barcode")
                      ),
                      menuItem(text = "Client Usage Volume",
                               tabName = "clientVolume",
                               icon = icon("barcode")
                      ),
                      menuItem(text = "Tabs/Crosstabs", 
                               tabName = "tabs", 
                               icon = icon("barcode"),
                               selected = TRUE,
                               menuSubItem('Client Project vs Category',
                                           tabName = 'tabs1',
                                           icon = icon('line-chart')
                               ),
                               menuSubItem('Client Projects',
                                           tabName = 'tabs2',
                                           icon = icon('line-chart')
                               )
                      ),
                      menuItem(text = "Documentation",
                               tabName = "documentation",
                               icon = icon("barcode")
                      ),
                      menuItem(text = "Navigate WDCM",
                               tabName = "navigation",
                               icon = icon("barcode")
                      )
                    )
                  ),
                  ### --- END dashboardSidebar
                  
                  ### --- dashboardBody
                  ### --------------------------------
                  
                  shinydashboard::dashboardBody(
                    
                    # - style
                    tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                            background-color: #ffffff;
                                            }'))),
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    
                    tabItems(
                      
                      ### --- TAB: Overview
                      ### --------------------------------
                      
                      tabItem(tabName = "overview"
                      ),
                      tabItem(tabName = "similarityMap",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Similarity Map and WD Usage Volume. </b>Each bubble represents a client project. 
                                                  The size of the bubble reflects the volume of Wikidata usage in the respective project; 
                                                  a logarithmic scale is used in this plot. Projects similar in respect to the semantics of Wikidata 
                                                   usage are grouped together. Use the tools next to the plot legend to explore the plot and hover 
                                                   over bubbles for details.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(rbokeh::rbokehOutput('overviewPlotDynamic',
                                                                                 width = "100%",
                                                                                 height = "900px")
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>'),
                                       htmlOutput('updateString')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab similarityMap
                      tabItem(tabName = "mapHighlights",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Wikidata Usage Highlights. </b>Each bubble represents a client project. 
                                                    The size of the bubble reflects the volume of Wikidata usage in the respective project.<br> 
                                                    Projects similar in respect to the semantics of Wikidata usage are grouped together. 
                                                   Only top five projects (of each project type) in respect to Wikidata usage volume are labeled.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotOutput('overviewPlot',
                                                                       width = "100%",
                                                                       height = "900px")
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )                    ), ### --- END Tab mapHighlights
                      tabItem(tabName = "usageTendency",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Wikidata Usage Tendency. </b>Each bubble represents a Wikidata semantic category. 
                                                    These categories represent one possible way of categorizing the Wikidata items.
                                                   The size of the bubble reflects the volume of Wikidata usage from the respective category. 
                                                   If two categories are found in proximity, that means that the projects that tend to use the one 
                                                   also tend to use the another, and vice versa.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotOutput('usageTendencyPlot',
                                                                       width = "100%",
                                                                       height = "700px")
                                                )
                                         )
                                       )
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab usageTendency
                      tabItem(tabName = "wdUsage"
                      ),
                      tabItem(tabName = "usageDistribution",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Wikidata Usage Distribution: Project Usage Rank-Frequency. </b>
                                                   Each point represents a client project. 
                                                    Wikidata usage is represented on the vertical and the project 
                                                    usage rank on the horizontal axis. Only top projects per project type 
                                                   are labeled in the Static chart.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                tabBox(id = 'tabset_projectRankFrequencyPlot', 
                                                       selected = 'Interactive', 
                                                       width = 12,
                                                       height = NULL, 
                                                       side = "left",
                                                       tabPanel("Interactive",
                                                                fluidRow(
                                                                  column(width = 12,
                                                                         shinycssloaders::withSpinner(plotly::plotlyOutput('projectRankFrequencyPlot_interactive',
                                                                                                                           width = "100%",
                                                                                                                           height = "600px"))
                                                                  )
                                                                )
                                                       ),
                                                       tabPanel("Static",
                                                                fluidRow(
                                                                  column(width = 12,
                                                                         shinycssloaders::withSpinner(plotOutput('projectRankFrequencyPlot',
                                                                                                                 width = "100%",
                                                                                                                 height = "600px"))
                                                                  )
                                                                )
                                                       )
                                                ),
                                                br()
                                         )
                                       )
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab usageDistribution,
                      tabItem(tabName = "usageDistributionLog",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Wikidata Usage Distribution: Project Usage log(Rank)-log(Frequency). </b>
                                                   Each point represents a client project. The logarithms of Wikidata usage and project 
                                                   usage rank are represented on on the vertical and horizontal axis, respectively. 
                                                   Top three projects per project type are labeled in the Static chart.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                tabBox(id = 'tabset_projectLogRankLogFrequencyPlot', 
                                                       selected = 'Interactive', 
                                                       width = 12,
                                                       height = NULL, 
                                                       side = "left",
                                                       tabPanel("Interactive",
                                                                fluidRow(
                                                                  column(width = 12,
                                                                         shinycssloaders::withSpinner(plotly::plotlyOutput('projectLogRankLogFrequencyPlot_interactive',
                                                                                                                           width = "100%",
                                                                                                                           height = "600px"))
                                                                  )
                                                                )
                                                       ),
                                                       tabPanel("Static",
                                                                fluidRow(
                                                                  column(width = 12,
                                                                         shinycssloaders::withSpinner(plotOutput('projectLogRankLogFrequencyPlot',
                                                                                                                 width = "100%",
                                                                                                                 height = "600px"))
                                                                  )
                                                                )
                                                       )
                                                ),
                                                br()
                                         )
                                       )
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab usageDistributionLog
                      tabItem(tabName = "clientTypes",
                              fluidRow(
                                column(width = 8, 
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Client Project Types. </b>Wikidata usage breakdown across 
                                                    the client project types. Each row represents one client project type. Semantic categories of 
                                                   Wikidata items are placed on the horizontal axis, while the respective usage counts are given on the vertical axis.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                tabBox(id = 'tabset_projectCategoryCross', 
                                                       selected = 'Interactive', 
                                                       width = 12,
                                                       height = NULL, 
                                                       side = "left",
                                                       tabPanel("Interactive",
                                                                fluidRow(
                                                                  column(width = 12,
                                                                         shinycssloaders::withSpinner(plotly::plotlyOutput('projectCategoryCross_interactive',
                                                                                                                           width = "100%",
                                                                                                                           height = "800px"))
                                                                  )
                                                                )
                                                       ),
                                                       tabPanel("Static",
                                                                fluidRow(
                                                                  column(width = 12,
                                                                         shinycssloaders::withSpinner(plotOutput('projectCategoryCross',
                                                                                                                 width = "100%",
                                                                                                                 height = "800px"))
                                                                  )
                                                                )
                                                       )
                                                ),
                                                br()
                                         )
                                       )
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab clientTypes
                      tabItem(tabName = "clientVolume",
                              fluidRow(
                                column(width = 8, 
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Client Projects Usage Volume. </b>Wikidata usage across the client projects. 
                                                    Use slider (below the chart) to select the range of client projects by percentile ranks*. 
                                                    <br><b>Note:</b> The chart present at most 30 top projects (in terms of Wikidata usage volume) from the selection.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotOutput('projectVolume',
                                                                       width = "100%",
                                                                       height = "700px")
                                                ),
                                                sliderInput('volumeSlider', 
                                                            'Percentile Rank (select lower and upper limit):', 
                                                            min = 1, 
                                                            max = 100, 
                                                            value = c(95,100), 
                                                            step = 1, 
                                                            round = TRUE,
                                                            ticks = TRUE, 
                                                            animate = FALSE,
                                                            width = '100%'),
                                                HTML('<p style="font-size:80%;">*The <a href="https://en.wikipedia.org/wiki/Percentile_rank" target="_blank">percentile rank</a> 
                                                    of a score is the percentage of scores in its frequency distribution that are equal to or lower than it. 
                                                   For example, a client project that has a Wikidata usage volume greater than or equal to 75% of all client projects under
                                                   consideration is said to be at the 75th percentile, where 75 is the percentile rank.</p>')
                                         )
                                       )
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab clientVolume
                      tabItem(tabName = "tabs"),
                      tabItem(tabName = "tabs1",
                              fluidRow(
                                column(width = 8, 
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Client Project + Semantic Category Usage Cross-Tabulation. </b>
                                                  Wikidata usage breakdown across the client projects, project types, and semantic categories. 
                                                   Sort the table by any of its columns or enter a search term to find a specific project, project type, or 
                                                   Wikidata semantic category.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(DT::dataTableOutput('projectCategoryDataTable', width = "100%"))
                                         )
                                       )
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab tabs1,
                      tabItem(tabName = "tabs2",
                              fluidRow(
                                column(width = 8, 
                                       fluidRow(
                                         column(width = 12,
                                                HTML('<p style="font-size:80%;"><b>Client Project Usage Tabulation. </b>
                                               Wikidata usage per client project. 
                                               Sort the table by any of its columns or enter a search term to find a specific project or project type.</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(DT::dataTableOutput('projectDataTable', width = "100%"))
                                         )
                                       )
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab tabs2
                      tabItem(tabName = "documentation",
                              fluidRow(
                                column(width = 8,
                                       HTML('<h2>WDCM Overview Dashboard</h2>
                                          <h4>Description<h4>
                                          <hr>
                                          <h4>Introduction<h4>
                                          <br>
                                          <p style="font-size:65%;">This Dashboard is a part of the <b>Wikidata Concepts Monitor (WDMC)</b>. The WDCM system provides analytics on Wikidata usage
                                          across the client projects. The WDCM Overview Dashboard presents the big picture of Wikidata usage; other WDCM dashboards go
                                          into more detail. The Overview Dashboard provides insights into <b>(1)</b> the similarities between the client projects in respect to their use of 
                                          of Wikidata, as well as <b>(2)</b> the volume of Wikidata usage in every client project, <b>(3)</b> Wikidata usage tendencies, described by the volume of 
                                          Wikidata usage in each of the semantic categories of items that are encompassed by the current WDCM edition, <b>(4)</b> the similarities between the 
                                          Wikidata semantic categories of items in respect to their usage across the client projects, <b>(5)</b> ranking of client projects in respect to their 
                                          Wikidata usage volume, <b>(6)</b> the Wikidata usage breakdown across the types of client projects and Wikidata semantic categories.</p>
                                          <hr>
                                          <h4>Definitions</h4>
                                          <br>
                                          <p style="font-size:80%;"><b>N.B.</b> The current <b>Wikidata item usage statistic</b> definition is <i>the count of the number of pages in a particular client project
                                          where the respective Wikidata item is used</i>. Thus, the current definition ignores the usage aspects completely. This definition is motivated by the currently 
                                          present constraints in Wikidata usage tracking across the client projects 
                                          (see <a href = "https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target = "_blank">Wikibase/Schema/wbc entity usage</a>). 
                                          With more mature Wikidata usage tracking systems, the definition will become a subject 
                                          of change. The term <b>Wikidata usage volume</b> is reserved for total Wikidata usage (i.e. the sum of usage statistics) in a particular 
                                          client project, group of client projects, or semantic categories. By a <b>Wikidata semantic category</b> we mean a selection of Wikidata items that is 
                                          that is operationally defined by a respective SPARQL query returning a selection of items that intuitivelly match a human, natural semantic category. 
                                          The structure of Wikidata does not necessarily match any intuitive human semantics. In WDCM, an effort is made to select the semantic categories so to match 
                                          the intuitive, everyday semantics as much as possible, in order to assist anyone involved in analytical work with this system. However, the choice of semantic 
                                          categories in WDCM is not necessarily exhaustive (i.e. they do not necessarily cover all Wikidata items), neither the categories are necessarily 
                                          mutually exclusive. The Wikidata ontology is very complex and a product of work of many people, so there is an optimization price to be paid in every attempt to 
                                          adapt or simplify its present structure to the needs of a statistical analytical system such as WDCM. The current set of WDCM semantic categories is thus not 
                                          normative in any sense and can become a subject of change in any moment, depending upon the analytical needs of the community.</p>
                                          <p style="font-size:80%;">The currently used <b>WDCM Taxonomy</b> of Wikidata items encompasses the following 14 semantic categories: <i>Geographical Object</i>, <i>Organization</i>, <i>Architectural Structure</i>, 
                                          <i>Human</i>, <i>Wikimedia</i>, <i>Work of Art</i>, <i>Book</i>, <i>Gene</i>, <i>Scientific Article</i>, <i>Chemical Entities</i>, <i>Astronomical Object</i>, <i>Thoroughfare</i>, <i>Event</i>, 
                                          and <i>Taxon</i>.</p>
                                          <hr>
                                          <h4>Wikidata Usage Overview</h4>
                                          <br>
                                          <p style="font-size:80%;">The similarity structure in Wikidata usage <i>across the client projects</i> is presented. Each bubble represents a client project.
                                          The size of the bubble reflects the volume of Wikidata usage in the respective project. Projects similar in respect to the semantics of Wikidata
                                          usage are grouped together.<br>
                                          The bubble chart is produced by performing a <a href="https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding" target="_blank">t-SNE dimensionality reduction</a> 
                                          of the client project pairwise Euclidean distances derived from the Projects x Categories contingency table. Given that the original higher-dimensional space 
                                          from which the 2D map is derived is rather constrained by the choice of a small number of semantic categories, the similarity mapping is somewhat 
                                          imprecise and should be taken as an attempt at an approximate big picture of the client projects similarity structure only. More precise 2D maps of 
                                          the similarity structures in client projects are found on the <a href = "http://wdcm.wmflabs.org/WDCM_SemanticsDashboard/" target = "_blank">WDCM Semantics Dashboard</a>, where each semantic category first receives an 
                                          <a href = "https://en.wikipedia.org/wiki/Topic_model" target = "_blank">LDA Topic Model</a>, 
                                          and the similarity structure between the client projects is then derived from project topical distributions.<br>
                                          While the <i>Explore</i> tab presents a dynamic <a href = "http://hafen.github.io/rbokeh/" target="_blank">{Rbokeh}</a> visualization alongside 
                                          the tools to explore it in detail, the <i>Highlights</i> tab shows a static <a href = "http://ggplot2.org/" target="_blank">{ggplot2}</a> plot with the most important client projects 
                                          marked (<b>NOTE.</b> Only top five projects (of each project type) in respect to Wikidata usage volume are labeled).</p>
                                          <hr>
                                          <h4>Wikidata Usage Tendency</h4>
                                          <br>
                                          <p style="font-size:80%;">The similarity structure in Wikidata usage <i>across the semantic categories</i> is presented. Each bubble represents a Wikidata semantic
                                          category. The size of the bubble reflects the volume of Wikidata usage from the respective category. If two categories are found in proximity,
                                          that means that the projects that tend to use the one also tend to use the another, and vice versa. Similarly to the Usage Overview, the 2D mapping is obtained by performing 
                                          a <a href="https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding" target="_blank">t-SNE dimensionality reduction</a> 
                                          of the pairwise category Euclidean distances derived from the Projects x Categories contingency table.</p>
                                          <hr>
                                          <h4>Wikidata Usage Distribution</h4>
                                          <br>
                                          <p style="font-size:80%;">The plots are helpful to build an understanding of the relative range of Wikidata usage across the client projects.
                                          In the <i>Project Usage Rank-Frequency</i> plot, each point represents a client project; Wikidata usage is represented on the vertical and
                                          the project usage rank on the horizontal axis, while only top project (per project type) are labeled. The highly-skewed, asymmetrical
                                          distribution reveals that a small fraction of client projects only accounts for a huge proportion of Wikidata usage.<br> In the
                                          <i>Project Usage log(Rank)-log(Frequency)</i> plot, the logarithms of both variables are represented. 
                                          A <a href = "https://en.wikipedia.org/wiki/Power_law" target="_blank">power-law</a> relationship holds true if this
                                          plot is linear. The plot includes the best linear fit, however, no attempts to estimate the underlying probability distribution were made.</p>
                                          <hr>
                                          <h4>Client Project Types</h4>
                                          <br>
                                          <p style="font-size:80%;">Project types are provided in the rows of this chart, while the semantic categories are given on the horizontal axis.
                                          The height of the respective bar indicates Wikidata usage volume from the respective semantic category in a particular client project.</p>
                                          <hr>
                                          <h4>Client Projects Usage Volume</h4>
                                          <br>
                                          <p style="font-size:80%;">Use the slider to select the percentile rank range of the Wikidata usage volume distribution across the client project to show. The
                                          chart will automatically adjust to present the selected projects in increasing order of Wikidata usage, and presenting at most 30 top projects
                                          from the selection. <b>NOTE.</b> The <a href="https://en.wikipedia.org/wiki/Percentile_rank" target="_blank">percentile rank</a> 
                                          of a score is the percentage of scores in its frequency distribution that are equal to or lower than it. 
                                          For example, a client project that has a Wikidata usage volume greater than or equal to 75% of all client projects under
                                          consideration is said to be at the 75th percentile, where 75 is the percentile rank.<br> In effect, you can browse the whole 
                                          distribution of Wikidata usage across the client projects by selecting the lower and uppers limit in terms of usage percentile rank.</font>
                                          <hr>
                                          <h4>Tabs/Crosstabs</h4>
                                          <br>
                                          <p style="font-size:80%;">A breakdown of Wikidata usage statistics across client projects and semantic categories. First, 
                                          a table that presents a Client Project vs. Semantic Category cross-tabulation. The Usage column in this table is the Wikidata 
                                          usage statistic for a particular Semantic Category x Client Project combination (e.g. The Wikidata usage in the category "Human" in 
                                          the dewiki project). The second table presents the total Wikidata usage per client project (i.e. the sum of Wikidata usage across 
                                          all semantic categories for a particular client project; e.g. the total Wikidata usage volume of enwiki).</p>')
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END documentation
                      tabItem(tabName = "navigation",
                              fluidRow(
                                column(width = 6,
                                       includeMarkdown(system.file("app/www/wdcmNavigate.html", 
                                                                   package = "WDCMOverviewDashboard")),
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 1,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      )
                      
                    ) ### --- END tab Navigate
                    
                  ) ### --- END dashboardBody
                  
    ) ### --- END dashboardPage
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'WDCM_OverviewDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

