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

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    fluidPage(
      
      fluidRow(
        column(width = 7,
               br(),
               uiOutput("logo"),
               includeMarkdown(system.file("app/www/dashboard_header.html",
                                       package = "WDUsageCoverage")),
               hr(),
               htmlOutput('timestamp'),
               htmlOutput('overall'),
               htmlOutput('coverage')
        )
      ),
      fluidRow(
        column(width = 6,
               hr(),
               DT::dataTableOutput('overviewDT', width = "100%"),
               hr(),
               HTML('<p style="font-size:80%;"align="left"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
               hr(),
               includeMarkdown(system.file("app/www/technical_notes.html",
                                           package = "WDUsageCoverage")),
               br(),
               hr()
        ),
        column(width = 6,
               hr(),
               shinycssloaders::withSpinner(DT::dataTableOutput('overviewDT_projectType', width = "100%")),
               HTML('<p style="font-size:80%;"align="left"><b>Note. </b>Percents of articles that make use of Wikidata 
                  and percents of articles with Sitelinks, per WMF project type.</p>'),
               hr(),
               shinydashboard::tabBox(id = 'tabset_percentProjectType', 
                      selected = 'Interactive', 
                      width = 12,
                      height = NULL, 
                      side = "left",
                      tabPanel("Interactive",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotly::plotlyOutput('percentProjectType_interactive',
                                                                                          width = "100%",
                                                                                          height = "600px"))
                                 )
                               )
                      ),
                      tabPanel("Static",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotOutput('percentProjectType',
                                                                                width = "100%",
                                                                                height = "600px"))
                                 )
                               )
                      )
               ),
               HTML('<p style="font-size:80%;"align="left"><b>Note. </b> Percents refer to the count of articles that use WD 
                  relative to the total number of articles in a given Project Type.</p>'),
               hr(),
               shinydashboard::tabBox(id = 'tabset_wdUsagePropPerProject', 
                      selected = 'Interactive', 
                      width = 12,
                      height = NULL, 
                      side = "left",
                      tabPanel("Interactive",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotly::plotlyOutput('wdUsagePropPerProject_interactive',
                                                                                          width = "100%",
                                                                                          height = "600px"))
                                 )
                               )
                      ),
                      tabPanel("Static",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotOutput('wdUsagePropPerProject',
                                                                                width = "100%",
                                                                                height = "600px"))
                                 )
                               )
                      )
               ), 
               HTML('<p style="font-size:80%;"align="left"><b>Note. </b> The pie chart represents distribution of total WD usage 
                  (in % of total WD usage) across the Project Types.</p>'),
               hr(),
               shinydashboard::tabBox(id = 'tabset_wdUsagePerProject', 
                      selected = 'Interactive', 
                      width = 12,
                      height = NULL, 
                      side = "left",
                      tabPanel("Interactive",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotly::plotlyOutput('wdUsagePerProject_interactive',
                                                                                          width = "100%",
                                                                                          height = "600px"))
                                 )
                               )
                      ),
                      tabPanel("Static",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotOutput('wdUsagePerProject',
                                                                                width = "100%",
                                                                                height = "600px"))
                                 )
                               )
                      )
               ),
               HTML('<p style="font-size:80%;"align="left"><b>Note. </b> The chart represents the top 20 Wikimedia Projects per 
                  WD usage.</p>'),
               hr(),
               shinycssloaders::withSpinner(plotOutput('wdUsagePropPerProjectType_Polar',
                                      width = "100%", height = "600")), 
               HTML('<p style="font-size:80%;"align="left"><b>Note. </b> The chart represents the top 20 Wikimedia Projects per 
                  proprotion of WD usage relative to the total number of articles in them.</p>')
        )
      )
      
    )
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
      app_title = 'WD_percentUsageDashboard',
      all_files = TRUE
    )
  )
}

