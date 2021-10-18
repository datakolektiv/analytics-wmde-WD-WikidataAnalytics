### ---------------------------------------------------------------------------
### --- Qurator: Current Events
### --- Version 1.0.0
### --- 2021.
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
        column(width = 6,
               br(),
               tags$img(src = "www/Wikidata-logo-en.png"),
               includeMarkdown(system.file("app/www/dashboard_header.html", 
                                           package = "QuratorCurentEvents")),
               # hr(),
               htmlOutput("updateTimestamp"),
               htmlOutput("wd"),
               hr()
        )
      ),
        
            tabsetPanel(type = "tabs",
                        
                        tabPanel("6 hours", 
                                 
                                 fluidRow(
                                   column(width = 12,
                                          h3("Frequently revised items in the previous 6 hours"),
                                          hr(),
                                          includeMarkdown(system.file("app/www/last_6hours.html", 
                                                                      package = "QuratorCurentEvents")),
                                          br(), br(),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("hours6_update"))
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(width = 12,
                                          hr(),
                                          HTML('<p style="font-size:80%;"><b>Wikidata Current Events :: Wikidata, WMDE 2020</b><br></p>'),
                                          HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                                  <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                          br(), br()
                                   )
                                 )
                                 
                        ),
                        
                        tabPanel("24 hours",
                                 
                                 fluidRow(
                                   column(width = 12,
                                          h3("Frequently revised items in the previous 24 hours"),
                                          hr(),
                                          HTML("<b>Updated every minute</b>"),
                                          includeMarkdown(system.file("app/www/last_24hours.html", 
                                                                      package = "QuratorCurentEvents")),
                                          br(), br(),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("hours24_update"))
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(width = 12,
                                          hr(),
                                          HTML('<p style="font-size:80%;"><b>Wikidata Current Events :: Wikidata, WMDE 2020</b><br></p>'),
                                          HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                                  <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                          br(), br()
                                          )
                                   )
                                 ),
                        
                        tabPanel("48 hours",
                                 
                                 fluidRow(
                                   column(width = 12,
                                          h3("Frequently revised items in the previous 48 hours"),
                                          hr(),
                                          HTML("<b>Updated every minute</b>"),
                                          includeMarkdown(system.file("app/www/last_48hours.html", 
                                                                      package = "QuratorCurentEvents")),
                                          br(), br(),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("hours48_update"))
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(width = 12,
                                          hr(),
                                          HTML('<p style="font-size:80%;"><b>Wikidata Current Events :: Wikidata, WMDE 2020</b><br></p>'),
                                          HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                                  <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                          br(), br()
                                   )
                                 )
                        ),
                        
                        tabPanel("72 hours",
                                 
                                 fluidRow(
                                   column(width = 12,
                                          h3("Frequently revised items in the previous 72 hours"),
                                          hr(),
                                          HTML("<b>Updated every minute</b>"),
                                          includeMarkdown(system.file("app/www/last_72hours.html", 
                                                                      package = "QuratorCurentEvents")),
                                          br(), br(),
                                          shinycssloaders::withSpinner(DT::dataTableOutput("hours72_update"))
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(width = 12,
                                          hr(),
                                          HTML('<p style="font-size:80%;"><b>Wikidata Current Events :: Wikidata, WMDE 2020</b><br></p>'),
                                          HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                                  <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                          br(), br()
                                   )
                                 )
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
      app_title = 'Qurator_CurrentEvents'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

