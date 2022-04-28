### ---------------------------------------------------------------------------
### --- Qurator: Current Events
### --- Version 1.0.0
### --- 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

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
        column(width = 12,
               br(),
               h1("Wikidata Current Events"),
               )
      ),
      
      fluidRow(
        column(width = 12,
               br(),
               includeMarkdown(system.file("app/www/dashboard_header.html",
                                           package = "QuratorCurentEvents")),
               )
      ),
      
      fluidRow(
        column(width = 12,
               tabsetPanel(type = "tabs",
                           tabPanel("72h",
                                    fluidRow(
                                      column(width = 12,
                                             br(),
                                             shinycssloaders::withSpinner(DT::dataTableOutput("hours72_update"))
                                      )
                                    )
                           ),
                           
                           tabPanel("48h",
                                    
                                    fluidRow(
                                      column(width = 12,
                                             br(),
                                             shinycssloaders::withSpinner(DT::dataTableOutput("hours48_update"))
                                      )
                                    )
                           ),
                           
                           tabPanel("24h",
                                    fluidRow(
                                      column(width = 12,
                                             br(),
                                             shinycssloaders::withSpinner(DT::dataTableOutput("hours24_update"))
                                      )
                                    )
                           ),
                           
                           tabPanel("6h", 
                                    fluidRow(
                                      column(width = 12,
                                             br(),
                                             shinycssloaders::withSpinner(DT::dataTableOutput("hours6_update"))
                                      )
                                    )
                                    
                           ),
                           )
               ),
        ),
      fluidRow(
        column(width = 12,
               htmlOutput("updateTimestamp"),
               hr()
               )
      ),
      fluidRow(
        div(style = "background-color: #ABBAEA;"),
        column(width = 3,
               HTML('<b>About Current Events Dashboard</b>
                    <br>
                    <br>
                    <a href="https://github.com/wikimedia/analytics-wmde-WD-WikidataAnalytics/blob/master/LICENSE" 
                    target = "_blank">Licensed under BSD 3-Clause License</a>
                    <br>
                    <a href="https://github.com/wikimedia/analytics-wmde-WD-WikidataAnalytics/tree/master/Qurator_CurrentEvents" 
                    target = "_blank">View Source</a>
                    <br>
                    <a href="https://phabricator.wikimedia.org/maniphest/task/edit/form/1/?title=Wikidata%20Current%20Events%20Dashboard&description=%2A%2AMain%20components%3A%2A%2A%20%0A%2A%20Current%20Events%20Dashboard%0A%0A%2A%2AProblem%3A%2A%2A%0APlease%20describe%20the%20problem.%20If%20possible%20include%20steps%20to%20reproduce%20and%20give%20an%20example.%0A%0A%2A%2AScreenshots%3A%2A%2A%0AIf%20possible%2C%20please%20add%20a%20screenshot%20that%20illustrates%20the%20problem.%20&projects=Wikidata_Analytics" 
                    target = "_blank">Report an issue</a>
                    ')
               ),
        column(width = 3,
               HTML('<b>About us</b>
                    <br>
                    <br>
                    <a href="https://foundation.wikimedia.org/wiki/Non-wiki_privacy_policy" 
                    target = "_blank">Privacy Policy</a>
                    <br>
                    <a href="https://www.wikimedia.de/" 
                    target = "_blank">Wikimedia Deutschland</a>
                    <br>
                    <a href="https://www.wikidata.org/wiki/Wikidata:Report_a_technical_problem" 
                    target = "_blank">Made by the Wikidata Team</a>')
               ),
        column(width = 3,
               HTML('<b>More Data Quality Tools</b>
                    <br>
                    <br>
                    <a href="https://query.wikidata.org/querybuilder/" 
                    target = "_blank">Query Builder</a>
                    <br>
                    <a href="https://item-quality-evaluator.toolforge.org/" 
                    target = "_blank">Item Quality Evaluator</a>
                    <br>
                    <a href="https://wikidata-analytics.wmcloud.org/app/CuriousFacts" 
                    target = "_blank">Curious Facts</a>')
               )
      ),
      fluidRow(
        column(width = 12,
               HTML("<br><br>")
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

