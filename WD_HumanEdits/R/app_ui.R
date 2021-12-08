### ---------------------------------------------------------------------------
### --- WD Human vs Bot Edits
### --- Version 1.0.0
### --- 2019.
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
        column(width = 6,
               br(),
               uiOutput("logo"),
               HTML('<p style="font-size:100%;"align="left"><b>Human vs Bot Edits per Class in Wikidata</b></p>'
               ),
               htmlOutput('timestamp')
        )
      ),
      fluidRow(
        column(width = 6,
               includeMarkdown(system.file("app/www/dashboard_header.html", 
                                           package = "WDHumanEdits")),
               hr()
        )
      ),
      fluidRow(
        column(width = 6,
               shinycssloaders::withSpinner(plotOutput('human_to_bot_ratio', height = 550)),
               br(),
               includeMarkdown(system.file("app/www/human_to_bot_ratio_legend.html", 
                                           package = "WDHumanEdits")),
               hr()
        ),
        column(width = 6,
               shinycssloaders::withSpinner(plotOutput('proportion_items_touched', height = 550)),
               includeMarkdown(system.file("app/www/proportion_items_touched_legend.html", 
                                           package = "WDHumanEdits")),
        )
      ),
      fluidRow(
        column(width = 6,
               shinydashboard::tabBox(id = 'tabset_median_unique_editors', 
                                      selected = 'Static', 
                                      width = 12,
                                      height = NULL, 
                                      side = "left",
                                      tabPanel("Interactive",
                                               fluidRow(
                                                 column(width = 12,
                                                        shinycssloaders::withSpinner(plotly::plotlyOutput('median_unique_editors_interactive',
                                                                                                          width = "100%",
                                                                                                          height = "600px"))
                                                 )
                                               )
                                      ),
                                      tabPanel("Static",
                                               fluidRow(
                                                 column(width = 12,
                                                        shinycssloaders::withSpinner(plotOutput('median_unique_editors',
                                                                                                width = "100%",
                                                                                                height = "600px"))
                                                 )
                                               )
                                      )
               ),
               br(), br(),
               includeMarkdown(system.file("app/www/median_unique_editors_legend.html", 
                                           package = "WDHumanEdits")),
               hr()
        ),
        column(width = 6,
               shinydashboard::tabBox(id = 'tabset_total_edits_class', 
                      selected = 'Static', 
                      width = 12,
                      height = NULL, 
                      side = "left",
                      tabPanel("Interactive",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotly::plotlyOutput('total_edits_class_interactive',
                                                                                          width = "100%",
                                                                                          height = "600px"))
                                 )
                               )
                      ),
                      tabPanel("Static",
                               fluidRow(
                                 column(width = 12,
                                        shinycssloaders::withSpinner(plotOutput('total_edits_class',
                                                                                width = "100%",
                                                                                height = "600px"))
                                 )
                               )
                      )
               ),
               includeMarkdown(system.file("app/www/total_edits_class_legend.html", 
                                           package = "WDHumanEdits")),
        ),
        hr()
      ),
      fluidRow(
        column(width = 12,
               hr(),
               HTML('<p style="font-size:100%;"align="left"><b>Human vs Bot Edits Statistics</b></p>'),
               includeMarkdown(system.file("app/www/table_legend.html", 
                                           package = "WDHumanEdits")),
               DT::dataTableOutput('overviewDT', width = "100%"),
               hr(),
               HTML('<p style="font-size:80%;"align="left"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
               hr(),
               br(),
               hr()
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
      app_title = 'WDHumanEdits'
    )
  )
}

