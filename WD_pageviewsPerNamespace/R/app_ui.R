### ---------------------------------------------------------------------------
### --- WD Pageviews per Namespace
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
    golem_add_external_resources()
  )
  
  fluidPage(
    # Your application UI logic 
    fluidRow(
      column(width = 12,
             br(),
             uiOutput("logo"),
             HTML('<p style="font-size:80%;"align="left"><b>Pageviews per namespace from Wikidata</b><br>
                  The origin of the data is the <a href = "https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Pageview_hourly" target = "_blank">wmf.pageview_hourly</a> table.<br>
                  This dashboard runs on regular daily updates.<br>
                  <b>Note.</b> The data reported here will differ from the <a href="https://stats.wikimedia.org/v2/#/all-projects" target="_blank">Wikistats 2</a> 
                  data because we consider only selected namespaces.</p>'),
             htmlOutput('timestamp'),
             hr()
             )
      ),
    fluidRow(
      column(width = 12,
             h4("Namespace: Item (0)"),
             hr())
    ),
    fluidRow(
      column(width = 6,
             mod_dygraph_ui("dygraphDesktop_Item0"),
             hr()
      ),
      column(width = 6,
             mod_dygraph_ui("dygraphMobile_Item0"),
             hr()
      )
    ),
    fluidRow(
      column(width = 12,
             h4("Namespace: Property (120)"),
             hr())
    ),
    fluidRow(
      column(width = 6,
             mod_dygraph_ui("dygraphDesktop_Property120"),
             hr()
      ),
      column(width = 6,
             mod_dygraph_ui("dygraphMobile__Property120"),
             hr()
      )
    ),
    fluidRow(
      column(width = 12,
             h4("Namespace: Lexeme (146)"),
             hr())
    ),
    fluidRow(
      column(width = 6,
             mod_dygraph_ui("dygraphDesktop_Lexeme146"),
             hr()
      ),
      column(width = 6,
             mod_dygraph_ui("dygraphMobile_Lexeme146"),
             hr()
      )
    ),
    fluidRow(
      column(width = 12,
             h4("Namespace: EntitySchema (640)"),
             hr())
    ),
    fluidRow(
      column(width = 6,
             mod_dygraph_ui("dygraphDesktop_EntitySchema640"),
             hr()
      ),
      column(width = 6,
             mod_dygraph_ui("dygraphMobile_EntitySchema640"),
             hr()
      )
    ),
    fluidRow(
      column(width = 6,
             HTML('<p style="font-size:80%;"align="left"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
             hr(),
             br(),
             hr()
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
golem_add_external_resources <- function() {
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'WDPageviews'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
  
}

