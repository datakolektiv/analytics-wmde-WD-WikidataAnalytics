### ---------------------------------------------------------------------------
### --- WD Game: Reference Treasure Hunt
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
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
                                           package = "WDGameReferenceHunt")),
        )
      ),
      fluidRow(
        column(width = 6,
               hr(),
               HTML('<h4>Item x Property x Value x Source x Choice Dataset</h4>
             <p style="font-size:80%;"align="left">Columns:
             <b>Item: </b>Wikidata item, <b>Property: </b>Wikidata property of the Item, 
             <b>Extracted Data (JSON): </b>Extracted value for the respective property and item, 
             <b>accepted: </b>How many decisions to accept the proposed value, 
             <b>rejected: </b>How many decisions to reject the proposed value, 
             <b>ratio: </b>accepted divided by rejected, 
             <b>percent_accepted: </b>the percent of accepted in accepted + rejected total, 
             <b>total_decisions: </b>how many assessments were provided in total, 
             <b>is_accepted: </b>is the proposed change acceptable given the criterion (described in the 
             dashboard\'s header).</p>'),
               DT::dataTableOutput('full_ratio', width = "100%"),
               downloadButton('full_ratio_download',
                              'Download (csv)'),
               br(), br(),
               HTML('<p style="font-size:80%;"align="left"><b>Note.</b> Please specify single quote as a string delimiter when opening this .csv file.</p>'),
               hr(),
               HTML('<h4>Per Property Statistics</h4>
             <p style="font-size:80%;"align="left">The following table presents the data aggregated across 
             the Wikidata properties. Columns: 
             <b>Property: </b>Wikidata property, 
             <b>accepted: </b>How many decisions to accept the proposed value, 
             <b>rejected: </b>How many decisions to reject the proposed value, 
             <b>ratio: </b>accepted divided by rejected, 
             <b>percent_accepted: </b>the percent of accepted in accepted + rejected total, 
             <b>total_decisions: </b>how many assessments were provided in total. 
             </p>'),
               DT::dataTableOutput('property_ratio', width = "100%"),
               downloadButton('property_ratio_download',
                              'Download (csv)'),
               hr(),
               HTML('<h4>Per Datatype Statistics</h4>
             <p style="font-size:80%;"align="left">The following table presents the data aggregated across 
             the Wikidata datatypes. Columns: 
             <b>datatype: </b>Wikidata datatype, 
             <b>accepted: </b>How many decisions to accept the proposed value, 
             <b>rejected: </b>How many decisions to reject the proposed value, 
             <b>ratio: </b>accepted divided by rejected, 
             <b>percent_accepted: </b>the percent of accepted in accepted + rejected total, 
             <b>total_decisions: </b>how many assessments were provided in total. 
             </p>'),
               DT::dataTableOutput('datatype_ratio', width = "100%"),
               downloadButton('datatype_ratio_download',
                              'Download (csv)'),
               hr(),
               HTML('<p style="font-size:80%;"align="left"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
               hr(),
               includeMarkdown(system.file("app/www/technical_notes.html",
                                           package = "WDGameReferenceHunt")),
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
      app_title = 'WD_referenceHunt'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

