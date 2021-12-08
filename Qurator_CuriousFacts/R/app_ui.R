### ---------------------------------------------------------------------------
### --- Qurator: Curious Facts
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
               br(), br(),
               HTML('<p style="font-size:400%;"align="left"><b>FOR REAL, WIKIDATA..?</b></p>'),
               br(), br()
        )
      ),
      fluidRow(
        column(width = 6,
               HTML('<p style="font-size:200%;"align="left"><b>Curious Facts</b></p>
                  <p style="font-size:120%;"align="left"><b>The world is weird. Wikidata describes that weird world. 
                        Wikidata also sometimes makes mistakes. Let\'s find out which is which together!
                        Here are a bunch of statements from Wikidata that are potentially weird. 
                        Have a look. Is it true? Cool, you learned something new about the world! 
                        Is it false? Can you go and fix Wikidata? <3 (If you don\'t know how, 
                        let us know on the <a href = "https://www.wikidata.org/wiki/Wikidata:Project_chat" 
                    target = "_blank">[Project chat]</a>.</b>)</p>'),
        ),
        column(width = 3, 
               tags$img(src = "www/Wikidata-logo-en.png")
        ),
      ),
      fluidRow(
        column(width = 12,
               hr()
        )
      ),
      fluidRow(
        column(width = 6,
               htmlOutput('fact'), 
               hr(), 
               tags$img(src = "www/refresh.png"),
               actionButton("button", "Give me some new weird stuff!")
        ), 
        column(width = 6,
               br(),
               shinycssloaders::withSpinner(htmlOutput('factImage'))
        )
      ),
      fluidRow(
        column(width = 10,
               hr(),
               HTML('<p style="font-size:85%;"><b>NOTE.</b> None of the facts in Wikidata exposed on this page are necessarily false - 
                   or represent mistakes.
                        In fact, we have obtained these facts by searching the <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">
                        Wikidata JSON dump copy in the WMF DataLake</a> for various types of anomalies that we have assumed migth exist, or 
                        even by knowing that something presents a violation of some important convention on how should Wikidata be structured. 
                        Please be aware of the fact that searching though whole Wikidata for anomalies is a time consuming and costly process. 
                        It cannot be performed in real time to produce the datasets such as the ones that we present here. Instead, the production of 
                        the datasets presented here relies on parallel processing of many facts found in the current snapshot of the Wikidata JSON dump 
                        (following its transfer into a Big Data store i.e. the WMF Data Lake). As a consequence, some of the facts provided have possibly 
                        already changed in Wikidata.</p>')
        )
      ),
      fluidRow(
        column(width = 10,
               hr(),
               HTML('<p style="font-size:80%;"><b>Curious Facts :: Wikidata, WMDE 2021</b><br></p>'),
               HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
               br(), br()
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
      app_title = 'Qurator_CuriousFacts'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

