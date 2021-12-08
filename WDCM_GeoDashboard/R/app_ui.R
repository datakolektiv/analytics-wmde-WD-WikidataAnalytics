### ---------------------------------------------------------------------------
### --- WDCM Geo
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

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
    dashboardPage(skin = "black",
                  
                  ### --- dashboarHeader
                  ### --------------------------------
                  
                  dashboardHeader(
                    # - Title
                    title = "WDCM: Geo",
                    titleWidth = 230
                  ), 
                  ### ---- END dashboardHeader
                  
                  ### --- dashboardSidebar
                  ### --------------------------------
                  
                  dashboardSidebar(
                    sidebarMenu(
                      id = "tabsWDCM",
                      menuItem(text = "Dashboard", 
                               tabName = "dashboard", 
                               icon = icon("barcode"),
                               selected = TRUE
                               
                      ),
                      menuItem(text = "Description", 
                               tabName = "description", 
                               icon = icon("barcode"),
                               selected = FALSE
                      ),
                      menuItem(text = "Navigate WDCM", 
                               tabName = "navigate", 
                               icon = icon("barcode"),
                               selected = FALSE
                      )
                    )
                  ),
                  ### --- END dashboardSidebar
                  
                  ### --- dashboardBody
                  ### --------------------------------
                  
                  dashboardBody(
                    
                    # - style
                    tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                            background-color: #ffffff;
                                            }'))),
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    
                    tabItems(
                      
                      ### --- TAB: Dashboard
                      ### --------------------------------
                      
                      tabItem(tabName = "dashboard",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 3,
                                                selectizeInput("selectCategory",
                                                               "Select Item Category:",
                                                               multiple = F,
                                                               choices = NULL,
                                                               selected = NULL
                                                )
                                         ),
                                         column(width = 9,
                                                br(),
                                                HTML('<p style="font-size:80%;"align="left">Select Wikidata item category, and the Dashboard will generate an interactive map 
                                                    of geo-localized Wikidata items  alongside their Wikidata usage 
                                                    statistics. Click the item marker for details. <br><b>Note.</b> The usage statistic is the number of 
                                                    pages that make use of the respective item across all Wikimedia projects.</p>')
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>'),
                                       htmlOutput('updateInfo')
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       tabBox(id = 'tabset1', 
                                              selected = 'Map', 
                                              width = 12,
                                              height = NULL, 
                                              side = "left",
                                              tabPanel("Map",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(leaflet::leafletOutput("wdcmMap", width = "100%", height = 800))
                                                         )
                                                       )
                                              ),
                                              tabPanel("Data",
                                                       fluidRow(
                                                         column(width = 12,
                                                                downloadButton('mapDataCSV',
                                                                               'Data (csv)'),
                                                                br(), br(),
                                                                shinycssloaders::withSpinner(DT::dataTableOutput('mapData', width = "100%"))
                                                         )
                                                       )
                                              )
                                       )
                                )
                              ),
                              
                              fluidRow(
                                hr(),
                                column(width = 2,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 10,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Geo :: Wikidata, WMDE 2017</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab: dashboard
                      
                      tabItem(tabName = "navigate",
                              fluidRow(
                                column(width = 6,
                                       includeMarkdown(system.file("app/www/wdcmNavigate.html", 
                                                                   package = "WDCMGeoDashboard")),
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
                                column(width = 2,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 10,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Geo :: Wikidata, WMDE 2017</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab item navigate
                      tabItem(tabName = "description",
                              fluidRow(
                                column(width = 8,
                                       HTML('<h2>WDCM Geo Dashboard</h2>
                                          <h4>Description<h4>
                                          <hr>
                                          <h4>Introduction<h4>
                                          <br>
                                          <p><font size = 2>This Dashboard is a part of the <b>Wikidata Concepts Monitor (WDMC)</b>. The WDCM system provides analytics on Wikidata usage
                                          across the Wikimedia sister projects. The WDCM Geo Dashboard collects several categories of Wikidata items that have geographical coordinates data and 
                                          presents them on an interactive <a href = "http://leafletjs.com/" target = "_blank">Leaflet map</a> alongside their usage statistics. To understand the WDCM usage statistics, check out the Definitions 
                                          section. The WDCM Geo Dashboard uses <a href = "https://www.openstreetmap.org/" target = "_blank">OpenStreetMap</a>.
                                          </font></p>
                                          <hr>
                                          <h4>Definitions</h4>
                                          <br>
                                          <p><font size = 2><b>N.B.</b> The current <b>Wikidata item usage statistic</b> definition is <i>the count of the number of pages in a particular client project
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
                                          normative in any sense and a subject  of change in any moment, depending upon the analytical needs of the community.</font></p>
                                          <p>The currently used <b>WDCM Taxonomy</b> of Wikidata items encompasses the following 14 semantic categories: <i>Geographical Object</i>, <i>Organization</i>, <i>Architectural Structure</i>, 
                                          <i>Human</i>, <i>Wikimedia</i>, <i>Work of Art</i>, <i>Book</i>, <i>Gene</i>, <i>Scientific Article</i>, <i>Chemical Entities</i>, <i>Astronomical Object</i>, <i>Thoroughfare</i>, <i>Event</i>, 
                                          and <i>Taxon</i>.</p>
                                          ')
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 2,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 10,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Geo :: Wikidata, WMDE 2017</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ) # - end tab Item Description
                      
                    ) # - end Tab Items
                    
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
      app_title = 'WDCM_GeoDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

