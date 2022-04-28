### ---------------------------------------------------------------------------
### --- WDCM Statements
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
                    title = "WDCM: Statements",
                    titleWidth = 230
                  ), 
                  ### ---- END dashboardHeader
                  
                  ### --- dashboardSidebar
                  ### --------------------------------
                  
                  shinydashboard::dashboardSidebar(
                    sidebarMenu(
                      id = "tabsWDCM",
                      menuItem(text = "Properties", 
                               tabName = "properties", 
                               icon = icon("barcode"),
                               selected = TRUE,
                               menuSubItem('In Wikidata',
                                           tabName = 'properties_in_wikidata',
                                           icon = icon('line-chart')
                               ),
                               menuSubItem('Reuse',
                                           tabName = 'properties_reuse',
                                           icon = icon('line-chart')
                               ),
                               menuSubItem('Dataset',
                                           tabName = 'properties_dataset',
                                           icon = icon('line-chart')
                               )
                               
                      ),
                      menuItem(text = "Items", 
                               tabName = "items", 
                               icon = icon("barcode"),
                               selected = TRUE,
                               menuSubItem('In Wikidata',
                                           tabName = 'items_in_wikidata',
                                           icon = icon('line-chart')
                               ),
                               menuSubItem('Reuse',
                                           tabName = 'items_reuse',
                                           icon = icon('line-chart')
                               ), 
                               menuSubItem('Dataset',
                                           tabName = 'items_dataset',
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
                      
                      ### --- TAB: Properties
                      ### --------------------------------
                      
                      tabItem(tabName = "properties"
                      ),
                      tabItem(tabName = "properties_in_wikidata",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 9,
                                                HTML('<p style="font-size:80%;"><b>Property use in Wikidata claims.</b> 
                                                   The whole Wikidata JSON dump 
                                                   (<a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">copy in hdfs</a>) 
                                                   is searched for all property use cases made in claims. The chart presents the top 100 
                                                   most frequently used properties, while the complete dataset is 
                                                   available from the Dataset tab.</p>')
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotly::plotlyOutput('property_use_in_claims',
                                                                                 width = "100%",
                                                                                 height = "900px")
                                                )
                                         )
                                       ), 
                                       fluidRow(
                                         column(width = 9,
                                                HTML('<p style="font-size:80%;"><b>Property use in Wikidata qualifiers.</b> 
                                                   The whole Wikidata JSON dump 
                                                   (<a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">copy in hdfs</a>) 
                                                   is searched for all property use cases made in qualifiers. The chart presents the top 100 
                                                   most frequently used properties, while the complete dataset is 
                                                   available from the Dataset tab.</p>')
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotly::plotlyOutput('property_use_in_qualifiers',
                                                                                                  width = "100%",
                                                                                                  height = "900px")
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 9,
                                                hr(),
                                                HTML('<p style="font-size:80%;"><b>Property use in references.</b> 
                                                   The whole Wikidata JSON dump 
                                                   (<a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">copy in hdfs</a>) 
                                                   is searched for all property use cases made in references. The chart presents the top 100 
                                                   most frequently used properties, while the complete dataset is 
                                                   available from the Dataset tab.</p>')
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotly::plotlyOutput('property_use_in_references',
                                                                                 width = "100%",
                                                                                 height = "900px")
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 9,
                                                hr(),
                                                HTML('<p style="font-size:80%;"><b>Number of references per property.</b> 
                                                   The whole Wikidata JSON dump 
                                                   (<a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">copy in hdfs</a>) 
                                                   is searched to count all references made for claims using a particular property. The chart presents the top 100 
                                                   properties in respect to how many references they recieve, while the complete dataset is 
                                                   available from the Dataset tab.</p>')
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotly::plotlyOutput('property_num_references',
                                                                                 width = "100%",
                                                                                 height = "900px")
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>'),
                                       htmlOutput('updateString'),
                                       htmlOutput('currentDump')
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Statements :: Wikidata, WMDE 2021</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab properties_in_wikidata
                      tabItem(tabName = "properties_reuse",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 9,
                                                HTML('<p style="font-size:80%;"><b>Property reuse.</b> 
                                                   This is the WDCM reuse statistic for properties.
                                                   <b>Note.</b> The <i>Reuse</i> variable here refers exactly to the <b>C aspect</b> of Wikidata reused as 
                                                  defined in the <a href="https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank">
                                                  Wikibase schema</a>. The chart presents the top 100 
                                                   properties in respect to how many times are the claims that encompass them reused, while the complete dataset is 
                                                   available from the Dataset tab.</p>')
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotly::plotlyOutput('property_c_reuse',
                                                                                 width = "100%",
                                                                                 height = "900px")
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2021</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )                    
                      ), ### --- END Tab properties_reuse
                      tabItem(tabName = "properties_dataset", 
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>The Properties dataset.</b> The following table 
                                          encompasses all Wikidata properties used in claims and references. Columns: 
                                          <b>(1) property</b>: a Wikidata property P-identifier, <b>(2) label</b>: the English label, 
                                          <b>(3)Used in claims</b>: how many times is this property used in claims across Wikidata, 
                                          <b>(4) Used in references</b>: how many times is this property used in references,
                                          <b>(5) Used in qualifiers</b>: how many times is this property used in qualifiers,
                                          <b>(6) Num. of references</b>: how many references, in total, are provided for statements 
                                          that use this property, <b>(7) Reuse</b>: how many times is this property found in the claims 
                                          on Wikidata items that are reused across the WMF projects.
                                          <b>Note.</b> The <i>Reuse</i> variable here refers exactly to the <b>C aspect</b> of Wikidata reused as 
                                          defined in the <a href="https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank">Wikibase schema</a>.</p>'),
                                       hr()
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                column(width = 9,
                                       shinycssloaders::withSpinner(DT::dataTableOutput('propertiesSet')
                                       )
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2021</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ),
                      
                      ### --- TAB: Properties
                      ### --------------------------------
                      tabItem(tabName = "items"),
                      tabItem(tabName = "items_in_wikidata",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 9,
                                                HTML('<p style="font-size:80%;"><b>Items used as values in Wikidata claims.</b> 
                                                   The chart presents the top 100 Wikidata items in respect to how many times 
                                                   they are used as values for various properties in claims. The dataset encompassing 
                                                   the statistics for the top 100,000 items is available from the Datasets tab.</p>')
                                         ),
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotly::plotlyOutput('items_use_in_properties',
                                                                                 width = "100%",
                                                                                 height = "900px")
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2021</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab items_in_wikidata
                      tabItem(tabName = "items_reuse",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 9,
                                                HTML('<p style="font-size:80%;"><b>Items whose claims on various properties are reused the most.</b> 
                                                     The chart presents the top 100 Wikidata items in respect to how many times 
                                                   their claims on various properties are reused across the WMF projects. The dataset encompassing 
                                                   the statistics for the top 100,000 items is available from the Datasets tab.
                                                   <b>Note.</b> The <i>Reuse</i> variable here refers exactly to the <b>C aspect</b> of Wikidata reused as 
                                                   defined in the <a href="https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank">Wikibase schema</a>.</p>')
                                         ), 
                                         column(width = 12,
                                                shinycssloaders::withSpinner(plotly::plotlyOutput('items_reuse',
                                                                                 width = "100%",
                                                                                 height = "900px")
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2021</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )                    
                      ), # - END TAB items_reuse
                      
                      tabItem(tabName = "items_dataset", 
                              fluidRow(
                                column(width = 9),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
                                        <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Overview-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                       HTML('<p style="font-size:80%;"><b>Wikidata Items as values in claims.</b> 
                                          The table encompasses the top 100,items in respect to how many times in Wikidata 
                                          they are used as values for various properties in claims.</p>'),
                                       hr(),
                                       shinycssloaders::withSpinner(DT::dataTableOutput('items_wd_set'))
                                ),
                                column(width = 6,
                                       HTML('<p style="font-size:80%;"><b>Reuse of claims from particular items.</b>
                                           The table encompasses the top 100,000 items in respect to how frequently are their claims 
                                          on various properties reused across the WMF projects.
                                          <b>Note.</b> The <i>Reuse</i> variable here refers exactly to the <b>C aspect</b> of Wikidata reused as 
                                          defined in the <a href="https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank"> 
                                          Wikibase schema</a>.</p>'),
                                       hr(),
                                       shinycssloaders::withSpinner(DT::dataTableOutput('items_reuse_set'))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2021</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ),
                      
                      tabItem(tabName = "documentation",
                              fluidRow(
                                column(width = 8,
                                       HTML('<h2>WDCM Statements Dashboard</h2>
                                          <h4>Description<h4>
                                          <hr>
                                          <h4>Introduction<h4>
                                          <br>
                                          <p style="font-size:65%;">This Dashboard is a part of the <b>Wikidata Concepts Monitor (WDMC)</b>. The WDCM system provides analytics on Wikidata reuse
                                          across the client projects, but this dashboard also reports statistics from Wikidata directly. The WDCM Statements Dashboard focuses on property and item 
                                          use in Wikidata claims, references, and the reuse of claims on various Wikidata items across the WMF projects.</p>
                                          <hr>
                                          <h4>Dashboard overview</h4>
                                          <br>
                                          <p style="font-size:80%;">
                                          <b>Property use in Wikidata claims.</b> 
                                                   The whole Wikidata JSON dump 
                                                   (<a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">copy in hdfs</a>) 
                                                   is searched for all property use cases made in claims. The chart presents the top 100 
                                                   most frequently used properties, while the complete dataset is 
                                                   available from the Dataset tab.
                                           <br>
                                           <b>Property use in references.</b> 
                                                   The whole Wikidata JSON dump 
                                                   (<a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">copy in hdfs</a>) 
                                                   is searched for all property use cases made in references. The chart presents the top 100 
                                                   most frequently used properties, while the complete dataset is 
                                                   available from the Dataset tab.
                                           <br>
                                           <b>Number of references per property.</b> 
                                                   The whole Wikidata JSON dump 
                                                   (<a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Content/Wikidata_entity" target="_blank">copy in hdfs</a>) 
                                                   is searched to count all references made for claims using a particular property. The chart presents the top 100 
                                                   properties in respect to how many references they recieve, while the complete dataset is 
                                                   available from the Dataset tab.
                                           <br>
                                           <b>Property reuse.</b> 
                                                   This is the WDCM reuse statistic for properties.
                                                   <b>Note.</b> The <i>Reuse</i> variable here refers exactly to the <b>C aspect</b> of Wikidata reused as 
                                                  defined in the <a href="https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank">
                                                  Wikibase schema</a>. The chart presents the top 100 
                                                   properties in respect to how many times are the claims that encompass them reused, while the complete dataset is 
                                                   available from the Dataset tab.
                                           <br>
                                           <b>The Properties dataset.</b> The following table 
                                          encompasses all Wikidata properties used in claims and references. Columns: 
                                          <b>(1) property</b>: a Wikidata property P-identifier, <b>(2) label</b>: the English label, 
                                          <b>(3)Used in claims</b>: how many times is this property used in claims across Wikidata, 
                                          <b>(4) Used in references</b>: how many times is this property used in references,
                                          <b>(5) Used in qualifiers</b>: how many times is this property used in qualifiers, 
                                          <b>(6) Num. of references</b>: how many references, in total, are provided for statements 
                                          that use this property, <b>(7) Reuse</b>: how many times is this property found in the claims 
                                          on Wikidata items that are reused across the WMF projects.
                                          <b>Note.</b> The <i>Reuse</i> variable here refers exactly to the <b>C aspect</b> of Wikidata reused as 
                                          defined in the <a href="https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank">Wikibase schema</a>.
                                          <br>
                                           <b>Items used as values in Wikidata claims.</b> 
                                                   The chart presents the top 100 Wikidata items in respect to how many times 
                                                   they are used as values for various properties in claims. The dataset encompassing 
                                                   the statistics for the top 100,000 items is available from the Datasets tab.
                                           <br>
                                           <b>Items whose claims on various properties are reused the most.</b> 
                                                     The chart presents the top 100 Wikidata items in respect to how many times 
                                                   their claims on various properties are reused across the WMF projects. The dataset encompassing 
                                                   the statistics for the top 100,000 items is available from the Datasets tab.
                                                   <b>Note.</b> The <i>Reuse</i> variable here refers exactly to the <b>C aspect</b> of Wikidata reused as 
                                                   defined in the <a href="https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank">Wikibase schema</a>.
                                           <br>
                                           <b>Wikidata Items as values in claims.</b> 
                                          The table encompasses the top 100,items in respect to how many times in Wikidata 
                                          they are used as values for various properties in claims.
                                          </p>')
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                        <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                        <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2021</b><br></p>'),
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
                                                                   package = "WDCMStatementsDashboard"))
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/statements/" target = "_blank">Public datasets</a><br>
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Overview :: Wikidata, WMDE 2021</b><br></p>'),
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
      app_title = 'WDCMStatementsDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

