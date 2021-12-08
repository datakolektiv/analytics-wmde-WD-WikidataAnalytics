### ---------------------------------------------------------------------------
### --- WDCM (S)itelinks
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
    
    shinydashboard::dashboardPage(skin = "black",
                  
                  ### --- dashboarHeader
                  ### --------------------------------
                  
                  shinydashboard::dashboardHeader(
                    # - Title
                    title = "WDCM Wikipedia (S)itelinks",
                    titleWidth = 300
                  ), 
                  ### ---- END dashboardHeader
                  
                  ### --- dashboardSidebar
                  ### --------------------------------
                  
                  shinydashboard::dashboardSidebar(
                    shinydashboard::sidebarMenu(
                      id = "tabsWDCM",
                      menuItem(text = "Category View", 
                               tabName = "categoryView", 
                               icon = icon("barcode"),
                               selected = TRUE,
                               menuSubItem('Category',
                                           tabName = 'themeDescription',
                                           icon = icon('table')),
                               menuSubItem('Themes: Items',
                                           tabName = 'themeItems',
                                           icon = icon('line-chart')),
                               menuSubItem('Distribution: Items',
                                           tabName = 'themeDistributionItems',
                                           icon = icon('bar-chart')),
                               menuSubItem('Themes: Projects',
                                           tabName = 'themeProjects',
                                           icon = icon('line-chart')),
                               menuSubItem('Distribution: Projects',
                                           tabName = 'themeDistributionProjects',
                                           icon = icon('bar-chart')),
                               menuSubItem('Items: Graph',
                                           tabName = 'itemsCategoryGraph',
                                           icon = icon('braille')),
                               menuSubItem('Items: Hierarchy',
                                           tabName = 'itemsCategoryHierarchy',
                                           icon = icon('braille')),
                               menuSubItem('Projects: Graph',
                                           tabName = 'projectsCategoryGraph',
                                           icon = icon('braille')),
                               menuSubItem('Projects: Hierarchy',
                                           tabName = 'projectsCategoryHierarchy',
                                           icon = icon('braille'))
                      ),
                      menuItem(text = "Wiki View", 
                               tabName = "projectView", 
                               icon = icon("barcode"),
                               selected = TRUE,
                               menuSubItem('Wiki: Wikipedia',
                                           tabName = 'wiki',
                                           icon = icon('line-chart')),
                               menuSubItem('Wiki:Similarity',
                                           tabName = 'wikiSimilarity',
                                           icon = icon('braille')),
                               menuSubItem('Wiki:Topics',
                                           tabName = 'wikiTopics',
                                           icon = icon('bar-chart'))
                      ),
                      menuItem(text = "Info",
                               tabName = "documentation",
                               icon = icon("barcode")),
                      menuItem(text = "Navigate WDCM", 
                               tabName = "navigate", 
                               icon = icon("barcode"),
                               selected = FALSE)
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
                      
                      tabItem(tabName = "categoryView"
                      ),
                      tabItem(tabName = "themeDescription",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Category </b>All semantics categories under consideration 
                                          (check out the Documentation tab for an overview) are described by a set of <i>semantic themes</i> 
                                          (or <i>topics</i>). Select a category of items from the drop-down menu and the Dashboard will generate a 
                                          concise description of the semantic themes found in that category. <b>NOTE. The selection of categories 
                                          made here is in power across all tabs in <i>Category View</i></b>.</p>
                                          <p style="font-size:80%;"><b>Explanation.</b> Each row in the table stands for one semantic theme that describes the selected 
                                          category of Wikidata items. The most important Wikidata <i>classes</i> that describe a particular semantic 
                                          theme are listed in the <i>Classes</i> column and <b><i>in decreasing order of importance in each theme</i></b>. 
                                          The <i>Diversity</i> score, expressed in percent units, tells us how well <i>"diversified"</i> is 
                                          the given  semantic theme. A semantic theme can be focused on some Wikidata items and classes while 
                                          some other items or classes might be relatively unimportant to it. The higher the <i>diversity</i> score for 
                                          some given semantic theme - the larger the number of items and classes that play an important role there.<br>
                                          In order to gain understanding on a particular theme, you need to inspect what classes are more important in 
                                          it. Later, you will observe how the Wikidata classes can be used to describe each Wikipedia in respect to where
                                          does it focus its interets in the scope of a given semantic theme and category (<i>hint:</i> see Wiki:Topics).</p>'),
                                       hr()
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor#WDCM_(S)itelinks_Dashboard" target="_blank">Documentation</a><br><a href = "https://analytics.wikimedia.org/datasets/wdcm/WDCM_Sitelinks/" 
                                          target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WDCM-WikipediaSemantics-Dashboard" 
                                          target = "_blank">GitHub</a></p>'),
                                       htmlOutput('updateString')
                                )
                              ),
                              fluidRow(
                                column(width = 2, 
                                       selectizeInput("selectCategory",
                                                      "Select category:",
                                                      multiple = F,
                                                      choices = NULL)
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       shinycssloaders::withSpinner(DT::dataTableOutput('themeDescription_DT', 
                                                                       width = "100%"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "themeItems",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View. Themes: Items. </b>The chart represents the most important items in the selected 
                                          semantic theme for the respectitve category (<b>reminder: </b>The choice of the 
                                          category of Wikidata items is the one you have made on the <i>Category View: Category</i> tab).
                                          The vertical axis represents the item weight (0 - 1) in the given semantic theme: higher weights indicate 
                                          more important items. In order to understand the meaning of the selected topic, look at the most important 
                                          items and ask yourself: what principle holds them together?</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 3,
                                       htmlOutput('themeItems_SelectedCategory')
                                ),
                                column(width = 2,
                                       uiOutput("selectTheme_Items")
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       tabBox(id = 'tabset_themeDistributionItems_interactive', 
                                              selected = 'Interactive', 
                                              width = 12,
                                              height = NULL, 
                                              side = "left",
                                              tabPanel("Interactive",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotly::plotlyOutput('themeDistributionItems_interactive',
                                                                                                                  width = "100%",
                                                                                                                  height = "800px"))
                                                         )
                                                       )
                                              ),
                                              tabPanel("Static",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotOutput('themeDistributionItems',
                                                                                                        width = "100%",
                                                                                                        height = "800px"))
                                                         )
                                                       )
                                              )
                                       ),
                                       br()
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "themeDistributionItems",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Distribution:Items. </b> The chart represents the 
                                          distribution of the item weight in all semantic themes of the selected semantic category 
                                          (<b>reminder: </b>The choice of the category of Wikidata items is the one you have made on the 
                                          <i>Category View: Category</i> tab). The horizontal axes represents Item Weight (which is a 
                                          probability measure, thus ranging from 0 to 1), while the vertical axis stands for the number 
                                          of items of a given weight. <i>Roughly speaking</i>, the more spread-out the distribution 
                                          in a given theme, the more diversified are the semantics that it describes (i.e. a larger number of 
                                          different Wikidata items play a significant role in it; the theme is "less focused").</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       htmlOutput('themeDistributionItems_SelectedCategory'),
                                       hr(),
                                       shinycssloaders::withSpinner(plotOutput('themeDistributionItems_Full', height = "1200px"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "themeProjects",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Themes:Projects. </b> The chart represents the top 50 Wikipedias in which the selected 
                                          semantic theme in the respective category plays an important role. (<b>reminder: </b>The choice of the category 
                                          of Wikidata items is the one you have made on the <i>Category View: Category</i> tab). Each Wikipedia receives an importance score 
                                          in each semantic theme of a particular category of Wikidata items. The vertical axes represent 
                                          the importance score (0 - 1, i.e. how much is the respective theme important in some Wikipedia).</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 3,
                                       htmlOutput('themeProjects_SelectedCategory')
                                ),
                                column(width = 2,
                                       uiOutput("selectTheme_Project")
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       tabBox(id = 'tabset_themeDistributionProjects', 
                                              selected = 'Interactive', 
                                              width = 12,
                                              height = NULL, 
                                              side = "left",
                                              tabPanel("Interactive",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotly::plotlyOutput('themeDistributionProjects_interactive',
                                                                                                                  width = "100%",
                                                                                                                  height = "800px"))
                                                         )
                                                       )
                                              ),
                                              tabPanel("Static",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotOutput('themeDistributionProjects',
                                                                                                        width = "100%",
                                                                                                        height = "800px"))
                                                         )
                                                       )
                                              )
                                       ),
                                       br()
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b><br></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "themeDistributionProjects",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Distribution: Projects. </b> The chart represents the distribution of the importance score 
                                          for a selected semantic theme across Wikipedias. Each Wikipedia receives an importance score in each semantic theme of a particular category 
                                          of Wikidata items. The more spread out the distribution of the importance score, larger the number of Wikipedias in which the respective 
                                          semantic theme plays an important role. The horizontal axis represent the importance score (0 - 1), while the vertical axis stands for 
                                          the count of Wikipedias with the respective score (<b>reminder: </b>The choice of the category of Wikidata items is the one you have 
                                          made on the <i>Category View:Category</i> tab).</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       htmlOutput('themeDistributionProjects_SelectedCategory'),
                                       hr(),
                                       shinycssloaders::withSpinner(plotOutput('themeDistributionProjects_Full', height = "1200px"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b><br></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "itemsCategoryGraph",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Items: Graph. </b> The graph represents the structure of similarity across the 
                                          most important items in the selected category (<b>reminder: </b>The choice of the category of Wikidata items is the one you have 
                                          made on the <i>Category View:Category</i> tab). The similarity between any two items is computed from their weights across all 
                                          semantic themes in the category. Each item in the graph points towards the three most similar items to it: the width of the line 
                                          that connects them corresponds to how similar they are. Items receiving a lot of incoming links are quite interesting, as they act as 
                                          "hubs" in the similarity structure of the whole category: they are rather illustrative of the category\'s semantics in general.
                                          You can focus on a particular item by selecting it from the \'Select by label\' drop-down menu, use mouse wheel to zoom in and out, 
                                          drag the whole graph or particular items around to inspect their neighbourhoods.
                                          <br><b style="color:blue;">Please be patient:</b> rendering a large graph might take a while.</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       htmlOutput('itemsGraph_SelectedCategory'),
                                       shinycssloaders::withSpinner(visNetwork::visNetworkOutput('itemsGraph', width = "100%", height = 850))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b><br></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "projectsCategoryGraph",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Projects: Graph. </b> The graph represents the structure of similarity across the 
                                          Wikipedias in the selected category (<b>reminder: </b>The choice of the category of Wikidata items is the one you have 
                                          made on the <i>Category View:Category</i> tab). The similarity between any two Wikipedias is computed from their importance scores across all 
                                          semantic themes in the category. Each Wikipedia in the graph points towards the three most similar Wikipedias to it: the width of the line 
                                          that connects them corresponds to how similar they are. Wikipedias receiving a lot of incoming links act as 
                                          "attractors" in the similarity structure of the whole category: they are rather representative of the category as such.
                                          You can focus on a particular Wikipedia by selecting its language code from the \'Select by label\' drop-down menu, use mouse wheel to zoom in and out, 
                                          drag the whole graph or particular Wikipedias around to inspect their neighbourhoods.
                                          <br><b style="color:blue;">Please be patient:</b> rendering a large graph might take a while.</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       htmlOutput('projectsGraph_SelectedCategory'),
                                       shinycssloaders::withSpinner(visNetwork::visNetworkOutput('projectsGraph', width = "100%", height = 850))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b><br></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "itemsCategoryHierarchy",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Items: Hiearchy. </b> We first look at (1) how similar are the items from the selected 
                                          category, then (2) trace how do the items form small groups (i.e. clusters) in respect to their mutual similarity, and then (3) how 
                                          do these small groups tend to join to form progressively larger groups of similar items (<b>reminder: </b>The choice of the category 
                                          of Wikidata items is the one you have made on the <i>Category View:Category</i> tab). Do not forget that the similarity between items 
                                          here is not guided only but what you or anyone else would claim to <i>know</i> about them, but also by how the editor community chooses 
                                          to <i>use these items</i> across various Wikipedias! For example, if two manifestly unrelated items are frequently used across the same set 
                                          of Wikipedias, they will be recognized as similar in that respect.</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                       htmlOutput('itemsHierarchy_SelectedCategory'),
                                       shinycssloaders::withSpinner(plotOutput('itemsHierarchy', width = "100%", height = "800px"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b><br></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "projectsCategoryHierarchy",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Category View: Projects: Hiearchy. </b> We look at (1) how similar are the Wikipedias in the selected 
                                          category, then (2) trace how do the they first form small groups of Wikipedias (i.e. clusters) in respect to their mutual similarity, 
                                          and then (3) how do these small groups tend to join to form progressively larger groups of similar Wikipedias (<b>reminder: </b>The choice 
                                          of the category is the one you have made on the <i>Category View:Category</i> tab). In other words, similar Wikipedias are found under 
                                          the same branches of the tree spawned by this hierarchical representation of similarity.</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                       htmlOutput('projectsHierarchy_SelectedCategory'),
                                       shinycssloaders::withSpinner(plotOutput('projectsHierarchy', width = "100%", height = "800px"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'), 
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      
                      tabItem(tabName = "projectView",
                              fluidRow(
                                HTML("wikiView."),
                                fluidRow(
                                  hr(),
                                  column(width = 1,
                                         br(),
                                         tags$img(src = "www/Wikidata-logo-en.png")
                                  ),
                                  column(width = 11,
                                         hr(),
                                         HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'),
                                         HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                         br(),
                                         br()
                                  )
                                )
                              )
                      ),
                      tabItem(tabName = "wiki",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Wiki View: Wikipedia. </b> Select a Wikipedia from the drop-down menu and wait for the Dashboard to 
                                          generate a set of charts that provide an overview of its semantics as derived from the way Wikidata is used in it. <b>NOTE. 
                                          The selection of a particular Wikipedia made here is in power across all tabs in <i>Wiki View</i></b>.</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 2, 
                                       selectizeInput("selectWiki",
                                                      "Select Wikipedia:",
                                                      multiple = F,
                                                      choices = NULL)
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                       HTML('<p style="font-size:80%;"><b>Category Distribution in Wiki. </b>This chart presents the 
                                          distribution of item usage across several Wikidata classes in the selected project.<br><br><br></p>'),
                                       hr(),
                                       shinycssloaders::withSpinner(plotOutput('wiki_CategoryDistribution', width = "100%", height = "600px"))
                                ),
                                column(width = 6,
                                       HTML('<p style="font-size:80%;"><b>Local Semantic Neighbourhood. </b>This graph presents the selected 
                                          Wikipedia alongside the ten most similar Wikipedias to it. Similarity was computed by inspecting a large 
                                          number of Wikidata items from all item classes under consideration and registering what items are 
                                          used across different Wikipedias. Each Wikipedia points towards the three most similar Wikipedias to it. 
                                          <b>NOTE.</b> This is the <i>local</i> similarity neighbourhood only; 
                                          for a full similarity map see the <b>Wiki:Similarity tab</b>.</p>'),
                                       hr(),
                                       shinycssloaders::withSpinner(plotOutput('wiki_Neighbourhood', width = "100%", height = "600px"))
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                       HTML('<p style="font-size:80%;"><b>Category Usage Profiles. </b>The chart represents the usage 
                                          of different Wikidata classes in the selected Wikipedia and the ten most similar Wikipedias to it. 
                                          The vertical axis, representing the count of items used from the respective classes on the horizontal 
                                          axis, is provided on a logarithmic scale. The data points of the selected Wikipedia are labeled by 
                                          exact counts.</p>'),
                                       hr(),
                                       shinycssloaders::withSpinner(plotOutput('wiki_CategoryProfiles', width = "100%", height = "600px"))
                                ),
                                column(width = 6,
                                       HTML('<p style="font-size:80%;"><b>Wikipedia Similarity Profile. </b>The histogram represents the 
                                          distribution of similarity between the selected Wikipedia and all other Wikipedias 
                                          on this dashboard. The similarity coefficient used is Jaccard, which has a range 
                                          from 0 (high similarity) to 1 (low similarity). Similarity is binned into ten categories on 
                                          the horizontal axis, while the counts of Wikipedias found in each bin is given on the vertical axis. 
                                          The more is the histogram skewed to the left - higher the number of Wikipedias similar to the selected 
                                          one.</p>'),
                                       hr(),
                                       shinycssloaders::withSpinner(plotOutput('wiki_SimilarityProfile', width = "100%", height = "600px"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "wikiSimilarity",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Wiki: Wiki:Similarity. </b>The graph represents the similarity structure across all Wikipedias 
                                          that can be compared to the selected one (<b>reminder: </b>The choice of the Wikipedia is the one you have made on the 
                                          <i>Wiki View: Wikipedia</i> tab; the selected Wikipedia is represented by the red node in the graph). We first select all 
                                          Wikipedias that make use of the same semantic categories as the selected one. Than we inspect how many times was each of the 
                                          10,000 most frequently used Wikidata items in each semantic category used in every comparable Wikipedia. From these data we 
                                          derive a similarity measure that describes the pairwise similarity among Wikipedias. <br>
                                          Each Wikipedia in the graph points towards the three most similar Wikipedias to it: the width of the line 
                                          that connects them corresponds to how similar they are. You can focus on a particular Wikipedia by selecting it from 
                                          the \'Select by label\' drop-down menu, use mouse wheel to zoom in and out, drag the whole graph or particular Wikipedias 
                                          around to inspect their neighbourhoods. <br><b style="color:blue;">Please be patient:</b> rendering a large graph might take a while.</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       htmlOutput('wikiGraph_SelectedWiki'),
                                       shinycssloaders::withSpinner(visNetwork::visNetworkOutput('wikiGraph', width = "100%", height = 850))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "wikiTopics",
                              fluidRow(
                                column(width = 9,
                                       HTML('<p style="font-size:80%;"><b>Wiki: Wiki:Topics </b>The chart represents the importance score of the selected Wikipedia in 
                                          each semantic theme (themes are represented on the horizontal axes of the plots), in each semantic class (shown on different 
                                          panels; <b>reminder: </b>The choice of the Wikipedia is the one you have made on the <i>Wiki View: Wikipedia</i> tab).
                                          <i>Hint</i>: this is where you can start building an understanding of "what is a particular Wikipedia about": you might 
                                          first study each semantic theme in each semantic class (in <i>Category View: Category</i>) to understand what do the semantic 
                                          themes represent, and then get back here to see in which semantic themes in particular classes is this Wikipedia well represented.<br>
                                          <b>Note: </b> While the horizontal axes represent a large number of semantic themes, not each semantic class (they are represented on 
                                          different panels here) encompass that many topics; take a look at <i>Category View: Category</i> to find out how many semantic themes 
                                          there are in a particular class. Data points for the themes that do not exist in a particular class, or have an importance score of zero, 
                                          are not labeled.</p>'),
                                       hr()
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       htmlOutput('wiki_TopicProfile_SelectedWiki'),
                                       tabBox(id = 'tabset_wiki_TopicProfile', 
                                              selected = 'Interactive', 
                                              width = 12,
                                              height = NULL, 
                                              side = "left",
                                              tabPanel("Interactive",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotly::plotlyOutput('wiki_TopicProfile_interactive',
                                                                                                                  width = "100%",
                                                                                                                  height = "800px"))
                                                         )
                                                       )
                                              ),
                                              tabPanel("Static",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotOutput('wiki_TopicProfile',
                                                                                                        width = "100%",
                                                                                                        height = "800px"))
                                                         )
                                                       )
                                              )
                                       ),
                                       br()
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "documentation",
                              fluidRow(
                                column(width = 4,
                                       HTML('<p style="font-size:80%;"><b>Info.</b> The WDCM (S)itelinks dashboard analyzes only the sitelinks usage aspect from 
                                          the <a href = "https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target="_blank">wbc_entity_usage table</a> 
                                          (from the <a href = "https://www.mediawiki.org/wiki/Wikibase/Schema" target="_blank">Wikibase schema</a>), and takes 
                                          into account only mature Wikipedia projects (in terms of Wikidata usage) in order to obtain 
                                          and present a broad and as clear as possible overview of the structure of Wikidata usage 
                                          across the Wikipedia.<br>
                                          Project selection criteria and all other technical information are provided on 
                                          <a href="https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor#WDCM_(S)itelinks_Dashboard" target="_blank">Wikitech</a>.<br>
                                          In order to learn how to work with the Wikidata Concepts Monitor, please visit the projects\'s 
                                          <a href = "https://www.wikidata.org/wiki/Wikidata:Wikidata_Concepts_Monitor" target = "_blank">Wikidata page</a>.</p>')
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
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      ),
                      tabItem(tabName = "navigate",
                              fluidRow(
                                column(width = 6,
                                       includeMarkdown(system.file("app/www/wdcmNavigate.html", 
                                                                   package = "WDCMSitelinksDashboard")),
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/Sitelinks/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Sitelinks-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              
                              fluidRow(
                                hr(),
                                column(width = 2,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 11,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM (S)itelinks :: Wikidata, WMDE 2018</b></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm</p>'),
                                       br(),
                                       br()
                                )
                              )
                      )
                      
                    ) ### --- END tabItems
                    
                  ) ### --- END dashboardBody
                  
    ) ### --- dashboardPage
    
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
      app_title = 'WDCM_SitelinksDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

