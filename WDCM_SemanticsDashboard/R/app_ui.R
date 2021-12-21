### ---------------------------------------------------------------------------
### --- WDCM Semantics
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
                    title = "WDCM: Semantics",
                    titleWidth = 230
                  ), 
                  ### ---- END dashboardHeader
                  
                  ### --- dashboardSidebar
                  ### --------------------------------
                  
                  shinydashboard::dashboardSidebar(
                    sidebarMenu(
                      id = "tabsWDCM",
                      menuItem(text = "Semantic Models", 
                               tabName = "semanticModels", 
                               icon = icon("barcode")
                      ),
                      menuItem(text = "Project Semantics",
                               tabName = "projectSemantics",
                               icon = icon("barcode")
                      ),
                      menuItem(text = "Similarity Maps",
                               tabName = "similarityMaps",
                               icon = icon("barcode")
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
                      
                      ### --- TAB: semanticmModels
                      ### --------------------------------
                      
                      tabItem(tabName = "semanticModels",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 12,
                                                h4('Semantic Models'),
                                                HTML('<p style="font-size:80%;">This page provides an oportunity to study the WDCM semantic models. 
                                                    The WDCM organizes its knowledge of Wikidata usage into <b>semantic categories</b> and currently 
                                                    uses 14 of them. Each semantic category encompasses a set of Wikidata items that match a particular intuitive, 
                                                    natural concept (e.g. "Human", "Geographical Object", "Event", etc).<br>
                                                    The WDCM develops a <b>semantic topic model</b> 
                                                    (see: <a href = "https://en.wikipedia.org/wiki/Topic_model" target = "_blank">Topic Model</a>) for each 
                                                    semantic category. Each semantic model encompasses a number of topics, or semantic themes. Each topic is 
                                                    characterized by the importance of Wikidata items from the respective semantic category in that topic. 
                                                    Here you can browse the semantic categories and inspect the structure of topics that are encompassed 
                                                    by the respective semantic model. You can also learn about the most important projects in a given category 
                                                    for a given topic from its semantic model.<br> 
                                                    The Dashboard will initialize a random choice of <i>Category</i> 
                                                    and pick the first <i>Topic</i> from its semantic model. Use the drop-down menus to select a category 
                                                    and a topic from its semantic model. Three outputs will be generated on this page: the Top 50 items chart, the 
                                                    topic similarity network, and the top 50 projects in this topic chart (scroll down)</p>'),
                                                hr()
                                         ),
                                         column(width = 12,
                                                fluidRow(
                                                  br(),
                                                  column(width = 3,
                                                         selectizeInput("selectCategory",
                                                                        "Select Item Category:",
                                                                        multiple = F,
                                                                        choices = NULL,
                                                                        selected = NULL)
                                                  ),
                                                  column(width = 3,
                                                         uiOutput("selectCatTopic")
                                                  )
                                                ),
                                                fluidRow(
                                                  column(width = 12,
                                                         hr(),
                                                         h4('Top 50 items in this topic'),
                                                         HTML('<p style="font-size:80%;">The chart represents the top 50 most important items in this topic. 
                                                            The importance of each item is given by its probability of being generated by this particular 
                                                            semantic topic (horizontal axis). The items are ranked; the rank numbers next to the labels on 
                                                            the vertical axis correspond to the rank numbers in parentheses next to data labels that show the item Wikidata IDs. 
                                                            <i>There\'s a game that you can play here:</i> ask yourself what makes this 50 items go together, 
                                                            what makes them similar, what unifying principle holds them together in the same semantic topic? 
                                                            Do not forget: it is not only about what you know about the World, but also about how our communities use 
                                                            Wikidata on their respective projects.</p>'),
                                                         br(), br(),
                                                         tabBox(id = 'tabset_topItemsTopic', 
                                                                selected = 'Interactive', 
                                                                width = 12,
                                                                height = NULL, 
                                                                side = "left",
                                                                tabPanel("Interactive",
                                                                         fluidRow(
                                                                           column(width = 12,
                                                                                  shinycssloaders::withSpinner(plotly::plotlyOutput('topItemsTopic_interactive',
                                                                                                                                    width = "100%",
                                                                                                                                    height = "800px"))
                                                                           )
                                                                         )
                                                                ),
                                                                tabPanel("Static",
                                                                         fluidRow(
                                                                           column(width = 12,
                                                                                  shinycssloaders::withSpinner(plotOutput('topItemsTopic',
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
                                                  column(width = 12, 
                                                         hr(),
                                                         h4('Topic similarity network'),
                                                         HTML('<p style="font-size:80%;">Each bubble represents one among the top 50 most important items in this semantic 
                                                              topic. Each item points towards the the item to which it is most similar. Similarity between items is 
                                                              derived not only from item importances (i.e. probabilities) in this topic, but from all topics that 
                                                              are encompasses by this category\'s semantic model. In interpreting the similarities, do not forget 
                                                              that game is not only about what you know about the World, but also about how different communities 
                                                              use Wikidata. The more similarly the items are used across the sister projects, the more likely they 
                                                              will group together in this network. You can drag the network and the nodes around and zoom in and 
                                                              out by your mouse wheel.</p>'),
                                                         br(), br(),
                                                         shinycssloaders::withSpinner(visNetwork::visNetworkOutput('networkItemsTopic', height = 850))
                                                  )
                                                ),
                                                fluidRow(
                                                  column(width = 12,
                                                         hr(),
                                                         h4('Top 50 projects in this topic'),
                                                         HTML('<p style="font-size:80%;">To put it in a nutshell: here you can see what projects use the selected 
                                                            topic from the respective semantic category the most. The chart represents the top 50 projects in 
                                                            respect to the prominence of the selected topic. In the WDCM topic models, the usage pattern of any 
                                                            particular semantic category of Wikidata items, in a particular project, can be viewed as a mixture 
                                                            of semantic topics from the respective category\'s semantic model. Thus, each project\'s usage pattern 
                                                            in a particular semantic category can be expressed as a set of proportions up to which each topic 
                                                            contributes to it. The horizontal axis represents the proportion (e.g. the probability) of the selected 
                                                            topic\'s presence in a particular project. Projects are found on the vertical axis, with the rank numbers 
                                                            corresponding to those near the data points in the chart.</p>'),
                                                         br(), br(),
                                                         tabBox(id = 'tabset_projectOverview_Category', 
                                                                selected = 'Interactive', 
                                                                width = 12,
                                                                height = NULL, 
                                                                side = "left",
                                                                tabPanel("Interactive",
                                                                         fluidRow(
                                                                           column(width = 12,
                                                                                  shinycssloaders::withSpinner(plotly::plotlyOutput('topProjectsTopic_interactive',
                                                                                                                                    width = "100%",
                                                                                                                                    height = "800px"))
                                                                           )
                                                                         )
                                                                ),
                                                                tabPanel("Static",
                                                                         fluidRow(
                                                                           column(width = 12,
                                                                                  shinycssloaders::withSpinner(plotOutput('topProjectsTopic',
                                                                                                                          width = "100%",
                                                                                                                          height = "800px"))
                                                                           )
                                                                         )
                                                                )
                                                         ),
                                                         br()
                                                  )
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/ml/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Semantics-Dashboard" target = "_blank">GitHub</a></p>'),
                                       htmlOutput('updateString')
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Semantics :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab semanticModels
                      
                      ### --- TAB: projectSemantics
                      ### --------------------------------
                      
                      tabItem(tabName = "projectSemantics",
                              fluidRow(
                                column(width = 9,
                                       fluidRow(
                                         column(width = 12,
                                                h4('Project Semantics'),
                                                HTML('<p style="font-size:80%;">Here you can make a selection of projects and learn about the importance of 
                                              all available semantic topics from each semantic category in the project(s) of your choice. <b>Note:</b> 
                                              You can search and add projects into the <i>Search projects</i> field by using (a) <b>project names</b> 
                                              (e.g. <i>enwiki</i>, <i>dewiki</i>, <i>sawikiquote</i>, and similar or (b) by using <b>project types</b> 
                                              that start with <b>"_"</b> (underscore, e.g. <i>_Wikipedia</i>, <i>_Wikisource</i>, <i>_Commons</i>, and 
                                              similar; try typing anything into the Select projects field that starts with an underscore). Please note 
                                              that by selecting a project type (again: <i>_Wikipedia</i>, <i>_Wikiquote</i>, and similar) you are selecting 
                                              <b>all</b> client projects of the respective type, and that\'s potentially a lot of data. The Dashboard 
                                              will pick unique projects from whatever you have inserted into the Search projects field. <br> <b>Note:</b> 
                                              The Dashboard will initialize with a choice of all <i>Wikipedia</i> projects. Then you can make a selection of 
                                              projects of your own and hit <i>Apply Selection</i> to obtain the result.</p>')
                                         )
                                       ),
                                       # - fluidRow: Selections
                                       fluidRow(
                                         br(),
                                         column(width = 6,
                                                selectizeInput("selectProject",
                                                               "Select Projects:",
                                                               multiple = T,
                                                               choices = NULL,
                                                               selected = NULL, 
                                                               width = 800)
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 2,
                                                actionButton('applySelection',
                                                             label = "Apply Selection",
                                                             width = '70%',
                                                             icon = icon("database", 
                                                                         class = NULL, 
                                                                         lib = "font-awesome")
                                                )
                                         )
                                       )
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/ml/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Semantics-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       hr(),
                                       h4('Semantic Topics in Wikimedia Projects'),
                                       HTML('<p style="font-size:80%;"align="left">The vertical axes represent the % of topic engagement in this particular
                                          selection of Wikimedia projects. <br><b>Note:</b> Please be remindided that semantic topics are <i>category-specific</i>: 
                                          each category has its own semantic model, and each semantic model encompasses a number of topics. To clarify: e.g. Topic 1 
                                          is not the same thing in two different categories. You can learn about the content of any semantic topic in any of 
                                          the semantic categories on the <i>Semantic Models</i> tab - and in fact that is what one should do <i>before</i> any 
                                          attempt to interpret the data that are provided here.</p>'),
                                       br(), br(),
                                       tabBox(id = 'tabset_projectTopicImportance', 
                                              selected = 'Interactive', 
                                              width = 12,
                                              height = NULL, 
                                              side = "left",
                                              tabPanel("Interactive",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotly::plotlyOutput('projectTopicImportance_interactive',
                                                                                                                  width = "100%",
                                                                                                                  height = "1000px"))
                                                         )
                                                       )
                                              ),
                                              tabPanel("Static",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotOutput('projectTopicImportance',
                                                                                                        width = "100%",
                                                                                                        height = "1000px"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Semantics :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab projectSemantics
                      tabItem(tabName = "similarityMaps",
                              fluidRow(
                                column(width = 9,
                                       h4('Similarity Maps'),
                                       HTML('<p style="font-size:80%;"align="left">Select a semantic category of Wikidata items to take a look at. 
                                          A 2D map will be generated where each project is represented by a bubble, and where the distance between 
                                          the projects corresponds with the similarity in their usage of Wikidata items <i>from the selected category</i>. 
                                          Think about semantic categories as perspectives from which you can take a look at the structure of similarity 
                                          that holds among the Wikimedia projects in respect to their usage of Wikidata items.</p>')
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/ml/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Semantics-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              # - fluidRow: Category Selection
                              fluidRow(
                                br(),
                                column(width = 3,
                                       selectizeInput("selectCategory2",
                                                      "Select Category:",
                                                      multiple = F,
                                                      choices = NULL,
                                                      selected = NULL)
                                )
                              ),
                              fluidRow(
                                hr(),
                                column(width = 12,
                                       h4('Similarity Map'),
                                       HTML('<p style="font-size:80%;"align="left">Each bubble represents a client project. The size of 
                                          the bubble reflects the volume of Wikidata usage in the respective project; a logarithmic scale 
                                          is used in this plot.<br>Projects similar in respect to their usage of Wikidata items <i>from the 
                                          selected category</i> are grouped together. Use the tools next to the plot legend to explore the 
                                          plot and hover over bubbles for details.</p>'),
                                       hr(),
                                       shinycssloaders::withSpinner(rbokeh::rbokehOutput('overviewPlotDynamic',
                                                                        width = "100%",
                                                                        height = "900px")
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Semantics :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), ### --- END Tab similarityMaps
                      tabItem(tabName = "documentation",
                              fluidRow(
                                column(width = 8,
                                       HTML('<h2>WDCM Semantics Dashboard</h2>
                                          <h4>Description<h4>
                                          <hr>
                                          <h4>Introduction<h4>
                                          <p style="font-size:70%;">This Dashboard is a part of the <b>Wikidata Concepts Monitor (WDMC)</b>. The WDCM system provides analytics on Wikidata usage
                                                             across the Wikimedia sister projects. The WDCM Semantics Dashboard is probably the central and the analytically most complicated of all WDCM Dashboards. 
                                                             Here we provide only the necessary basics of distributional semantics needed in order to understand the results of semantic topic modeling presented on this 
                                                             WDCM dashboard. A user who needs to dive deep into the similarity structures between the Wikimedia sister projects in respect to their Wikidata usage patterns 
                                                             will most probably have to do some additional reading first. However, the Dashboard simplifies the presentation of the results as much as possible to make them 
                                                             accessible to any Wikidata user or Wikipedia editor who is not necessarily involved in Data or Cognitive Science. Reading through the <b>WDCM Semantic Topic Models</b> 
                                                             section in this page is <i>highly advised</i> to anyone who has never met semantic topic models or distributional semantics before. Before that, our next stop: Definitions.</p>
                                          <hr>
                                          <h4>Definitions</h4>
                                          <p style="font-size:80%;"><b>N.B.</b> The current <b>Wikidata item usage statistic</b> definition is <i>the count of the number of pages in a particular client project
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
                                                   normative in any sense and a subject  of change in any moment, depending upon the analytical needs of the community.</p>
                                          <p style="font-size:80%;">The currently used <b>WDCM Taxonomy</b> of Wikidata items encompasses the following 14 semantic categories: <i>Geographical Object</i>, <i>Organization</i>, <i>Architectural Structure</i>, 
                                          <i>Human</i>, <i>Wikimedia</i>, <i>Work of Art</i>, <i>Book</i>, <i>Gene</i>, <i>Scientific Article</i>, <i>Chemical Entities</i>, <i>Astronomical Object</i>, <i>Thoroughfare</i>, <i>Event</i>, 
                                          and <i>Taxon</i>.</p>
                                          <hr>
                                          <h4>WDCM Semantic Topic Models</h4>
                                          <h5>Suggested Readings</h5>
                                          <ul>
                                          <li><b>Distributional Semantics.</b> In <i>Wikipedia</i>. Retrieved October 24, 2017, from <a href = "https://en.wikipedia.org/wiki/Distributional_semantics" target = "_blank">https://en.wikipedia.org/wiki/Distributional_semantics</a></li>
                                          <li><b>Topic model.</b> In <i>Wikipedia</i>. Retrieved October 24, 2017, from <a href = "https://en.wikipedia.org/wiki/Topic_model" target = "_blank">https://en.wikipedia.org/wiki/Topic_model</a></li>
                                          <li><b>Latent Dirichlet allocation.</b> In <i>Wikipedia</i>. Retrieved October 24, 2017, from <a href = "https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation" target = "_blank">https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation</a></li>
                                          <li><b>Dimensionality reduction.</b> In <i>Wikipedia</i>. Retrieved October 24, 2017, from <a href = "https://en.wikipedia.org/wiki/Dimensionality_reduction" target = "_blank">https://en.wikipedia.org/wiki/Dimensionality_reduction</a></li>
                                          </ul>
                                          <p style="font-size:80%;">While <a href = "https://www.wikidata.org/wiki/Wikidata:Main_Page" target = "_blank">Wikidata</a> itself is a <a href = "https://en.wikipedia.org/wiki/Ontology_(information_science)" 
                                          target = "_blank">semantic ontology</a> with pre-defined and evolving normative rules of description and inference, <b>Wikidata usage</b> is essentialy a social, behavioral phenomenon, 
                                          suitable for study by means of <a href = "https://en.wikipedia.org/wiki/Machine_learning" target = "_blank">machine learning</a> in the field of <a href = "https://en.wikipedia.org/wiki/Distributional_semantics" 
                                          target = "_blank">distributional semantics</a>: the analysis and modeling of statistical patterns of occurrence and co-occurence of Wikidata item and property 
                                          usage across the client projects (e.g. <i>enwiki</i>, <i>frwiki</i>, <i>ruwiki</i>, etc). WDCM thus employes various statistical approaches in an attempt to describe and provide insights from the observable Wikidata 
                                          usage statistics (e.g. <a href = "https://en.wikipedia.org/wiki/Topic_model" target = "_blank">topic modeling</a>, <a href = "https://en.wikipedia.org/wiki/Cluster_analysis" target = "_blank">clustering</a>, 
                                          <a href = "https://en.wikipedia.org/wiki/Dimensionality_reduction" target = "_blank">dimensionality reduction</a>, all beyond providing elementary descriptive statistics of Wikidata usage, of course).
                                          <br>
                                          <b><i>Wikidata Usage Patterns.</b></i> The <i>“golden line”</i> that connects the reasoning behind all WDCM functions can be non-technically described in the following way. Imagine observing the number of times a set of 
                                          size <b>N</b> of particular Wikidata items was used across some project (<i>enwiki</i>, for example). Imagine having the same data or other projects as well: for example, if 200 projects are under analysis, then we 
                                          have <b>200</b> counts for <b>N</b> items in a set, and the data can be desribed by a <b>N x 200</b> matrix (<i>items</i> x <i>projects</i>). Each column of counts, representing the frequency of occurence of all Wikidata 
                                          entities under consideration across one of the 200 projects under discussion - a vector, obviously - represents a particular <i>Wikidata usage pattern</i>. By inspecting and modeling statistically the usage pattern matrix - 
                                          a matrix that encompasses all such usage patterns across the projects, or the derived covariance/correlation matrix - many insigths into the similarities between Wikimedia projects items projects (or, more precisely, 
                                          the similarities between their usage patterns) can be found.
                                          <br>In essence, the technology and mathematics behind WDCM relies on the same set of practical tools and ideas that support the development of <a href = "https://en.wikipedia.org/wiki/Semantic_search" target = "_blank">semantic search engines</a> 
                                          and <a href = "https://en.wikipedia.org/wiki/Recommender_system" target = "_blank">recommendation systems</a>, only applied to a specific dataset that encompasses the usage patterns for tens of millions of Wikidata entities across its client projects.</p>
                                          <hr>
                                          <h4>Dashboard: Semantic Models</h4>
                                          <p style="font-size:80%;">Each of the 14 currently used semantic categories in the WDCM Taxonomy of Wikidata items receives a separate topic model. Each topic model encompasses two or more 
                                          topics, or semantic themes. Here you can select a semantic category (e.g. "Geographical Object", "Human") and a particular topic from its model. The page will produce three outputs: 
                                          (1) the <i>Top 50 items in this topic</i> chart, which presents the 50 most important items in the select topic of the selected category\'s topic model, (2) the <i>Topic similarity network</i>, 
                                                   which presents the similarity structure among the 50 most important items in the selected topic, and (c) the <i>Top 50 projects in this topic</i> chart, where 50 Wikimedia projects in which the 
                                                   selected topic plays a prominent role in the selected semantic category.</p>
                                          <hr>
                                          <h4>Dashboard: Project Semantics</h4>
                                          <p style="font-size:80%;">Make a selection of Wikimedia projects here and hit <i>Apply Selection</i>. The Dashboard will produce a series of charts, one per each Wikidata semantic category that is 
                                           present in your selection of projects, and compute the relative importance (%) of each topic in the given selection and for each semantic category. Do not forget that category specific 
                                           semantic models do not necessarily encompass the same number of topics (in fact, they rarely do); also, <i>Topic n</i> in one category is obviously not the same thing as  <i>Topic n</i> in 
                                           some other category.</p>
                                         <hr>
                                         <h4>Dashboard: Similarity Maps</h4>
                                         <p style="font-size:80%;">Upon a selection of semantic category, the Dashboard will present a 2D map which represents the similarities between the Wikimedia projects computed from the selected category\'s 
                                         semantic model only. Here you can learn how similar or dissimilar are the sister projects in respect to their usage Wikidata items from a single semantic category.</p>
                                         <hr>
                                         <h4>License</h4>
                                         <br>
                                         <p><font size = 2><a href="https://github.com/wikimedia/analytics-wmde-WD-WikidataAnalytics/blob/master/LICENSE" target="_blank">License: BSD 3-Clause License</a></font></p>
                                          ')
                                ),
                                column(width = 1),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/ml/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Semantics-Dashboard" target = "_blank">GitHub</a></p>')
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Semantics :: Wikidata, WMDE 2019</b><br></p>'),
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
                                                                   package = "WDCMSemanticsDashboard"))
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/ml/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Semantics-Dashboard" target = "_blank">GitHub</a></p>')
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Semantics :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      )
                      
                    ) ### --- END tab Navigate
                    
                  ) ### --- END dashboardBody
                  
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
      app_title = 'WDCM_SemanticsDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

