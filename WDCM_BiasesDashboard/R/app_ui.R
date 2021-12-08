### ---------------------------------------------------------------------------
### --- WDCM Biases
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
                    title = "WDCM: Biases",
                    titleWidth = 230
                  ), 
                  ### ---- END dashboardHeader
                  
                  ### --- dashboardSidebar
                  ### --------------------------------
                  
                  shinydashboard::dashboardSidebar(
                    sidebarMenu(
                      id = "tabsWDCM",
                      menuItem(text = "Gender Bias", 
                               tabName = "genderbias", 
                               icon = icon("barcode"),
                               selected = TRUE
                               
                      ),
                      menuItem(text = "Gender Bias per Project", 
                               tabName = "genderbiasperproject", 
                               icon = icon("barcode"),
                               selected = FALSE
                               
                      ),
                      menuItem(text = "Gender Bias per Profession", 
                               tabName = "genderbiasperprofession", 
                               icon = icon("barcode"),
                               selected = FALSE
                               
                      ),
                      menuItem(text = "Gender Usage Diversity", 
                               tabName = "genderusagediversity", 
                               icon = icon("barcode"),
                               selected = FALSE
                               
                      ),
                      menuItem(text = "M/F Ratio in Large Projects", 
                               tabName = "mfratiolargeprojects", 
                               icon = icon("barcode"),
                               selected = FALSE
                               
                      ),
                      menuItem(text = "Gender and Geography", 
                               tabName = "genderandgeography", 
                               icon = icon("barcode"),
                               selected = FALSE
                               
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
                      
                      ### --- TAB: genderbias
                      ### --------------------------------
                      
                      tabItem(tabName = "genderbias",
                              fluidRow(
                                column(width = 9,
                                       h4('WDCM Biases Dashboard'),
                                       HTML('<p style="font-size:80%;"align="left">This page presents the global gender bias in Wikidata usage across all WMF sister projects.
                                             The detailed overview of the gender bias in Wikidata usage per project is found on the following tabs.<br>
                                             <b>Note.</b> The WDCM Biases dashboards reports mainly on Wikidata <b>re-use</b> across the Wikimedia projects.<br>
                                             Precisely: we focus on the <a href = "https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target = "_blank">wbc_entity_usage table</a> 
                                             of the <a href = "https://www.mediawiki.org/wiki/Wikibase/Schema" target = "_blank">Wikibase Schema</a> here.<br>
                                             To obtain the statistics for items as present in Wikidata itself, see: <a href="https://www.denelezh.org/gender-gap/" target="_blank">Denelezh — Gender Gap in Wikidata</a></p>')
                                ),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Biases-Dashboard" target = "_blank">GitHub</a></p>'),
                                       htmlOutput('updateInfo')
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       h4('No. of items per gender currently in Wikidata:'),
                                       br(),
                                       fluidRow(
                                         column(width = 2,
                                                shinycssloaders::withSpinner(valueBoxOutput('stat1_M', width = 12))
                                         ),
                                         column(width = 2,
                                                shinycssloaders::withSpinner(valueBoxOutput('stat2_F', width = 12))
                                         ),
                                         column(width = 2,
                                                shinycssloaders::withSpinner(valueBoxOutput('stat3_Intersex', width = 12))
                                         ),
                                         column(width = 2,
                                                shinycssloaders::withSpinner(valueBoxOutput('stat4_transM', width = 12))
                                         ),
                                         column(width = 2,
                                                shinycssloaders::withSpinner(valueBoxOutput('stat5_transF', width = 12))
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 9,
                                                HTML('<p style="font-size:80%;"align="left"><b>Note.</b> This statistics refer to the results of the latestet 
                                                   Wikidata JSON dump copy in HDFS, see <a href="https://phabricator.wikimedia.org/T209655" target = "blank">Phab:T209655</a></p>')
                                         )
                                       )
                                )
                              ),
                              fluidRow(
                                br(),
                                column(width = 6,
                                       br(),
                                       h4("Wikidata Item Re-Use per Gender: Rank vs Usage"),
                                       htmlOutput("genderUsage_Distribution_img"),
                                       br(),
                                       HTML('<font size=2><b>Description.</b> All Wikidata Male and Female items are ranked from the most to the least used on the horizontal axis. The vertical axis is the Wikidata usage statistic given on a 
                                                                    logarithmic scale.</font>')
                                ),
                                column(width = 6,
                                       br(),
                                       h4("Wikidata Item Re-Use per Gender"),
                                       htmlOutput("genderUsage_Distribution_jitterp_img"),
                                       br(),
                                       HTML('<font size=2><b>Description.</b> Each Wikidata item is represented by a point. The vertical axis is the Wikidata usage statistic given on a 
                                                                    logarithmic scale.</font>')
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       hr(),
                                       h4('Male and Female items and item re-use distributions'),
                                       fluidRow(
                                         column(width = 6,
                                                shinycssloaders::withSpinner(plotOutput('numGender'))
                                         ),
                                         column(width = 6,
                                                shinycssloaders::withSpinner(plotOutput('useGender'))
                                         )
                                       ),
                                       
                                       fluidRow(
                                         column(width = 6,
                                                h4("No. of Wikidata Items per Gender"),
                                                shinycssloaders::withSpinner(valueBoxOutput("nMaleItems", width = 6), size = .5),
                                                shinycssloaders::withSpinner(valueBoxOutput("nFemaleItems", width = 6), size = .5)
                                         ),
                                         column(width = 6,
                                                h4("Wikidata Item Re-Use per Gender"),
                                                shinycssloaders::withSpinner(valueBoxOutput("totalUsage_M", width = 6), size = .5),
                                                shinycssloaders::withSpinner(valueBoxOutput("totalUsage_F", width = 6), size = .5)
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 6, 
                                                br(),
                                                HTML('<font size=2><b>Definition.</b> The statistics are the count of Wikidata items across the gender categories of male and female. The Male and Female Wikidata items are all members of <a href = "https://www.wikidata.org/wiki/Q5" target="_blank">Q5 (Human)</a>, 
                                                                            in a <a href = "https://www.wikidata.org/wiki/Property:P21" target="_blank">P21 (sex or gender)</a> relation with <a href = "https://www.wikidata.org/wiki/Q6581097" target="_blank">Q6581097 (male)</a>
                                                                            or <a href = "https://www.wikidata.org/wiki/Q6581072" target="_blank">Q6581072 (female)</a>.</font>')
                                         ),
                                         column(width = 6, 
                                                br(),
                                                HTML('<font size=2><b>Definition.</b> These are the <b>WDCM usage statistics</b>. The current Wikidata item usage statistic definition under WDCM is the count of the number of pages in a particular client 
                                                                                     project where the respective Wikidata item is used.</font>')
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab: genderbiases
                      
                      # - tab: genderbiasperproject
                      tabItem(tabName = "genderbiasperproject",
                              fluidRow(
                                column(width = 6,
                                       h4('Gender bias in Wikidata usage per Project'),
                                       HTML('<font size = 2><b>Description of columns.</b> <i>M Usage</i> and <i>F Usage</i>: the WDCM usage statistics for Male and Female items, respectively. 
                                                                                     <i>M%</i> and <i>F%</i>: the respective usage percents for Male and Female items. <i>Project Type</i>: Wikipedia, Wikiquote, Wikivoyage, Wikinews, 
                                                                                     Wikisource, Wiktionary, Wikiversity, Wikibooks, or Other. <i>Probability (M>F)</i> A result of a Bayesian A/B test across the M and F usage statistics: 
                                                                                     the posterior probability that the Male items are used more then the Female items given the observed usage statistics. <i>CI 5%</i> and <i>CI 95%</i>: 
                                                                                     credible interval on (M-F)/F. <i>M/F</i> is the ratio of Male to Female usage statistics (i.e. how many times are Male items used more than Female items).<br>
                                                                                     <b>Note.</b> The test statistics and the M/F ratio are not computed when M or F - or F alone for M/F - usage statistics are zero.</font>'),
                                       hr()
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Biases-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       shinycssloaders::withSpinner(DT::dataTableOutput('projectTable', width = "100%"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab item genderbiasperproject
                      
                      # - tab: genderbiasperprofession
                      tabItem(tabName = "genderbiasperprofession",
                              fluidRow(
                                column(width = 6,
                                       h4('Gender Re-Use per Occupation'),
                                       HTML('<font size = 2>A selection of statistics and charts on gender bias in Wikidata usage per occupation. Scroll down for more results.</font>'),
                                       hr()
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Biases-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       HTML('<font size = 2>The top 100 most mentioned occupations in Wikimedia projects are selected and placed on the horizontal axis. The vertical axis represents the WDCM usage statictic, 
                                                                            for Male and Female items separately.</font>'),
                                       br(), br(),
                                       tabBox(id = 'tabset_genderOccPlot', 
                                              selected = 'Interactive', 
                                              width = 12,
                                              height = NULL, 
                                              side = "left",
                                              tabPanel("Interactive",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotly::plotlyOutput('genderOccPlot_interactive',
                                                                                                                  width = "100%",
                                                                                                                  height = "600px"))
                                                         )
                                                       )
                                              ),
                                              tabPanel("Static",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotOutput('genderOccPlot',
                                                                                                        width = "100%",
                                                                                                        height = "600px"))
                                                         )
                                                       )
                                              )
                                       ),
                                       hr()
                                ),
                                column(width = 12,
                                       h4('Gender Bias in Wikidata Re-Use per Occupation'),
                                       HTML('<font size = 2><b>Description of columns.</b> <i>Usage (M)</i> and <i>Usage (F)</i>: the WDCM usage statistics for Male and Female items, respectively, i.e. 
                                                                             how much M and F items having the respective occupation value are used. <i>Usage (Total)</i>: the sum of Usage (M) and Usage (F). <i>Wikidata Items (M) and (F)</i>:
                                                                             the number of male and female Wikidata items having the respective occupation value. <i>Probability Usage(M) > Usage(F)</i> A result of a Bayesian A/B test across 
                                                                             the M and F usage statistics: the posterior probability that the Male items are used more then the Female items per occupation, given the observed usage statistics. 
                                                                             <i>CI 5%</i> and <i>CI 95%</i>: credible interval on (Usage(M)-Usage(F))/Usage(F). <i>Usage(M)/Usage(F)</i> is the ratio of Male to Female usage statistics (i.e. how many times are Male items used more than Female items).<br>
                                                                                     <b>Note.</b> The test statistics and the M/F ratio are not computed when M or F - or F alone for M/F - usage statistics are zero.</font>'),
                                       br(), br(),
                                       shinycssloaders::withSpinner(DT::dataTableOutput('genderOccDT', width = "100%")), 
                                       hr()
                                ), 
                                column(width = 12,
                                       h4('Occupations were Female items are mentioned more than Male items'),
                                       br(), br(),
                                       shinycssloaders::withSpinner(DT::dataTableOutput('femaleOccDT', width = "100%"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab item genderbiasperprofession
                      
                      # - tab: genderusagediversity
                      tabItem(tabName = "genderusagediversity",
                              fluidRow(
                                column(width = 6,
                                       h4('Gender diversity in Wikidata'),
                                       HTML('<font size = 2><b>Description.</b> Think of Wikidata usage as a value that can be distributed among a number of individuals in an economy, 
                                                                                     drawing the following analogy: Wikidata items are taken to be individuals, and the sum of their total usage across our projects is taken to 
                                                                                     represent the total wealth. So, each Wikidata item\'s "worth" is measured by its Wikidata usage: the number of pages that make a mention of 
                                                                                     that item in our projects. Then rank all the items according to their usage in the Wikimedia universe and divide them in a number of equal-sized 
                                                                                     groups. For each group of items compute its share in the total Wikidata usage ("wealth"), and plot the cumulative percentage or proportion of 
                                                                                     Wikidata items covered by each successive group against their share (also expressed as cumulative percentage or proportion). What obtains is 
                                                                                     the <a href = "https://en.wikipedia.org/wiki/Lorenz_curve" target="_blank">Lorenz curve</a>, a concept in economics widely used to express the distribution of economic inequality in a particular society. 
                                                                                     The following figure presents two Lorenz curves, for male and female Wikidata human (Q5) items usage.</font>'),
                                       hr()
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Biases-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              # - fluidRow: Gender Diveristy: Lorenz and Gini
                              fluidRow(
                                column(width = 12,
                                       htmlOutput('Gender_LorenzCurves_img'),
                                       br()
                                )                                                              
                              ),
                              fluidRow(
                                column(width = 6, 
                                       HTML('<font size = 2>The diagonal represents the line of equality: a category of items that would be 
                                                                             found to have a straight, diagonal Lorenz function would be the one where all items are mentioned
                                                                             exactly the same number of times. The empirical Lorenz curves for the female (red) and male (blue) 
                                                                             Wikidata items usage are found far away from the line of equality, which is nothing strange and 
                                                                             unexpected. The finding is similar to, for example, the well-known facts about word usage frequency 
                                                                             distributions (see: <a href = "https://en.wikipedia.org/wiki/Zipf%27s_law" target="_blank">Zipf\'s Law</a>) in any language, where a small fraction of words is used predominantly 
                                                                             in comparison to the rest of the words that are used rarely. The closer an empirical curve gets to the 
                                                                             line of equality, the more equal distribution of wealth - Wikidata usage, in this case - it represents.
                                                                             Also, the more equal the distribution of Wikidata usage, the larger the respective the 
                                                                             <a href="https://en.wikipedia.org/wiki/Gini_coefficient" target="_blank">Gini coefficient</a> 
                                                                            (reported for both genders under the chart title).</font>'))
                              ),
                              fluidRow(
                                hr(),
                                column(width = 2,
                                       br(),
                                       tags$img(src = "www/Wikidata-logo-en.png")
                                ),
                                column(width = 10,
                                       hr(),
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab item genderusagediversity
                      
                      # - tab: mfratiolargeprojects
                      tabItem(tabName = "mfratiolargeprojects",
                              fluidRow(
                                column(width = 6,
                                       h4('M/F Wikidata usage ratio in large projects'),
                                       HTML('<font size = 2><b>Description.</b> Wikimedia projects are represented on the horizontal axis and ranked from those that 
                                                                                     make the most use of <a href="https://www.wikidata.org/wiki/Q5" target = "_blank">Q5 (Human)</a> items with <a href = "https://www.wikidata.org/wiki/Property:P21" target = "_blank">P21 (sex or gender)</a> defined (to the left) to those that make less use of them (to the right). 
                                                                                     Only the top 50 projects in respect to the usage of such items are presented. <i>The M/F Usage Ratio</i>, represented on the vertical axis, is the ratio of Male and Female 
                                                                                     Wikidata usage statistics (i.e. how many times are Male items used more than Female items). For example, if a particular 
                                                                                     project has an M/F value of 5, that means that five mentions of Male items are made in it for every single mention of a 
                                                                                     Female item. The detailed M/F statistics for all Wikimedia projects under consideration are provided in the table on the previous tab.</font>'),
                                       hr()
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Biases-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              # - fluidRow: Gender Bias per Project DT
                              fluidRow(
                                column(width = 12,
                                       tabBox(id = 'tabset_mfRatioPlot', 
                                              selected = 'Interactive', 
                                              width = 12,
                                              height = NULL, 
                                              side = "left",
                                              tabPanel("Interactive",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotly::plotlyOutput('mfRatioPlot_interactive',
                                                                                                                  width = "100%",
                                                                                                                  height = "600px"))
                                                         )
                                                       )
                                              ),
                                              tabPanel("Static",
                                                       fluidRow(
                                                         column(width = 12,
                                                                shinycssloaders::withSpinner(plotOutput('mfRatioPlot',
                                                                                                        width = "100%",
                                                                                                        height = "600px"))
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab item mfratiolargeprojects
                      
                      # - tab: genderandgeography
                      tabItem(tabName = "genderandgeography",
                              fluidRow(
                                column(width = 6,
                                       h4('Gender bias and the North-South Divide'),
                                       HTML('<font size = 2><b>Description.</b> Each marker in the maps below represents a birthplace of a person referred to by some Wikidata Q5 (Human) item. 
                                                                            The size of the marker corresponds to the total Wikidata usage of all Q5 items who were born in the respective place.</font>'),
                                       hr()
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Biases-Dashboard" target = "_blank">GitHub</a></p>')
                                )
                              ),
                              # - fluidRow: Gender Bias per Project DT
                              fluidRow(
                                column(width = 6,
                                       # - fluidRow: Data
                                       fluidRow(
                                         column(width = 12,
                                                HTML("<font size = 2><b>Wikidata male item birthplaces</b></font>"),
                                                br(), br(),
                                                htmlOutput('M_Items_Distribution_img')
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 12,
                                                br(),
                                                HTML("<font size = 2><b>Wikidata female item birthplaces</b></font>"),
                                                br(), br(),
                                                htmlOutput('F_Items_Distribution_img')
                                         )
                                       )
                                ),
                                column(width = 6,
                                       fluidRow(
                                         column(width = 12,
                                                br(),
                                                h4('Statistics'),
                                                HTML('<font size = 2><b>Description.</b> Please take into your consideration that the following statictis are based only on the 
                                                   usage data for those <a href = "https://www.wikidata.org/wiki/Q5" target = "_blank">Q5 (Human)</a> items with a geo-localized <a href = "https://www.wikidata.org/wiki/Property:P19" target = "_blank">birthplace</a> in Wikidata.
                                                   All numbers represent statistics are WDCM usage statistics, i.e.  the count of the number of pages across the projects where the respective Wikidata items are used.</font>'),
                                                hr()
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 12, 
                                                HTML("<font size = 2><b>Gender Distribution</b></font>"),
                                                hr()
                                         ),
                                         column(width = 6,
                                                shinycssloaders::withSpinner(valueBoxOutput("globalUsageM", width = 12), size = .5)
                                         ),
                                         column(width = 6,
                                                shinycssloaders::withSpinner(valueBoxOutput("globalUsageF", width = 12), size = .5)
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 12, 
                                                hr(),
                                                HTML("<font size = 2><b>North-South Divide</b></font>"),
                                                hr()
                                         ),
                                         column(width = 6,
                                                shinycssloaders::withSpinner(valueBoxOutput("globalUsageN", width = 12), size = .5)
                                         ),
                                         column(width = 6,
                                                shinycssloaders::withSpinner(valueBoxOutput("globalUsageS", width = 12), size = .5)
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 12, 
                                                hr(),
                                                HTML("<font size = 2><b>Gender Bias and North-South Divide</b></font>"),
                                                hr()
                                         ),
                                         column(width = 12,
                                                fluidRow(
                                                  column(width = 6,
                                                         shinycssloaders::withSpinner(valueBoxOutput("globalUsageMN", width = 12), size = .5)
                                                  ),
                                                  column(width = 6,
                                                         shinycssloaders::withSpinner(valueBoxOutput("globalUsageMS", width = 12), size = .5)
                                                  )
                                                ),
                                                fluidRow(
                                                  column(width = 6,
                                                         shinycssloaders::withSpinner(valueBoxOutput("globalUsageFN", width = 12), size = .5)
                                                  ),
                                                  column(width = 6,
                                                         shinycssloaders::withSpinner(valueBoxOutput("globalUsageFS", width = 12), size = .5)
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab item mfratiolargeprojects
                      
                      
                      tabItem(tabName = "navigate",
                              fluidRow(
                                column(width = 6,
                                       includeMarkdown(system.file("app/www/wdcmNavigate.html", 
                                                                   package = "WDCMBiasesDashboard")),
                                ),
                                column(width = 3),
                                column(width = 3,
                                       HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Biases-Dashboard" target = "_blank">GitHub</a></p>')
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
                                       HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                       br(), br()
                                )
                              )
                      ), # - end tab item navigate
                      tabItem(tabName = "description",
                              fluidRow(
                                column(width = 8,
                                       HTML('<h2>WDCM Biases Dashboard</h2>
                                                   <h4>Description<h4>
                                                   <hr>
                                                   <h4>Introduction<h4>
                                                   <br>
                                                   <p><font size = 2>This Dashboard is a part of the <b>Wikidata Concepts Monitor (WDMC)</b>. The WDCM system provides analytics on Wikidata usage
                                                   across the Wikimedia sister projects. The WDCM Biases Dashboard collects and visualizes usage statistics for Wikidata items that are members of <a href = "https://www.wikidata.org/wiki/Q5" target="_blank">Q5 (Human)</a>, 
                                                   in a <a href = "https://www.wikidata.org/wiki/Property:P21" target="_blank">P21 (sex or gender)</a> relation with <a href = "https://www.wikidata.org/wiki/Q6581097" target="_blank">Q6581097 (male)</a> 
                                                   or <a href = "https://www.wikidata.org/wiki/Q6581072" target="_blank">Q6581072 (female)</a>. All functions of this Dashboard are documented alongside the respective data visualizations and tables. 
                                                   To understand the WDCM usage statistics, check out the Definitions section.</font></p>
                                                   <p><font size = 2>If you are interested in <b>gender statistics on Wikidata alone</b> (reminder: the WDCM system tracks Wikidata usage across the WMF projects, not Wikidata item statistics <i>per se</i>), visit: 
                                                    <a href = "https://denelezh.dicare.org/gender-gap.php" target = "_blank">Gender Gap in Wikidata on denelezh.org</a></font></p> 
                                                   <hr>
                                                   <h4>Definitions</h4>
                                                   <br>
                                                   <p><font size = 2><b>N.B.</b> The current <b>Wikidata item usage statistic</b> definition is <i>the count of the number of pages in a particular client project
                                                   where the respective Wikidata item is used</i>. For example, if a Wikidata item is used once or more than once on a page, we take that as one mention of that item 
                                                   - no matter the number of its occurrences on that page. The current definition thus ignores the usage aspects completely.</font></p>
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
                                       HTML('<p style="font-size:80%;"><b>WDCM Biases :: Wikidata, WMDE 2019</b><br></p>'),
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
      app_title = 'WDCM_BiasesDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

