### ---------------------------------------------------------------------------
### --- WD Inequality
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
### --- This file is part of Wikidata Analytics (WA)
### --- https://wikidata-analytics.wmcloud.org/
### ---
### --- WA is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WA is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WA If not, see <http://www.gnu.org/licenses/>.
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
        column(width = 6,
               br(),
               tags$img(src = "www/Wikidata-logo-en.png"),
               br(),
               HTML('<p style="font-size:110%;"align="left"><b>Wikidata Edits: Hoover Inequality Score</b></p>
                  <p style="font-size:80%;"align="left">The Hoover index, also known as the Robin Hood index or the Schutz index, 
                  is a measure of income metrics. It is equal to the portion of the total community income that would have to be 
                  redistributed (taken from the richer half of the population and given to the poorer half) for there to be 
                  income uniformity [Source: <a href="https://en.wikipedia.org/wiki/Hoover_index" target="_blank">Hoover Index, 
                  from English Wikipedia</a>]. Here we use the Hoover Index to describe the inequality in the distribution of 
                  Wikidata edits across the community. The distribution of user edits is obtained from the 
                  <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Edits/MediaWiki_history" target="_blank">wmf.mediawiki_history</a> 
                  table in the WMF Data Lake. Bots are filtered out.</p>')
        )
      ),
      fluidRow(
        column(width = 6,
               hr(),
               HTML('<h4>Hoover index</h4>
             <p style="font-size:80%;"align="left">Columns:
             <b>Measurement:</b> current_snapshot - edit inequality computed from the current wmf.mediawiki_history snapshot (i.e. the current month) only, 
             current_year - edit inequality computed for the current year only, history_to_current_snapshot - 
             edit inequality computed from the beginning of time and up to the current wmf.mediawiki_history snapshot (i.e. up to the current month), 
             <b>Snapshot:</b> the respective wmf.mediawiki_history snapshot, 
             <b>Hoover Index:</b> the value of the inequality index.</p>'),
               DT::dataTableOutput('hooverFrame', width = "100%"),
               downloadButton('hooverFrame_download',
                              'Download (csv)'),
               hr(),
               HTML('<p style="font-size:80%;"align="left"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm<br><b>Wikimedia Deutschland, 2021.</b></p>')
        ),
        column(width = 6, 
               hr(), 
               shinycssloaders::withSpinner(plotly::plotlyOutput('hooverIndexPlot',
                                                                 width = "100%",
                                                                 height = "600px"))
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
      app_title = 'WD_Inequality'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

