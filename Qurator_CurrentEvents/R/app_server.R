### ---------------------------------------------------------------------------
### --- Qurator: Current Events
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
### --- https://wikidata-analytics.wmflabs.org/
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

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {

  ### --- constants
  pubDir <- 
    'https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/Qurator/'
  googleNews <- 
    'https://news.google.com/search?q='
  
  # - output$updateTimestamp
  output$updateTimestamp <- renderText({
    # - get update stamp:
    h <- curl::new_handle()
    curl::handle_setopt(h,
                        copypostfields = "QURATOR_CurrentEvents");
    curl::handle_setheaders(h,
                            "Cache-Control" = "no-cache"
    )
    timestamp <- curl::curl_fetch_memory(URLencode(pubDir))
    timestamp <- rawToChar(timestamp$content)
    timestamp <- stringr::str_extract_all(timestamp, 
                                          "[[:digit:]]+-[[:digit:]]+-[[:digit:]]+\\s[[:digit:]]+:[[:digit:]]+")[[1]][1]
    timestamp <- trimws(timestamp, which = "both")
    ifelse(is.na(timestamp),
           "No update timestamp info is available at this point.",
           paste0("Updated: ", timestamp, " UTC")
    )
    
  })
  
  # - output$minutes10_update
  output$minutes10_update <- DT::renderDataTable({
    
    # - invalidate: every minute
    invalidateLater(1000 * 60, session)
    
    # - load data and provide
    dataSet <- readRDS(gzcon(url(paste0(pubDir,
                                        "aggRev_minutes10_stats.Rds"))))
    
    if (dim(dataSet)[1] > 0) {
      
      # - fix missing labels
      dataSet$label[nchar(dataSet$label) == 0 | 
                      grepl("No label defined", dataSet$label)] <- 
        dataSet$title[nchar(dataSet$label) == 0 | 
                        grepl("No label defined", dataSet$label)]
      
      # - produce html
      url <- paste0('https://www.wikidata.org/wiki/', 
                    dataSet$title)
      text <- paste0(dataSet$label,
                     " (",
                     dataSet$title,
                     ")")
      url <- paste0('<a href="', 
                    url, '" target="_blank">', 
                    text, 
                    "</a>")
      newsLink <- paste0(googleNews, 
                         dataSet$label) 
      newsLink <- paste0('<a href="', 
                         newsLink,
                         '" target="_blank">Search URL</a>')
      w <- which(grepl("No label defined", newsLink))
      if (length(w) > 0) {
        newsLink[w] <- ""
      }
      dataSet <- data.frame(Entity = url, 
                            Revisions = dataSet$revisions, 
                            Timestamp = dataSet$timestamp,
                            Editors = dataSet$n_users,
                            Search = newsLink,
                            stringsAsFactors = F) %>% 
        dplyr::filter(Revisions >= 3)
      
      DT::datatable(dataSet,
                    options = list(
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-left', targets = "_all"))
                    ),
                    rownames = FALSE, 
                    escape = F
      )
      
    } else {
      
      dataSet <- data.frame(`Message` = "No current events in the previous ten minutes.")
      
      DT::datatable(dataSet,
                    options = list(
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-left', targets = "_all"))
                    ),
                    rownames = FALSE,
                    escape = F
                    
      )
      
    }
    
  })
  
  # - output$hours1_update
  output$hours1_update <- DT::renderDataTable({
    
    # - invalidate: every minute
    invalidateLater(1000 * 60, session)
    
    # - load data and provide
    dataSet <- readRDS(gzcon(url(paste0(pubDir,
                                        "aggRev_hours1_stats.Rds"))))
    
    # - check if there is any current events, process or not
    if (dim(dataSet)[1] > 0) {
      
      # - fix missing labels
      dataSet$label[nchar(dataSet$label) == 0 | 
                      grepl("No label defined", dataSet$label)] <- 
        dataSet$title[nchar(dataSet$label) == 0 | 
                        grepl("No label defined", dataSet$label)]
      
      # - produce html
      url <- paste0('https://www.wikidata.org/wiki/', 
                    dataSet$title)
      text <- paste0(dataSet$label,
                     " (",
                     dataSet$title,
                     ")")
      url <- paste0('<a href="', 
                    url, '" target="_blank">', 
                    text, 
                    "</a>")
      newsLink <- paste0(googleNews, 
                         dataSet$label) 
      newsLink <- paste0('<a href="', 
                         newsLink,
                         '" target="_blank">Search URL</a>')
      w <- which(grepl("No label defined", newsLink))
      if (length(w) > 0) {
        newsLink[w] <- ""
      }
      dataSet <- data.frame(Entity = url, 
                            Revisions = dataSet$revisions, 
                            Timestamp = dataSet$timestamp,
                            Editors = dataSet$n_users,
                            Search = newsLink,
                            stringsAsFactors = F)%>% 
        dplyr::filter(Revisions >= 5)
      
      DT::datatable(dataSet,
                    options = list(
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-left', targets = "_all"))
                    ),
                    rownames = FALSE,
                    escape = F
                    
      )
      
    } else {
      
      dataSet <- data.frame(`Message` = "No current events in the previous hour.")
      
      DT::datatable(dataSet,
                    options = list(
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-left', targets = "_all"))
                    ),
                    rownames = FALSE,
                    escape = F
                    
      )
      
    }
    
    
  })
  
}
