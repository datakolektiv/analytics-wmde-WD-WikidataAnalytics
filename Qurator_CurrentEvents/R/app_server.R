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
  dataDir <- "data/"
  googleNews <- 
    'https://news.google.com/search?q='
  
  # - output$updateTimestamp
  output$updateTimestamp <- renderText({
    
    # - invalidate: every 10 minutes
    invalidateLater(1000 * 60 * 10, session)
    
    tryCatch({
      
      f <- file.info(paste0(dataDir, 
                            "batch.Rds"))$ctime
      attr(f, "tzone") <- "UTC"
      return(paste0("<b>Latest update batch timestamp:</b> ",
                    paste0(as.character(f),
                           " UTC"))
             )
      
    }, 
    error = function(condition) {
      return(NULL)
    })
    
  })
  
  # - output$hours6_update
  output$hours6_update <- DT::renderDataTable({
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours6_stats.Rds"))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions))
      
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
                         '" target="_blank">News search</a>')
      w <- which(grepl("No label defined", newsLink))
      if (length(w) > 0) {
        newsLink[w] <- ""
      }
      dataSet <- data.frame(Entity = url, 
                            Revisions = dataSet$revisions, 
                            Editors = dataSet$n_users,
                            stringsAsFactors = F) %>% 
        dplyr::filter(Revisions >= 3)
      
      DT::datatable(dataSet,
                    options = list(
                      bFilter = 0,
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-right', 
                                             targets = 1:2))
                    ),
                    rownames = FALSE, 
                    escape = F
      )
      
    }, 
    error = function(condition) {
      return(NULL)
    })
    
  })
  
  # - output$hours24_update
  output$hours24_update <- DT::renderDataTable({
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours24_stats.Rds"))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions))
      
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
                         '" target="_blank">News search</a>')
      w <- which(grepl("No label defined", newsLink))
      if (length(w) > 0) {
        newsLink[w] <- ""
      }
      dataSet <- data.frame(Entity = url, 
                            Revisions = dataSet$revisions, 
                            Editors = dataSet$n_users,
                            stringsAsFactors = F) %>% 
        dplyr::filter(Revisions >= 3)
      
      DT::datatable(dataSet,
                    options = list(
                      bFilter = 0,
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-right', 
                                             targets = 1:2))
                    ),
                    rownames = FALSE, 
                    escape = F
      )
      
    }, 
    error = function(condition) {
      return(NULL)
    })
    
  })
  
  # - output$hours48_update
  output$hours48_update <- DT::renderDataTable({
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours48_stats.Rds"))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions))
      
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
                         '" target="_blank">News search</a>')
      w <- which(grepl("No label defined", newsLink))
      if (length(w) > 0) {
        newsLink[w] <- ""
      }
      dataSet <- data.frame(Entity = url, 
                            Revisions = dataSet$revisions, 
                            Editors = dataSet$n_users,
                            stringsAsFactors = F) %>% 
        dplyr::filter(Revisions >= 3)
      
      DT::datatable(dataSet,
                    options = list(
                      bFilter = 0,
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-right', 
                                             targets = 1:2))
                    ),
                    rownames = FALSE, 
                    escape = F
      )
      
    }, 
    error = function(condition) {
      return(NULL)
    })
    
    
  })
  
  # - output$hours72_update
  output$hours72_update <- DT::renderDataTable({
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours72_stats.Rds"))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions))
      
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
                         '" target="_blank">News search</a>')
      w <- which(grepl("No label defined", newsLink))
      if (length(w) > 0) {
        newsLink[w] <- ""
      }
      dataSet <- data.frame(Entity = url, 
                            Revisions = dataSet$revisions, 
                            Editors = dataSet$n_users,
                            stringsAsFactors = F) %>% 
        dplyr::filter(Revisions >= 3)
      
      DT::datatable(dataSet,
                    options = list(
                      bFilter = 0, 
                      pageLength = 25,
                      width = '100%',
                      escape = F,
                      columnDefs = list(list(className = 'dt-right', 
                                             targets = 1:2))
                    ),
                    rownames = FALSE, 
                    escape = F
      )
      
    }, 
    error = function(condition) {
      return(NULL)
    })
    
    
  })
  
}
