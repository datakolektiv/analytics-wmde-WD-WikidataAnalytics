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
  # - shared data directory
  dataDir <- "data/"
  # - items to filter out
  # - see: https://phabricator.wikimedia.org/T297225#7554084
  filterOut <- c("Q16943273",
                 "Q4115189",
                 "Q17339402",
                 "Q4115189",
                 "Q13406268",
                 "Q15397819",
                 "Q16943273",
                 "Q17339402",
                 "Q17437798",
                 "Q17437796",
                 "Q17506997",
                 "Q17507019",
                 "Q17559452",
                 "Q17566023",
                 "Q17578745",
                 "Q17580674",
                 "Q17580678",
                 "Q17580679",
                 "Q17580680",
                 "Q17580682",
                 "Q17442550",
                 "Q18574815",
                 "Q18644427",
                 "Q20748091",
                 "Q20748092",
                 "Q20748093",
                 "Q20748094",
                 "Q26961029",
                 "Q26932615",
                 "Q28064618",
                 "Q51759403",
                 "Q85408509",
                 "Q85408938",
                 "Q85409163",
                 "Q85409310",
                 "Q85409446",
                 "Q85409596")

  ### --- functions
  api_fetch_labels <- 
    function(items,
             language = "en",
             fallback = TRUE,
             APIprefix = 'https://www.wikidata.org/w/api.php?action=wbgetentities&') {
    
    # - enforce item uniqueness
    items <- unique(items)
    # - iLabs: store batches
    iLabs <- list()
    
    # fetch items
    # - counter
    c <- 0
    # - batch start
    ixStart <- 1
    repeat {
      ixEnd <- ixStart + 50 - 1
      searchItems <- items[ixStart:ixEnd]
      w <- which(is.na(searchItems))
      if (length(w) > 0) {searchItems <- searchItems[-w]}
      ids <- paste(searchItems, collapse = "|")
      if (fallback == T) {
        query <- paste0(APIprefix,
                        'ids=', ids, '&',
                        'props=labels&languages=',
                        language,
                        '&languagefallback=&sitefilter=wikidatawiki&format=json')
      } else {
        query <- paste0(APIprefix,
                        'ids=', ids, '&',
                        'props=labels&languages=',
                        language,
                        '&sitefilter=wikidatawiki&format=json')
      }
      res <- tryCatch(
        {
          httr::GET(url = utils::URLencode(query))
        },
        error = function(condition) {
          httr::GET(url = utils::URLencode(query))
        },
        warning = function(condition) {
          httr::GET(url = utils::URLencode(query))
        }
      )
      rclabs <- rawToChar(res$content)
      rclabs <- jsonlite::fromJSON(rclabs)
      itemLabels <- unlist(lapply(rclabs$entities, function(x) {
        if (length(x$labels) > 0) {
          return(x$labels[[1]]$value)
        } else {
          return("")
        }
      }))
      itemLabels <- data.frame(title = names(itemLabels),
                               en_label = itemLabels,
                               stringsAsFactors = F,
                               row.names = c())
      c <- c + 1
      iLabs[[c]] <- itemLabels
      if (length(searchItems) < 50) {
        break
      } else {
        ixStart <- ixStart + 50
      }
    }
    iLabs <- data.table::rbindlist(iLabs)
    iLabs <- as.data.frame(iLabs)
    iLabs$en_label[nchar(iLabs$en_label) == 0] <- ""
    colnames(iLabs) <- c('item', 'label')
    
    # - output:
    return(iLabs)
    
  }
  
  ### --- logic
  
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
  output$hours6_update <- DT::renderDataTable(server=FALSE, {
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours6_stats.Rds"))
      # - filterOut:
      dataSet <- dataSet %>% 
        dplyr::filter(!(title %in% filterOut))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions)) %>% 
        dplyr::filter(n_users > 1) %>% 
        head(100)
      
      # - fix missing labels
      dataSet$label[nchar(dataSet$label) == 0 | 
                      grepl("No label defined", dataSet$label)] <- 
        dataSet$title[nchar(dataSet$label) == 0 | 
                        grepl("No label defined", dataSet$label)]
      w_missing <- which(grepl("^Q[[:digit:]]+$", dataSet$label))
      if (length(w_missing) > 0) {
        mlabs <- 
          api_fetch_labels(items = dataSet$title[w_missing],
                           language = "en",
                           fallback = TRUE,
                           APIprefix = 'https://www.wikidata.org/w/api.php?action=wbgetentities&')
        wmatch <- which(dataSet$title %in% mlabs$item)
        dataSet$label[wmatch] <- mlabs$label
      }
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
      dataSet <- data.frame(Entity = url, 
                            Editors = dataSet$n_users,
                            Revisions = dataSet$revisions, 
                            stringsAsFactors = F) %>% 
        dplyr::arrange(desc(Editors), desc(Revisions))
      
      if (dim(dataSet)[1] == 0) {
        msgSet <- data.frame(Message = 
                               "No items currently satisfy the criteria.")
        return(DT::datatable(msgSet,
                             options = list(
                               bFilter = 0,
                               width = '100%',
                               escape = F,
                               columnDefs = list(list(className = 'dt-left', 
                                                      targets = 1))
                             ),
                             rownames = FALSE, 
                             escape = F
        )
        )
      } else {
        return(
        DT::datatable(dataSet,
                      extensions = 'Buttons',
                      options = list(
                        bFilter = 0,
                        pageLength = 25,
                        width = '100%',
                        escape = F,
                        columnDefs = list(list(className = 'dt-right', 
                                               targets = 1:2)), 
                        dom = 'tpB',
                        buttons = c('csv')
                      ),
                      class = "display",
                      rownames = FALSE, 
                      escape = F
                      )
        )
        }
      
    }, 
    error = function(condition) {
      msgSet <- data.frame(Message = 
                             "No items currently satisfy the criteria.")
      return(DT::datatable(msgSet,
                           options = list(
                             bFilter = 0,
                             width = '100%',
                             escape = F,
                             columnDefs = list(list(className = 'dt-left', 
                                                    targets = 1))
                           ),
                           rownames = FALSE, 
                           escape = F
      )
      )
    })
    
  })
  
  # - output$hours24_update
  output$hours24_update <- DT::renderDataTable(server=FALSE, {
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours24_stats.Rds"))
      # - filterOut:
      dataSet <- dataSet %>% 
        dplyr::filter(!(title %in% filterOut))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions)) %>% 
        dplyr::filter(n_users > 1) %>% 
        head(100)
      
      # - fix missing labels
      dataSet$label[nchar(dataSet$label) == 0 | 
                      grepl("No label defined", dataSet$label)] <- 
        dataSet$title[nchar(dataSet$label) == 0 | 
                        grepl("No label defined", dataSet$label)]
      w_missing <- which(grepl("^Q[[:digit:]]+$", dataSet$label))
      if (length(w_missing) > 0) {
        mlabs <- 
          api_fetch_labels(items = dataSet$title[w_missing],
                           language = "en",
                           fallback = TRUE,
                           APIprefix = 'https://www.wikidata.org/w/api.php?action=wbgetentities&')
        wmatch <- which(dataSet$title %in% mlabs$item)
        dataSet$label[wmatch] <- mlabs$label
      }
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
      dataSet <- data.frame(Entity = url, 
                            Editors = dataSet$n_users,
                            Revisions = dataSet$revisions, 
                            stringsAsFactors = F) %>% 
        dplyr::arrange(desc(Editors), desc(Revisions))
      
      if (dim(dataSet)[1] == 0) {
        msgSet <- data.frame(Message = 
                               "No items currently satisfy the criteria.")
        return(DT::datatable(msgSet,
                             options = list(
                               bFilter = 0,
                               width = '100%',
                               escape = F,
                               columnDefs = list(list(className = 'dt-left', 
                                                      targets = 1))
                             ),
                             rownames = FALSE, 
                             escape = F
        )
        )
      } else {
        return(
          DT::datatable(dataSet,
                        extensions = 'Buttons',
                        options = list(
                          bFilter = 0,
                          pageLength = 25,
                          width = '100%',
                          escape = F,
                          columnDefs = list(list(className = 'dt-right', 
                                                 targets = 1:2)), 
                          dom = 'tpB',
                          buttons = c('csv')
                        ),
                        class = "display",
                        rownames = FALSE, 
                        escape = F
          )
        )
      }
      
    }, 
    error = function(condition) {
      msgSet <- data.frame(Message = 
                             "No items currently satisfy the criteria.")
      return(DT::datatable(msgSet,
                           options = list(
                             bFilter = 0,
                             width = '100%',
                             escape = F,
                             columnDefs = list(list(className = 'dt-left', 
                                                    targets = 1))
                           ),
                           rownames = FALSE, 
                           escape = F
      )
      )
    })
    
  })
  
  # - output$hours48_update
  output$hours48_update <- DT::renderDataTable(server=FALSE, {
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours48_stats.Rds"))
      # - filterOut:
      dataSet <- dataSet %>% 
        dplyr::filter(!(title %in% filterOut))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions)) %>% 
        dplyr::filter(n_users > 1) %>% 
        head(100)
      
      # - fix missing labels
      dataSet$label[nchar(dataSet$label) == 0 | 
                      grepl("No label defined", dataSet$label)] <- 
        dataSet$title[nchar(dataSet$label) == 0 | 
                        grepl("No label defined", dataSet$label)]
      w_missing <- which(grepl("^Q[[:digit:]]+$", dataSet$label))
      if (length(w_missing) > 0) {
        mlabs <- 
          api_fetch_labels(items = dataSet$title[w_missing],
                           language = "en",
                           fallback = TRUE,
                           APIprefix = 'https://www.wikidata.org/w/api.php?action=wbgetentities&')
        wmatch <- which(dataSet$title %in% mlabs$item)
        dataSet$label[wmatch] <- mlabs$label
      }
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
      dataSet <- data.frame(Entity = url, 
                            Editors = dataSet$n_users,
                            Revisions = dataSet$revisions, 
                            stringsAsFactors = F) %>% 
        dplyr::arrange(desc(Editors), desc(Revisions))
      
      if (dim(dataSet)[1] == 0) {
        msgSet <- data.frame(Message = 
                               "No items currently satisfy the criteria.")
        return(DT::datatable(msgSet,
                             options = list(
                               bFilter = 0,
                               width = '100%',
                               escape = F,
                               columnDefs = list(list(className = 'dt-left', 
                                                      targets = 1))
                             ),
                             rownames = FALSE, 
                             escape = F
        )
        )
      } else {
        return(
          DT::datatable(dataSet,
                        extensions = 'Buttons',
                        options = list(
                          bFilter = 0,
                          pageLength = 25,
                          width = '100%',
                          escape = F,
                          columnDefs = list(list(className = 'dt-right', 
                                                 targets = 1:2)), 
                          dom = 'tpB',
                          buttons = c('csv')
                        ),
                        class = "display",
                        rownames = FALSE, 
                        escape = F
          )
        )
      }
      
    }, 
    error = function(condition) {
      msgSet <- data.frame(Message = 
                             "No items currently satisfy the criteria.")
      return(DT::datatable(msgSet,
                           options = list(
                             bFilter = 0,
                             width = '100%',
                             escape = F,
                             columnDefs = list(list(className = 'dt-left', 
                                                    targets = 1))
                           ),
                           rownames = FALSE, 
                           escape = F
      )
      )
    })
    
    
  })
  
  # - output$hours72_update
  output$hours72_update <- DT::renderDataTable(server=FALSE, {
    
    tryCatch({
      
      # - invalidate: every 10 minutes
      invalidateLater(1000 * 60 * 10, session)
      
      # - load data and provide
      dataSet <- readRDS(paste0(dataDir, "aggRev_hours72_stats.Rds"))
      # - filterOut:
      dataSet <- dataSet %>% 
        dplyr::filter(!(title %in% filterOut))
      dataSet <- dplyr::arrange(dataSet, 
                                dplyr::desc(n_users),
                                dplyr::desc(revisions)) %>% 
        dplyr::filter(n_users > 1) %>% 
        head(100)
      
      # - fix missing labels
      dataSet$label[nchar(dataSet$label) == 0 | 
                      grepl("No label defined", dataSet$label)] <- 
        dataSet$title[nchar(dataSet$label) == 0 | 
                        grepl("No label defined", dataSet$label)]
      w_missing <- which(grepl("^Q[[:digit:]]+$", dataSet$label))
      if (length(w_missing) > 0) {
        mlabs <- 
          api_fetch_labels(items = dataSet$title[w_missing],
                           language = "en",
                           fallback = TRUE,
                           APIprefix = 'https://www.wikidata.org/w/api.php?action=wbgetentities&')
        wmatch <- which(dataSet$title %in% mlabs$item)
        dataSet$label[wmatch] <- mlabs$label
      }
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
      dataSet <- data.frame(Entity = url, 
                            Editors = dataSet$n_users,
                            Revisions = dataSet$revisions, 
                            stringsAsFactors = F) %>% 
        dplyr::arrange(desc(Editors), desc(Revisions))
      
      if (dim(dataSet)[1] == 0) {
        msgSet <- data.frame(Message = 
                               "No items currently satisfy the criteria.")
        return(DT::datatable(msgSet,
                             options = list(
                               bFilter = 0,
                               width = '100%',
                               escape = F,
                               columnDefs = list(list(className = 'dt-left', 
                                                      targets = 1))
                             ),
                             rownames = FALSE, 
                             escape = F
        )
        )
      } else {
        return(
          DT::datatable(dataSet,
                        extensions = 'Buttons',
                        options = list(
                          bFilter = 0,
                          pageLength = 25,
                          width = '100%',
                          escape = F,
                          columnDefs = list(list(className = 'dt-right', 
                                                 targets = 1:2)), 
                          dom = 'tpB',
                          buttons = c('csv')
                        ),
                        class = "display",
                        rownames = FALSE, 
                        escape = F
          )
        )
      }
      
    }, 
    error = function(condition) {
      msgSet <- data.frame(Message = 
                             "No items currently satisfy the criteria.")
      return(DT::datatable(msgSet,
                           options = list(
                             bFilter = 0,
                             width = '100%',
                             escape = F,
                             columnDefs = list(list(className = 'dt-left', 
                                                    targets = 1))
                           ),
                           rownames = FALSE, 
                           escape = F
      )
      )
    })
    
  })
  
}
