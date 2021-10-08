#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Current Events (QCE)
### --- Version 1.0.0
### --- Script: currentEvents_Revolver.R
### --- September 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: Continuous Wikibase API contact for QCE
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of QURATOR Current Events (QCE)
### ---
### --- QCE is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- QCE is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with QCE If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Setup
library(magrittr)
options(dplyr.summarise.inform = FALSE)

### --- Directory Tree
dataDir <- "data/"

### --- Constants
nTop_users <- 20

### --- Functions
# - fetch item labels in batches 
# - (max values = 50, MediaWiki API constraint)
wd_api_fetch_labels <- function(items, language, fallback, retry) {
  
  # - params:
  # - items - character vector of Wikidata identifiers
  # - language - character, ISO 639-1 two-letter language code
  # - fallback - to use or not to use the Wikidata language fallback 
  # - retry - how many times to retry each batch against the API
  
  # - API prefix    
  APIprefix <- 
    "https://www.wikidata.org/w/api.php?action=wbgetentities&"
  
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
    if (length(w) > 0) {
      searchItems <- searchItems[-w]
    }
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
    
    retryCount <- retry
    repeat {
      res <- tryCatch(
        {
          httr::GET(url = URLencode(query))
        },
        error = function(condition) {
          NULL
        },
        warning = function(condition) {
          NULL
        }
      )
      if (is.null(res)) {
        Sys.sleep(5)
        retryCount <- retryCount - 1
      }
      if ((retryCount == 0) | !is.null(res)) {
        break
      }
    }
    
    if (!is.null(res)) {
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
    } else {
      itemLabels <- data.frame(title = ids, 
                               en_label = "Could not retrieve label.", 
                               stringsAsFactors = F, 
                               row.names = c())
      c <- c + 1
      iLabs[[c]] <- itemLabels
    }
    
    if (length(searchItems) < 50) {
      break
    } else {
      ixStart <- ixStart + 50
      # - pause here 1 sec
      Sys.sleep(1)
    }
  }
  iLabs <- data.table::rbindlist(iLabs)
  iLabs <- as.data.frame(iLabs)
  iLabs$en_label[nchar(iLabs$en_label) == 0] <- 
    "No label defined"
  return(iLabs)
}


### --- Revolver

repeat {
  
  # - check if there is any data present
  lF <- list.files(dataDir)
  if ("batch.Rds" %in% lF) {
    
    # - load batch
    batch <- 
      readRDS(paste0(dataDir, "batch.Rds"))
    
    # - load aggregate: 1 hour
    aggRev_hours1 <- 
      readRDS(paste0(dataDir, "aggRev_hours1.Rds"))
    
    # - endTimestamp
    endTimestamp <- as.character(max(batch$timestamp))
    checkEndT <- grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\d\\s\\d\\d:\\d\\d:\\d\\d$", 
                       endTimestamp)
    if (checkEndT) {
      endTimestamp <- paste0(substr(endTimestamp, 1, 10),
                             "T",
                             substr(endTimestamp, 12, 19),
                             "Z"
      )
    } else {
      endTimestamp <- paste0(endTimestamp, 
                             "T", 
                             "00:00:00Z")
    }
    
    # - startTimestamp
    startTimestamp <- as.character(Sys.time())
    checkStartT <- grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\d\\s\\d\\d:\\d\\d:\\d\\d$",
                         startTimestamp)
    if (checkStartT) {
      startTimestamp <- paste0(substr(startTimestamp, 1, 10),
                               "T",
                               substr(startTimestamp, 12, 19),
                               "Z"
      )
    } else {
      startTimestamp <- paste0(startTimestamp, 
                               "T", 
                               "00:00:00Z")
    }
    
    # - API call: update
    rc <- list()
    APIcall <- paste0('https://www.wikidata.org/w/api.php?action=query',
                      '&list=recentchanges',
                      '&rcstart=', startTimestamp,
                      '&rcend=', endTimestamp,
                      '&rcnamespace=0',
                      '&rclimit=500',
                      '&rcprop=title|redirect|timestamp|ids|user',
                      '&rctype=edit',
                      '&format=json')
    counter = 1
    repeat {
      # - contact the API
      result <- httr::GET(url = URLencode(APIcall))
      # - parse result
      result <- rawToChar(result$content)
      # - to JSON:    
      result <- tryCatch({
        jsonlite::fromJSON(result,
                           simplifyDataFrame = T)
      },
      error = function(condition) {
        NULL
      },
      warning = function(condition) {
        NULL
      })
      if (is.null(result)) {
        next
      }
      # - content:
      rc[[counter]] <- result$query$recentchanges
      # - check if there are more results
      if (!is.null(result$continue$continue)) {
        # - pick up continuation parameters
        continue <- result$continue$continue
        rccontinue <- result$continue$rccontinue
        # - increase counter
        counter <- counter + 1
        # - Compose continuation query
        APIcall <- paste0(APIcall,
                          '&continue=', continue,
                          '&rccontinue=', rccontinue)
      } else {
        break
      }
    }
    
    # - compose rc
    rc <- as.data.frame(
      data.table::rbindlist(rc,
                            use.names = T,
                            fill = T)
    )
    
    # - wrangle recent changes
    rc <- dplyr::select(rc, -one_of('old_revid', 'rcid'))
    rc$timestamp <- gsub("T", " ", rc$timestamp)
    rc$timestamp <- gsub("Z", "", rc$timestamp)
    rc$timestamp <- as.POSIXct(rc$timestamp)
    
    # - clean up
    rcCol <- colnames(rc)
    if (('type' %in% rcCol) & ('ns' %in% rcCol) & ('user' %in% rcCol)) {
      if ('redirect' %in% colnames(rc)) {
        rc <- dplyr::filter(rc,
                            type == "edit",
                            ns == 0,
                            is.na(redirect)) %>% 
          dplyr::select(title, revid, timestamp, user)
      } else {
        rc <- dplyr::filter(rc,
                            type == "edit",
                            ns == 0) %>% 
          dplyr::select(title, revid, timestamp, user)
      }
    } else {
      next
    }
    
    # - fetch item labels
    items <- unique(rc$title)
    iLabs <- tryCatch({
      wd_api_fetch_labels(items,
                          'en',
                          fallback = T,
                          retry = 10)
    }, 
    error = function(condition) {
      NULL
    })
    if(is.null(iLabs)) {
      next
    }
    
    # - join item labels with rc
    rc <- dplyr::left_join(rc, iLabs, by = "title")
    rc$en_label[is.na(rc$en_label)] <- ""
    
    # - store batch
    saveRDS(rc, 
            paste0(dataDir, "batch.Rds"))
    
    # - update aggregate: 1 hour
    aggRev_hours1 <- rbind(aggRev_hours1, rc)
    # - check for duplicated revisions, if any
    w <- duplicated(
      aggRev_hours1[, 
                    c('title', 'revid', 'timestamp', 'user')]
    )
    if (length(w) > 0) {
      aggRev_hours1 <- aggRev_hours1[-w, ]
    }
    # - keep 1 hour of data only
    aggRev_hours1 <- dplyr::arrange(aggRev_hours1, desc(timestamp))
    cutOff_timestamp <- max(aggRev_hours1$timestamp) - 60*60
    aggRev_hours1 <- dplyr::filter(aggRev_hours1,
                                   timestamp >= cutOff_timestamp)
    # - save aggregate: 1 hour
    saveRDS(aggRev_hours1, 
            paste0(dataDir, "aggRev_hours1.Rds"))
    
    # - produce statistics for 1 hour
    aggRev_hours1_stats <- aggRev_hours1 %>% 
      dplyr::select(title, en_label, timestamp, user) %>% 
      dplyr::group_by(title) %>% 
      dplyr::summarise(revisions = dplyr::n(), 
                       label = tail(en_label, 1), 
                       timestamp = max(timestamp), 
                       n_users = dplyr::n_distinct(user)) %>% 
      dplyr::arrange(dplyr::desc(revisions))
    aggRev_hours1_stats <- dplyr::filter(aggRev_hours1_stats,
                                         n_users >= 3)
    frequencies <- head(sort(
      unique(aggRev_hours1_stats$n_users), decreasing = T), 
      nTop_users)
    aggRev_hours1_stats <- aggRev_hours1_stats %>%
      dplyr::filter(revisions %in% frequencies)
    # - save statistics for 1 hour
    saveRDS(aggRev_hours1_stats, 
            paste0(dataDir, "aggRev_hours1_stats.Rds"))
    
  } else {
    
    # - start now
    
    # - startTimestamp
    startTimestamp <- as.character(Sys.time())
    startTimestamp <- paste0(substr(startTimestamp, 1, 10),
                             "T",
                             substr(startTimestamp, 12, 19),
                             "Z")
    
    # - API call
    APIcall <- paste0('https://www.wikidata.org/w/api.php?action=query',
                      '&list=recentchanges',
                      '&rcstart=', startTimestamp,
                      '&rcnamespace=0',
                      '&rclimit=100',
                      '&rcprop=title|redirect|timestamp|ids|user',
                      '&rctype=edit',
                      '&format=json')
    
    # - this is the first batch
    res <- httr::GET(url = URLencode(APIcall))
    rc <- rawToChar(res$content)
    rc <- tryCatch({jsonlite::fromJSON(rc)$query$recentchanges}, 
                   error = function(condition) {
                     NULL
                   }, 
                   warning = function(condition) {
                     NULL
                   })
    if (is.null(rc)) {
      next
    }
    
    # - wrangle batch
    rc <- dplyr::select(rc, -one_of('old_revid', 'rcid'))
    rc$timestamp <- gsub("T", " ", rc$timestamp)
    rc$timestamp <- gsub("Z", "", rc$timestamp)
    rc$timestamp <- as.POSIXct(rc$timestamp)
    
    # - clean up
    rcCol <- colnames(rc)
    if (('type' %in% rcCol) & ('ns' %in% rcCol) & ('user' %in% rcCol)) {
      if ('redirect' %in% colnames(rc)) {
        rc <- dplyr::filter(rc,
                            type == "edit",
                            ns == 0,
                            is.na(redirect)) %>% 
          dplyr::select(title, revid, timestamp, user)
      } else {
        rc <- dplyr::filter(rc,
                            type == "edit",
                            ns == 0) %>% 
          dplyr::select(title, revid, timestamp, user)
      }
    } else {
      next
    }
    
    # - fetch item labels in batches 
    items <- unique(rc$title)
    iLabs <- wd_api_fetch_labels(items, 
                                 'en', 
                                 fallback = T,
                                 retry = T)
    # - join item labels with rc
    rc <- dplyr::left_join(rc, iLabs, by = "title")
    
    # - save batch
    saveRDS(rc, paste0(dataDir, "batch.Rds"))
    
    # - save aggregate: 1 hour
    saveRDS(rc, paste0(dataDir, "aggRev_hours1.Rds"))
    
  }
  
}

