### ---------------------------------------------------------------------------
### --- wdcmFunctions.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- helper functions for WDCM
### ---------------------------------------------------------------------------

### --- Function: wd_api_fetch_labels()
# - fetch item labels in batches 
# - (max values = 50, MediaWiki API constraint)
wd_api_fetch_labels <- function(items, language, fallback, proxy) {
  
  # - params:
  # - items - character vector of Wikidata identifiers
  # - language - character, ISO 639-1 two-letter language code
  # - fallback - to use or not to use the Wikidata language fallback 
  # - proxy: character() length 2, http proxy, https proxy
  
  # - set proxy
  Sys.setenv(
    http_proxy = proxy[1],
    https_proxy = proxy[2])
  
  # - API prefix    
  APIprefix <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'
  
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
        GET(url = URLencode(query))
      },
      error = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      },
      warning = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      }
    )
    rclabs <- rawToChar(res$content)
    rclabs <- fromJSON(rclabs)
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
      # - pause here 1 sec
      Sys.sleep(1)
    }
  }
  iLabs <- rbindlist(iLabs)
  iLabs <- as.data.frame(iLabs)
  iLabs$en_label[nchar(iLabs$en_label) == 0] <- 'No label defined'
  return(iLabs)
}


