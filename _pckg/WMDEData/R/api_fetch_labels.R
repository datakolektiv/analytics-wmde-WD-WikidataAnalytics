### ---------------------------------------------------------------------------
### --- WMDEdata::api_fetch_labels.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::api_fetch_labels()
### --- obtains a list of Wikidata labels in the desired language
### --- from a character vector of Q ids.
### ---------------------------------------------------------------------------

#' api_fetch_labels
#'
#' Fetch item labels from via the Wikibase API.
#'
#' @param items character
#' @param language ISO 639-1 two-letter language code
#' @param fallback boolean: whether to use the Wikidata language fallback chain or not
#' @param APIprefix Wikibase API prefix
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' api_fetch_labels(
#'    items = paste0("Q", 1:100), 
#'    language = "en", 
#'    fallback = TRUE, 
#'    APIprefix = "https://www.wikidata.org/w/api.php?action=wbgetentities&")
#' }
api_fetch_labels <- function(items,
                             language = "en",
                             fallback = TRUE,
                             APIprefix = 'https://www.wikidata.org/w/api.php?action=wbgetentities&') {

  message("--- WMDEdata: search for labels through API.")

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
        Sys.sleep(10)
        httr::GET(url = utils::URLencode(query))
      },
      warning = function(condition) {
        Sys.sleep(10)
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
      # - pause here 1 sec
      Sys.sleep(1)
    }
  }
  iLabs <- data.table::rbindlist(iLabs)
  iLabs <- as.data.frame(iLabs)
  iLabs$en_label[nchar(iLabs$en_label) == 0] <- ""
  colnames(iLabs) <- c('item', 'label')

  # - output:
  return(iLabs)

}
