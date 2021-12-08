### ---------------------------------------------------------------------------
### --- WMDEdata::wdqs_send_query.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::wdqs_send_query()
### --- Function to query WDQS
### ---------------------------------------------------------------------------

#' wdqs_send_query
#'
#' Function to query WDQS
#'
#' @param query character, a SPARQL query
#' @param SPARQL_Endpoint character, the WDQS endpoint to use
#' @param max_retry numeric, number of re-tries to attempt if anything fails
#'
#' @return
#' @export
#'
#' @examples 
#' \dontrun{
#' wdqs_send_query(
#'    query = 'SELECT ?item ?itemLabel WHERE { ?item wdt:P31 wd:Q146. }', 
#'    SPARQL_Endpoint = "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query=", 
#'    max_retry = 10)
#' }
wdqs_send_query <- function(query,
                            SPARQL_Endpoint = 
                              "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query=",
                            max_retry = 10) {

  # - Run query
  message("--- WMDEdata: send SPARQL query to WDQS.")
  for (c in 1:max_retry) {

    # - try: GET
    message("Query WDQS...")
    res <- tryCatch({
      httr::GET(url = paste0(SPARQL_Endpoint, utils::URLencode(query)))
    },
    error = function(condition) {
      return(NULL)
    },
    warning = function(condition) {
      return(NULL)
    })

    # - check: res
    if (is.null(res)) {
      message("Something's wrong on WDQS: retry or exit.")
      if (c == max_retry) {
        message("Max retry reached: exit.")
        break
      } else {
        message(paste0("Try:", c, ". Repeat query."))
        next
      }
    } else {

      # - check: status code
      message("Check server response status code...")
      if (res$status_code == 200) {
        message("Data retrieved with status code 200. Converting...")
        # - tryCatch rawToChar
        # - NOTE: might fail for very long vectors
        rc <- tryCatch({
          rawToChar(res$content)
        },
        error = function(condition) {
          return(NULL)
        })
      } else {
        message("Server response code is not 200. Retry or exit.")
        if (c == max_retry) {
          message("Max retry reached: exit.")
          break
        } else {
          message(paste0("Try:", c, ". Repeat query."))
          next
        }
      }

      # - check: rc
      message("Check conversion from raw to char...")
      if(is.null(rc)) {
        message("Conversion failed. Retry or exit.")
        if (c == max_retry) {
          message("Max retry reached: exit.")
          break
        } else {
          message(paste0("Try:", c, ". Repeat query."))
          next
        }
      } else {

        # - check: is.ExceptionTimeout
        message("Check WDQS timeout...")
        queryTimeout <- grepl("timeout", rc, ignore.case = TRUE)
        if (queryTimeout) {
          message("WDQS timeout detected. Retry or exit.")
          if (c == max_retry) {
            message("Max retry reached: exit.")
            break
          } else {
            message(paste0("Try:", c, ". Repeat query."))
            next
          }
        } else {
          message("Success. Exit.")
          break
        }

      }

    }

  }

  # - output
  return(rc)

  }
