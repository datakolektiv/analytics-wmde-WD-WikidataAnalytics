### ---------------------------------------------------------------------------
### --- WMDEdata::rdbs_runSQL.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::rdbs_runSQL()
### --- run SQL query from the Analytics Clients.
### ---------------------------------------------------------------------------

#' rdbs_runSQL
#'
#' Run SQL query from WMF Analytics Client(s)
#'
#' @param query SQL query
#' @param database wiki
#' @param localPath path to results (local filesystem)
#' @param localFilename result set local filesystem filename
#'
#' @return
#' @export
#'
#' @examples 
#' \dontrun {
#' rdbs_runSQL(
#'    query = "show tables;", 
#'    database = "enwiki", 
#'    localPath = "/home/myuser/", 
#'    localFilename = "tables.tsv")
#' }
rdbs_runSQL <- function(query,
                        database,
                        localPath,
                        localFilename) {

  # - run SQL:
  mySqlArgs <-
    paste0('/usr/local/bin/analytics-mysql')
  # - command:
  mySqlCommand <- paste0(mySqlArgs,
                         ' ',
                         database,
                         ' -e ',
                         query,
                         ' > ',
                         localPath,
                         localFilename)
  system(command = mySqlCommand,
         wait = TRUE)

}
