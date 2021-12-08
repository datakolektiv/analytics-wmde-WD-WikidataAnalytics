### ---------------------------------------------------------------------------
### --- WMDEdata::hdfs_mkdir.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::hdfs_mkdir()
### --- mkdir in hdfs (WMF Data Lake)
### ---------------------------------------------------------------------------

#' hdfs_mkdir
#'
#' @param kerberosUser character, the Kerberos user to impersonate
#' @param hdfsDir character, hdfsDir: the hdfs directory to mkdir
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' hdfs_mkdir(kerberosUser = "", hdfsDir = "")
#' }

hdfs_mkdir <- function(kerberosUser = "analytics-privatedata",
                       hdfsDir) {

  message("--- WMDEdata: mkdir in hdfs.")
  system(paste0('sudo -u ',
                kerberosUser,
                ' kerberos-run-command ',
                kerberosUser,
                ' hdfs dfs -mkdir ',
                hdfsDir),
         wait = T)

}
