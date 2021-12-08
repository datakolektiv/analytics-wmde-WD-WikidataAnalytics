### ---------------------------------------------------------------------------
### --- WMDEdata::hdfs_rmdir.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::hdfs_rmdir()
### --- rmr in hdfs (WMF Data Lake)
### ---------------------------------------------------------------------------

#' hdfs_rmdir
#'
#' @param kerberosUser character, the Kerberos user to impersonate
#' @param hdfsDir character, hdfsDir: the hdfs directory to remove
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' hdfs_rmdir(kerberosUser = "", hdfsDir = "")
#' }
hdfs_rmdir <- function(kerberosUser = "analytics-privatedata",
                       hdfsDir) {

  message("--- WMDEdata: rm dir in hdfs.")
  system(paste0('sudo -u ',
                kerberosUser,
                ' kerberos-run-command ',
                kerberosUser,
                ' hdfs dfs -rmr ',
                hdfsDir),
         wait = T)

}
