### ---------------------------------------------------------------------------
### --- WMDEdata::hdfs_copy_to.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::hdfs_copy_to()
### --- copy from the Analytics Clients local filesystem
### --- to hdfs for the Analytics Cluster operations.
### ---------------------------------------------------------------------------

#' hdfs_copy_to
#'
#' @param kerberosUser character, Kerberos user to impersonate
#' @param localPath character, the path to the directory where the localFilename is stored
#' @param localFilename character, the local filename
#' @param hdfsDir character, the hdfs directory where the files will be placed
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' hdfs_copy_to(
#'    kerberosUser = "",
#'    localPath = "", 
#'    localFilename = "", 
#'    hdfsDir = "")
#' }
hdfs_copy_to <- function(kerberosUser,
                         localPath,
                         localFilename,
                         hdfsDir) {

  # - copy to hdfs:
  message("--- WMDEdata: copy to hdfs.")
  system(command = paste0('sudo -u ',
                          kerberosUser,
                          ' kerberos-run-command ',
                          kerberosUser,
                          ' hdfs dfs -put -f ',
                          paste0(localPath, localFilename),
                          ' ',
                          hdfsDir),
         wait = T)
}
