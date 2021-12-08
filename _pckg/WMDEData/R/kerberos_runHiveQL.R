### ---------------------------------------------------------------------------
### --- WMDEdata::kerberos_runHiveQL.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEdata::kerberos_runHiveQL()
### --- run HiveQL query and store result in the local filesystem
### --- under kerberos auth from the Analytics Clients.
### ---------------------------------------------------------------------------

#' kerberos_runHiveQL
#'
#' Run HiveQL script from the local filesystem on WMF Analytics Client(s)
#'
#' @param kerberosUser kerberosUser
#' @param query full local path to the HiveQL query
#' @param localPath the path to the directory where the files should be stored
#' @param localFilename the result set local filename
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' kerberos_runHiveQL(kerberosUser = "", 
#'    query = "", 
#'    localPath = "", 
#'    localFilename = "")
#' }
kerberos_runHiveQL <- function(kerberosUser,
                               query,
                               localPath,
                               localFilename) {

  # - arguments:
  # - kerberosUser - the Kerberos user to impersonate
  # - query - full local path to the HiveQL query
  # - localPath - the path to the directory where
  # - the files should be stored
  # - localFilename - the local filename

  # - check if localPath ends with "/"
  if (!grepl("/$", localPath)) {
    localPath <- paste0(localPath, "/")
  }

  # - run HiveQL query
  message("--- WMDEdata: run HiveQL query.")
  system(command = paste0('sudo -u ',
                          kerberosUser,
                          ' kerberos-run-command ',
                          kerberosUser,
                          ' /usr/local/bin/beeline --incremental=true --silent -f "',
                          query,
                          '" > ',
                          localPath,
                          localFilename),
         wait = T)

}
