### ---------------------------------------------------------------------------
### --- WMDEdata::kerberos_init.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEdata::kerberos_init()
### --- run HiveQL query and store result in the local filesystem
### --- under kerberos auth from the Analytics Clients.
### ---------------------------------------------------------------------------

#' kerberos_init
#'
#' @param kerberosUser Kerberos user to impersonate
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' kerberos_init("analytics-privatedata")
#' }
kerberos_init <- function(kerberosUser = "analytics-privatedata") {

  # - arguments:
  # - kerberosUser - the Kerberos user to impersonate

  # - Kerberos init
  message("--- WMDEdata: Kerberos init.")
  system(command = paste0('sudo -u ',
                          kerberosUser,
                          ' kerberos-run-command ',
                          kerberosUser,
                          ' hdfs dfs -ls'),
         wait = T)

}
