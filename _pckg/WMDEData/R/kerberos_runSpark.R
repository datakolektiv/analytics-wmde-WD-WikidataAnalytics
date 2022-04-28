### ---------------------------------------------------------------------------
### --- WMDEdata::kerberos_runSpark.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::kerberos_runSpark()
### --- run PySpark program under kerberos auth
### --- from the Analytics Clients in the WMF Analytics Cluster.
### ---------------------------------------------------------------------------

#' kerberos_runSpark
#'
#' Run Pyspark script from WMF Analytics Client(s)
#'
#' @param kerberosUser kerberos user
#' @param pysparkPath full path to the Pyspark program
#' @param sparkMaster sparkMaster master
#' @param sparkDeployMode sparkDeployMode
#' @param sparkNumExecutors sparkNumExecutors
#' @param sparkDriverMemory sparkDriverMemory
#' @param sparkExecutorMemory sparkExecutorMemory
#' @param sparkConfigDynamic sparkConfigDynamic
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun {
#' kerberos_runSpark(
#'    kerberosUser = "", 
#'    pysparkPath = "", 
#'    sparkMaster = "", 
#'    sparkDeployMode = "", 
#'    sparkNumExecutors = "", 
#'    sparkDriverMemory = "", 
#'    sparkExecutorMemory = "", 
#'    sparkConfigDynamic = "")
#' }
kerberos_runSpark <- function(kerberosUser,
                              pysparkPath,
                              sparkMaster,
                              sparkDeployMode,
                              sparkNumExecutors,
                              sparkDriverMemory,
                              sparkExecutorMemory,
                              sparkConfigDynamic) {


  # - run PySpark:
  message("--- WMDEdata: run PySpark program.")
  system(command = paste0('sudo -u ', kerberosUser,
                          ' spark2-submit ',
                          sparkMaster, ' ',
                          sparkDeployMode, ' ',
                          sparkNumExecutors, ' ',
                          sparkDriverMemory, ' ',
                          sparkExecutorMemory, ' ',
                          sparkConfigDynamic, ' ',
                          pysparkPath),
         wait = T)

}
