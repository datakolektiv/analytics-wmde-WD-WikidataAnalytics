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
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of the WMDEData project
### ---
### --- WLP is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WMDEData is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WMDEData If not, see <http://www.gnu.org/licenses/>.
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
