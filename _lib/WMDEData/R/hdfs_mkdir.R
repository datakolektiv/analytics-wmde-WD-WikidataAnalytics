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

#' hdfs_mkdir
#'
#' @param kerberosUser character, the Kerberos user to impersonate
#' @param hdfsDir character, hdfsDir: the hdfs directory to mkdir
#'
#' @return
#' @export
#'
#' @examples
#' hdfs_mkdir(kerberosUser = "", hdfsDir = "")

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
