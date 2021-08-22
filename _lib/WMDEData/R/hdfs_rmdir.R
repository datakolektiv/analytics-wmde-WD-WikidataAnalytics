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

#' hdfs_rmdir
#'
#' @param kerberosUser character, the Kerberos user to impersonate
#' @param hdfsDir character, hdfsDir: the hdfs directory to remove
#'
#' @return
#' @export
#'
#' @examples
#' hdfs_rmdir(kerberosUser = "", hdfsDir = "")
hdfs_rmdir <- function(kerberosUser = "analytics-privatedata",
                       hdfsDir) {

  message("--- WMDEdata: mkdir in hdfs.")
  system(paste0('sudo -u ',
                kerberosUser,
                ' kerberos-run-command ',
                kerberosUser,
                ' hdfs dfs -rmr ',
                hdfsDir),
         wait = T)

}
