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
#' hdfs_copy_to(kerberosUser = "", localPath = "", localFilename = "", hdfsDir = "")
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
