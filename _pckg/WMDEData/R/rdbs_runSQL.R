### ---------------------------------------------------------------------------
### --- WMDEdata::rdbs_runSQL.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::rdbs_runSQL()
### --- run SQL query from the Analytics Clients.
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

#' rdbs_runSQL
#'
#' Run SQL query from WMF Analytics Client(s)
#'
#' @param query SQL query
#' @param database wiki
#' @param localPath path to results (local filesystem)
#' @param localFilename result set local filesystem filename
#'
#' @return
#' @export
#'
#' @examples rdbs_runSQL(query = "show tables;", database = "enwiki", localPath = "/home/goransm/", localFilename = "tables.tsv")
rdbs_runSQL <- function(query,
                        database,
                        localPath,
                        localFilename) {


  # - run SQL:
  mySqlArgs <-
    paste0('/usr/local/bin/analytics-mysql')
  # - command:
  mySqlCommand <- paste0(mySqlArgs,
                         ' ',
                         database,
                         ' -e ',
                         query,
                         ' > ',
                         localPath,
                         localFilename)
  system(command = mySqlCommand,
         wait = TRUE)

}
