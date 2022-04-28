### ---------------------------------------------------------------------------
### --- WD Languages Landscape
### --- Version 1.0.0
### --- 2019.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Analytics (WA)
### --- https://wikidata-analytics.wmflabs.org/
### ---
### --- WA is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WA is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WA If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

#' Access files in the current app
#' 
#' NOTE: If you manually change your package name in the DESCRIPTION, 
#' don't forget to change it here too, and in the config file. 
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#' 
#' @param ... character vectors, specifying subdirectory and file(s) 
#' within your package. The default, none, returns the root of the app. 
#' 
#' @noRd
app_sys <- function(...){
  system.file(..., package = "WDLanguagesLandscape")
}


#' Read App Config
#' 
#' @param value Value to retrieve from the config file. 
#' @param config R_CONFIG_ACTIVE value. 
#' @param use_parent Logical, scan the parent directory for config file.
#' 
#' @noRd
get_golem_config <- function(
  value, 
  config = Sys.getenv("R_CONFIG_ACTIVE", "default"), 
  use_parent = TRUE
){
  config::get(
    value = value, 
    config = config, 
    # Modify this if your config file is somewhere else:
    file = app_sys("golem-config.yml"), 
    use_parent = use_parent
  )
}

