### ---------------------------------------------------------------------------
### --- WMDEdata::set_proxy.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEdata::set_proxy()
### --- sets the http and https proxies from the WMF Analytics Clients
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

#' set_proxy
#'
#' Sets the http and https proxies from the WMF Analytics Clients.
#'
#' @param http_proxy character: the HTTP proxy
#' @param https_proxy character: the HTTPS proxy
#'
#' @return
#' @export
#'
#' @examples
#' set_proxy(http_proxy = "http://webproxy.eqiad.wmnet:8080", https_proxy = "http://webproxy.eqiad.wmnet:8080")
set_proxy <- function(http_proxy = "http://webproxy.eqiad.wmnet:8080",
                      https_proxy = "http://webproxy.eqiad.wmnet:8080") {


  # - set proxy
  message("--- WMDEdata: set proxy for WMF Analytics Client.")
  Sys.setenv(
    http_proxy = http_proxy,
    https_proxy = https_proxy)

}
