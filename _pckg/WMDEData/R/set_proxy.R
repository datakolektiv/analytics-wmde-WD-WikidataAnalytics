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
#' \dontrun{
#' set_proxy(
#'    http_proxy = "http://webproxy.eqiad.wmnet:8080", 
#'    https_proxy = "http://webproxy.eqiad.wmnet:8080")
#' }
set_proxy <- function(http_proxy = "http://webproxy.eqiad.wmnet:8080",
                      https_proxy = "http://webproxy.eqiad.wmnet:8080") {


  # - set proxy
  message("--- WMDEdata: set proxy for WMF Analytics Client.")
  Sys.setenv(
    http_proxy = http_proxy,
    https_proxy = https_proxy)

}
