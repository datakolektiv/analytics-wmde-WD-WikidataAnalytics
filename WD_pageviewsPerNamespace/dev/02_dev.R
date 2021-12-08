### ---------------------------------------------------------------------------
### --- WD Pageviews per Namespace
### --- Version 1.0.0
### --- 2019.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package(shiny)
usethis::use_package(shinycssloaders)
usethis::use_package(dplyr)
usethis::use_package(tidyr)
usethis::use_package(httr)
usethis::use_package(curl)
usethis::use_package(stringr)
usethis::use_package(dygraphs)
usethis::use_package(xts)

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "dygraph" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )
