### ---------------------------------------------------------------------------
### --- WDCM Statements
### --- Version 1.0.0
### --- 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

## Run checks ----
devtools::check()

# Deploy
## ShinyProxy
golem::add_dockerfile_shinyproxy()

