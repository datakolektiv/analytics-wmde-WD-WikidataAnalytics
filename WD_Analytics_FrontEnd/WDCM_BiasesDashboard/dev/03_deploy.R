### ---------------------------------------------------------------------------
### --- WDCM Biases
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()

# Deploy
## ShinyProxy
golem::add_dockerfile_shinyproxy()
