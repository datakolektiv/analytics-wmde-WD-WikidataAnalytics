### ---------------------------------------------------------------------------
### --- WDCM Overview
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------

## 
golem::fill_desc(
  pkg_name = "WDCMOverviewDashboard", # The Name of the package containing the App 
  pkg_title = "Wikidata Concepts Monitor Overview Dashboard", # The Title of the package containing the App 
  pkg_description = "PKG_DESC.", # The Description of the package containing the App 
  author_first_name = "Goran", # Your First Name
  author_last_name = "Milovanović", # Your Last Name
  author_email = "goran.milovanovic_ext@wikimedia.de", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
usethis::use_gpl_license(version = 2, include_future = TRUE)
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct()
