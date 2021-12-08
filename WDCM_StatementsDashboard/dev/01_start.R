### ---------------------------------------------------------------------------
### --- WDCM Statements
### --- Version 1.0.0
### --- 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

golem::fill_desc(
  pkg_name = "WDCMStatementsDashboard", # The Name of the package containing the App 
  pkg_title = "PKG_TITLE", # The Title of the package containing the App 
  pkg_description = "PKG_DESC.", # The Description of the package containing the App 
  author_first_name = "AUTHOR_FIRST", # Your First Name
  author_last_name = "AUTHOR_LAST", # Your Last Name
  author_email = "AUTHOR@MAIL.COM", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct()

