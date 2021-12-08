### ---------------------------------------------------------------------------
### --- WD Human vs Bot Edits
### --- Version 1.0.0
### --- 2019.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de

## 
golem::fill_desc(
  pkg_name = "WDHumanEdits", # The Name of the package containing the App 
  pkg_title = "Wikidata Human vs Bot Edits", # The Title of the package containing the App 
  pkg_description = "The dashboards reports on and visualizes Wikidata edits, separately for humans and bots.",  
  author_first_name = "Goran", # Your First Name
  author_last_name = "Milovanović", # Your Last Name
  author_email = "goran.milovanovic_ext@wikimedia.de", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct()

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()
