### ---------------------------------------------------------------------------
### --- WD Inequality
### --- Version 1.0.0
### --- 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------

## 
golem::fill_desc(
  pkg_name = "WDInequality",  
  pkg_title = "Hoover Index for Wikidata Edits", 
  pkg_description = "PKG_DESC.", 
  author_first_name = "Goran",
  author_last_name = "Milovanovic",
  author_email = "goran.milovanovic_ext@wikimedia.de",
  repo_url = NULL  
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_gpl_license(version = 2, 
                         include_future = TRUE)
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct()


## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

