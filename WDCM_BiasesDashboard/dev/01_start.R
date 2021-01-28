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
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
### --- https://wikidata-analytics.wmflabs.org/
### ---
### --- WDCM is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDCM is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDCM If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

## 
golem::fill_desc(
  pkg_name = "WDCMBiasesDashboard", # The Name of the package containing the App 
  pkg_title = "Wikidata Concepts Monitor Biases Dashboard", # The Title of the package containing the App 
  pkg_description = "Gender bias in Wikidata usage and north-south divide statistics and maps.", # The Description of the package containing the App 
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

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()
