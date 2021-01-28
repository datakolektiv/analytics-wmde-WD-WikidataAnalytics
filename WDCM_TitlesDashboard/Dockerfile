### ---------------------------------------------------------------------------
### --- WDCM (T)itles
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

FROM rocker/r-ver:4.0.3
RUN apt-get update && apt-get install -y  git-core libcairo2-dev libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.1")'
RUN Rscript -e 'remotes::install_version("thinkr",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_github("r-lib/gh@b524a442b854e9acc45fed0b0f3e5b2666fc8783")'
RUN Rscript -e 'remotes::install_github("r-lib/usethis@6cc7d34082d06acbdb8f483dc8fb1fd77d95f16b")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@0603f0915d7bab71a9f6ce0452030d63bf95efac")'
RUN Rscript -e 'remotes::install_version("shinycssloaders", upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_github("rstudio/shinydashboard")'
RUN Rscript -e 'remotes::install_github("tidyverse/dplyr")'
RUN Rscript -e 'remotes::install_github("tidyverse/tidyr")'
RUN Rscript -e 'remotes::install_github("tidyverse/stringr")'
RUN Rscript -e 'remotes::install_github("tidyverse/ggplot2")'
RUN Rscript -e 'remotes::install_github("cran/ggrepel", upgrade="never")'
RUN Rscript -e 'remotes::install_github("r-lib/scales", upgrade="never")'
RUN Rscript -e 'remotes::install_github("datastorm-open/visNetwork", upgrade="never")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.13.2")'
RUN Rscript -e 'remotes::install_github("cran/ape",upgrade="never", version = "1.13.2")'
RUN Rscript -e 'install.packages("igraph")'
RUN Rscript -e 'devtools::install_github("ropensci/plotly")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never", dependencies = F)'
RUN rm -rf /build_zone
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');WDCMTitlesDashboard::run_app()"]
