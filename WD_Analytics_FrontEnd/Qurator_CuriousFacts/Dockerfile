FROM rocker/r-ver:4.0.3
RUN apt-get update && apt-get install -y  git-core libcairo2-dev libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.1")'
RUN Rscript -e 'remotes::install_version("thinkr",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_github("r-lib/gitcreds@fba9ed73ea6fa5efc7ad89bb4f9747b866ad8a6b")'
RUN Rscript -e 'remotes::install_github("r-lib/gh@b524a442b854e9acc45fed0b0f3e5b2666fc8783")'
RUN Rscript -e 'remotes::install_github("r-lib/gert@69be66c86ec7c9bea5e907a72a28b90c976f4e21")'
RUN Rscript -e 'remotes::install_github("r-lib/usethis@6cc7d34082d06acbdb8f483dc8fb1fd77d95f16b")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@0603f0915d7bab71a9f6ce0452030d63bf95efac")'
RUN Rscript -e 'devtools::install_github(repo = "Rdatatable/data.table")'
RUN Rscript -e 'install.packages("shinycssloaders")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("httr")'
RUN Rscript -e 'install.packages("jsonlite")'
RUN Rscript -e 'install.packages("XML")'
RUN Rscript -e 'install.packages("shinycssloaders")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
RUN mkdir /build_zone/data
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');CuriousFacts::run_app()"]