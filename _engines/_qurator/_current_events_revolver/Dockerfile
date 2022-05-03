### ---------------------------------------------------------------------------
### --- Project: QURATOR Current Events (QCE)
### --- Version 1.0.0
### --- Script: currentEvents_Revolver.R
### --- September 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: Continuous Wikibase API contact for QCE
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get install -y  git-core libcairo2-dev libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("data.table")'
RUN Rscript -e 'install.packages("httr")'
RUN Rscript -e 'install.packages("jsonlite")'
RUN Rscript -e 'install.packages("magrittr")'

RUN mkdir /app
RUN mkdir /app/data
COPY currenteventsrevolver.R /app
WORKDIR /app

CMD ["Rscript", "currenteventsrevolver.R"]
