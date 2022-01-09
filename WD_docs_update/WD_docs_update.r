#!/usr/bin/env Rscript

# - reporting
print("----------------------------")
print(
  paste0("Time is: ", 
         Sys.time(), 
         ".")
)
print("Begin update!")
print("----------------------------")

# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = TRUE)
fPath <- unlist(strsplit(fPath, split = "/", fixed = TRUE))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")

# - setup
library(rmarkdown)

### --- 1. Wikidata ORES Quality Report

# - render
print("Render R markdown: 1. Wikidata ORES Quality Report.")
# - productionDir: report
prodDir <- '/srv/_Img/WD_Analytics_FrontEnd/WD_docs/docs/'
rmarkdown::render(input =
                    paste0(fPath, "WikidataORESQualityReport.Rmd"), 
                  output_format = 
                    "html_notebook",
                  output_file = 
                    paste0(prodDir, "Wikidata Quality Report.nb.html")
)

# - docker-compose
# - productionComposeDir: docker-compose dir
productionComposeDir <- "/srv/_Img/WD_Analytics_FrontEnd/WD_docs"
print("docker-compose build now.")
system(command = paste0("cd ", 
                        productionComposeDir, 
                        " && ", 
                        "sudo docker-compose build"),
       wait = TRUE)

# - report
print("Done; EXIT.")
print("----------------------------")
