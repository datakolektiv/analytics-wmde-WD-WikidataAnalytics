#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WDCM EngineBiases
### --- Version 1.0.0
### --- Script: WDCM_EngineBiases.R
### --- June 2020.
### --- Author: Goran S. Milovanovic, Data Analyst, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- WDCM_EngineBiases contacts the WDQS SPARQL end-point
### --- and fetches all Q5 (Human) item IDs
### --- that have Gender (P21) defined. 
### --- WDCM_EngineBiases connects to https://tools.wmflabs.org/wikidata-analysis/
### --- finds the latest WDTK generated update of geo-localized Wikidata items
### --- and downloads the respective wdlabel.json for further processing.
### --- The goal of the analysis is to present the Gender Bias as well as the 
### --- North-South Divide in Wikidata usage.
### --- The remainder of the script searches the Hive goransm.wdcm_maintable
### --- for usage data and prepares the export .tsv files.
### --- NOTE: the execution of this WDCM script is always dependent upon the
### --- previous WDCM_Sqoop_Clients.R run from stat1004 (currently).

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
### --- WDCM is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### --- WDCM is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### --- You should have received a copy of the GNU General Public License
### --- along with WDCM. If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

# - toLog
print(paste0("WDCM Biases updated started at: ", 
             as.character(Sys.time())
             )
      )

### --- Setup
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(WikidataR)
library(ggplot2)
library(scales)
library(bayesAB)
library(ineq)
library(XML)

### --- parameters
### --- Read WDCM paramereters: wdcmConfig.xml
# - toLog
print(paste0("Read WDCM params: ", Sys.time()))
# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = T)
fPath <- unlist(strsplit(fPath, split = "/", fixed = T))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")
params <- xmlParse(paste0(fPath, "wdcmConfig.xml"))
params <- xmlToList(params)
### --- Directories
# - toLog
print(paste0("Set directory params: ", Sys.time()))
# - fPath: where the scripts is run from?
fPath <- params$biases$biases_fPath
# - log paths
logDir <- params$general$logDir
# - hdfs directory
hdfsDir <- params$biases$biases_hdfsDir
# - temporary ETL dir
tempDataDir <- params$biases$biases_tempDataDir
# - published-datasets dir, maps onto
# - https://analytics.wikimedia.org/datasets/wdcm/
pubDataDir <- params$biases$biases_pubDataDir

# - spark2-submit parameters: wdcmConfig_Deployment.xml
paramsDeployment <- xmlParse(paste0(fPath, "wdcmConfig_Deployment.xml"))
paramsDeployment <- xmlToList(paramsDeployment)
# - toLog
print(paste0("Set Spark params: ", Sys.time()))
sparkMaster <- paramsDeployment$biases$spark$biases_master
sparkDeployMode <- paramsDeployment$biases$spark$biases_deploy_mode
sparkNumExecutors <- paramsDeployment$biases$spark$biases_num_executors
sparkDriverMemory <- paramsDeployment$biases$spark$biases_driver_memory
sparkExecutorMemory <- paramsDeployment$biases$spark$biases_executor_memory
sparkConfigDynamic <- paramsDeployment$biases$spark$biases_config

### --- Set proxy
# - toLog
print(paste0("Set proxy params: ", Sys.time()))
Sys.setenv(
  http_proxy = params$general$http_proxy,
  https_proxy = params$general$http_proxy)

# - clear tempDataDir
lF <- list.files(tempDataDir)
if (length(lF) > 0) {
  lapply(paste0(tempDataDir, lF), file.remove)
}

### --- Functions
# - projectType() to determine project type
projectType <- function(projectName) {
  unname(sapply(projectName, function(x) {
    if (grepl("commons", x, fixed = T)) {"Commons"
    } else if (grepl("mediawiki|meta|species|wikidata", x)) {"Other"
    } else if (grepl("wiki$", x)) {"Wikipedia"
    } else if (grepl("quote$", x)) {"Wikiquote"
    } else if (grepl("voyage$", x)) {"Wikivoyage"
    } else if (grepl("news$", x)) {"Wikinews"
    } else if (grepl("source$", x)) {"Wikisource"
    } else if (grepl("wiktionary$", x)) {"Wiktionary"
    } else if (grepl("versity$", x)) {"Wikiversity"
    } else if (grepl("books$", x)) {"Wikibooks"
    } else {"Other"}
  }))
}

### --- Constants
gender <- list()
gender$male <- 'Q6581097'
gender$female <- 'Q6581072'
gender$intersex <- 'Q1097630'
gender$transF <- 'Q1052281'
gender$transM <- 'Q2449503'

### ---------------------------------------------------------------------------
### --- Apache Spark ETL 
### ---------------------------------------------------------------------------
# - toLog
print(paste0("Run Apache Spark ETL: ", Sys.time()))
# - Kerberos init
system(command = 
         'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
       wait = T)
# -  delete hdfsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rm -r ',
           hdfsDir),
       wait = T)
# -  make hdfsDir
system(command = 
         paste0(
           'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -mkdir ',
           hdfsDir),
       wait = T)
# - run Pyspark
system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                        sparkMaster, ' ',
                        sparkDeployMode, ' ', 
                        sparkNumExecutors, ' ',
                        sparkDriverMemory, ' ',
                        sparkExecutorMemory, ' ',
                        sparkConfigDynamic, ' ',
                        paste0(fPath, 'wdcmModule_Biases_ETL.py')
                        ),
       wait = T)
print(paste0("Run Apache Spark ETL (DONE): ", Sys.time()))

### ---------------------------------------------------------------------------
### --- Load Spark ETL results
### ---------------------------------------------------------------------------
# - toLog
print(paste0("Read Apache Spark ETL data: ", Sys.time()))
# - copy splits from hdfs to local dataDir
# - from statements:
system(paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls ',
  paste0(hdfsDir, 'WDCM_Biases_ETL'), ' > ', tempDataDir, 'files.txt'),
  wait = T)
files <- read.table(
  paste0(tempDataDir, 'files.txt'), 
  skip = 1)
files <- as.character(files$V8)[2:length(files$V8)]
file.remove(paste0(tempDataDir, 'files.txt'))
for (i in 1:length(files)) {
  system(paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -text ',
    files[i], ' > ',
    paste0(tempDataDir, "wdcm_biases_etl_", i, ".csv")), wait = T)
}
lF <- list.files(tempDataDir)
dataSet <- lapply(paste0(tempDataDir, lF), fread)
dataSet <- rbindlist(dataSet)
colnames(dataSet) <- c('item', 'placeOfBirth', 'gender',
                       'occupation', 'lat', 'lon', 'project', 'usage')
# - remove temporary files
lapply(paste0(tempDataDir, lF), file.remove)

### ---------------------------------------------------------------------------
### --- Prepare Data Sets 
### ---------------------------------------------------------------------------
# - toLog
print(paste0("Prepare data sets: ", Sys.time()))
genderConsidered <- unlist(gender)
dataSet <- dplyr::filter(dataSet, 
                         gender %in% genderConsidered)
items <- dataSet %>% 
  dplyr::select(item, gender)
items <- items[!duplicated(items), ]
wikidataGender <- as.data.frame(table(items$gender))

### -----------------------------------
### --- indicators
### -----------------------------------
nMaleItems <- wikidataGender$Freq[which(wikidataGender$Var1 == gender$male)]
nFemaleItems <- wikidataGender$Freq[which(wikidataGender$Var1 == gender$female)]
nIntersexItems <- wikidataGender$Freq[which(wikidataGender$Var1 == gender$intersex)]
nTransFItems <- wikidataGender$Freq[which(wikidataGender$Var1 == gender$transF)]
nTransMItems <- wikidataGender$Freq[which(wikidataGender$Var1 == gender$transM)]

### -----------------------------------
### --- Gender x Projects
### -----------------------------------
# - toLog
print(paste0("Gender x Projects: ", Sys.time()))
genderProjectData <- dataSet %>% 
  dplyr::select(item, gender, project, usage)
genderProjectData <- genderProjectData[!duplicated(genderProjectData), ]
genderProjectData <- genderProjectData %>% 
  dplyr::select(gender, project, usage)
w1 <- which(genderProjectData$gender == gender$male)
w2 <- which(genderProjectData$gender == gender$female)
w <- unique(c(w1, w2))
genderProjectData <- genderProjectData[w, ]
genderProjectData <- genderProjectData %>% 
  dplyr::group_by(project, gender) %>% 
  dplyr::summarise(usage = sum(usage)) %>% 
  dplyr::filter(!is.na(usage) | project != "")
genderProjectData <- tidyr::spread(genderProjectData, 
                                   key = gender,
                                   value = usage, 
                                   fill = 0)
colnames(genderProjectData) <- c('project', 'usageF', 'usageM')
genderProjectData$propF <- 
  genderProjectData$usageF/(genderProjectData$usageF + genderProjectData$usageM)
genderProjectData$propM <- 
  genderProjectData$usageM/(genderProjectData$usageF + genderProjectData$usageM)
genderProjectData$percentF <- genderProjectData$propF * 100 
genderProjectData$percentM <- genderProjectData$propM * 100
genderProjectData$projectType <- projectType(genderProjectData$project)
# - store
write.csv(genderProjectData, 
          paste0(tempDataDir, 'genderProjectDataSet.csv'))

### -----------------------------------
### --- Gender x Occupation
### -----------------------------------
# - toLog
print(paste0("Gender x Occupation: ", Sys.time()))
genderOccupationData <- dataSet %>% 
  dplyr::select(gender, occupation) %>% 
  dplyr::filter(occupation != "")
w1 <- which(genderOccupationData$gender == gender$male)
w2 <- which(genderOccupationData$gender == gender$female)
w <- unique(c(w1, w2))
genderOccupationData <- 
  genderOccupationData[w, ]
genderOccupationData <- genderOccupationData %>% 
  dplyr::group_by(occupation, gender) %>% 
  dplyr::summarise(usage = n())
genderOccupationData <- tidyr::spread(genderOccupationData,
                                      key = gender,
                                      value = usage,
                                      fill = 0)
colnames(genderOccupationData) <- c('occupation', 'usageF', 'usageM')
genderOccupationData$totalUsage <- 
  genderOccupationData$usageF + genderOccupationData$usageM
genderOccupationData <- dplyr::arrange(genderOccupationData, desc(totalUsage))
genderOccupationData <- genderOccupationData[1:1000, ]
# - fetch occupation labels w. {WikidataR}
labels <- sapply(genderOccupationData$occupation,
                 function(x) {
                   repeat {
                     i <- tryCatch({
                       get_item(x)
                     },
                     error = function(condition) {
                       Sys.sleep(2)
                       FALSE
                     })
                     if (class(i) == "wikidata") {
                       break
                     }
                   }
                   i[[1]]$labels$en$value
                 })
labNames <- names(labels)
labels <- as.character(labels)
genderOccupationData$label <- labels
# - store
write.csv(genderOccupationData, 
          paste0(tempDataDir, 'occUsage.csv'))
rm(genderOccupationData); rm(labNames); rm(labels)

### ---------------------------------------------------------------------------
### --- Analytics/Outputs
### ---------------------------------------------------------------------------
# - toLog
print(paste0("Analytics/Outputs: ", Sys.time()))
### --- Gender vs Geography Tab
print(paste0("Gender vs Geography Tab: ", Sys.time()))
geoItems <- dataSet %>% 
  dplyr::select(item, gender, project, usage, lat, lon)
geoItems <- geoItems[!duplicated(geoItems), ]
w1 <- which(geoItems$gender == gender$male)
w2 <- which(geoItems$gender == gender$female)
w <- unique(c(w1, w2))
geoItems <- geoItems[w, ]

# - geo-localized data for: gender == male & gender == female
geoItems <- as.data.frame(geoItems)
geo_M <- geoItems[geoItems$gender %in% gender$male, ]
geo_F <- geoItems[geoItems$gender %in% gender$female, ]
# - sum: usage
geo_M <- geo_M %>% 
  dplyr::select(item, lat, lon, usage) %>% 
  dplyr::group_by(item, lat, lon) %>% 
  dplyr::summarise(usage = sum(usage))
geo_M <- as.data.frame(geo_M)
geo_M <- geo_M[complete.cases(geo_M), ]
geo_F <- geo_F %>% 
  dplyr::select(item, lat, lon, usage) %>% 
  dplyr::group_by(item, lat, lon) %>% 
  dplyr::summarise(usage = sum(usage))
geo_F <- as.data.frame(geo_F)
geo_F <- geo_M[complete.cases(geo_F), ]

# - {ggplot2} for M items
# - toLog
print(paste0("M_Items_Distribution.png: ", Sys.time()))
filename <- 'M_Items_Distribution.png'
png(filename = paste0(tempDataDir, filename),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot(geo_M, aes(x = lon,
                  y = lat)) +
  geom_point(size = geo_M$usage/max(geo_M$usage)*5,
             alpha = log(geo_M$usage)/max(log(geo_M$usage)),
             color = "cadetblue3") +
  xlim(-180, 180) + ylim(-90, 90) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.background = element_rect(color = "black", fill = "black")) +
  theme(panel.border = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.position = "bottom")
dev.off()

# - {ggplot2} for F items
# - toLog
print(paste0("F_Items_Distribution.png: ", Sys.time()))
filename <- 'F_Items_Distribution.png'
png(filename = paste0(tempDataDir, filename),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot(geo_F, aes(x = lon,
                  y = lat)) +
  geom_point(size = geo_F$usage/max(geo_F$usage)*5,
             alpha = log(geo_F$usage)/max(log(geo_F$usage)),
             color = "green") +
  xlim(-180, 180) + ylim(-90, 90) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.background = element_rect(color = "black", fill = "black")) +
  theme(panel.border = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.position = "bottom")
dev.off()

### -----------------------------------
### --- indicators
### -----------------------------------
geoItems <- geoItems[complete.cases(geoItems), ]
globalGenderProportion_M <- 
  sum(geoItems$usage[geoItems$gender == gender$male])
globalGenderProportion_F <- 
  sum(geoItems$usage[geoItems$gender == gender$female])
globalGenderProportion_N <- 
  sum(geoItems$usage[geoItems$lat > 0])
globalGenderProportion_S <- 
  sum(geoItems$usage[geoItems$lat < 0])
genderPropotion_M_N <- 
  sum(geoItems$usage[geoItems$gender == gender$male & geoItems$lat > 0])
genderPropotion_M_S <- 
  sum(geoItems$usage[geoItems$gender == gender$male & geoItems$lat < 0])
genderPropotion_F_N <- 
  sum(geoItems$usage[geoItems$gender == gender$female & geoItems$lat > 0])
genderPropotion_F_S <- 
  sum(geoItems$usage[geoItems$gender == gender$female & geoItems$lat < 0])
rm(geoItems)

### --- Gender by Project Tab
# - toLog
print(paste0("Gender by Project Tab: ", Sys.time()))
genderProject <- fread(paste0(tempDataDir, 'genderProjectDataSet.csv'), 
                       header = T)
genderProject$V1 <- NULL
genderProject <- genderProject[, c(
  'project', 'usageM', 'usageF', 'propM', 'propF', 
  'percentM', 'percentF', 'projectType')]
# - Bayesian Binomial test w. Beta(1,1)
# - toLog
print(paste0("Bayesian tests: ", Sys.time()))
BBT <- apply(genderProject[, 2:3], 1, function(x) {
  if (x[1] == 0 | x[2] == 0) {
    data.frame(pMF = NA,
               CI5 = NA,
               CI95 = NA)
  } else
  {
    mData <- rep(0, x[1] + x[2])
    mOnes <- sample(1:length(mData), x[1])
    mData[mOnes] <- 1
    fData <- rep(0, x[1] + x[2])
    fOnes <- sample(1:length(fData), size = x[2])
    fData[fOnes] <- 1
    AB1 <- summary(bayesTest(mData, fData,
                             priors = c('alpha' = 1, 'beta' = 1),
                             distribution = 'bernoulli',
                             n_samples = 1e6))
    data.frame(pMF = unlist(AB1$probability),
               CI5 = AB1$interval$Probability[1],
               CI95 = AB1$interval$Probability[2])
  }
})
BBT <- rbindlist(BBT)
genderProject <- cbind(genderProject, BBT)
write.csv(genderProject, 
          paste0(tempDataDir, 'genderProjectDataSet.csv'))

# - MF proportion per project table
# - toLog
print(paste0("mfPropProject.csv table: ", Sys.time()))
mfPropProject <- genderProject %>%
  select(usageM, usageF, projectType) %>%
  group_by(projectType) %>%
  summarise(usageM = sum(usageM), usageF = sum(usageF))
# - add proportions and percents
mfPropProject$propM <- 
  mfPropProject$usageM/(mfPropProject$usageM + mfPropProject$usageF)
mfPropProject$propF <- 
  mfPropProject$usageF/(mfPropProject$usageM + mfPropProject$usageF)
mfPropProject$percentM <- 
  round(mfPropProject$usageM/(mfPropProject$usageM + mfPropProject$usageF)*100, 2)
mfPropProject$percentF <- 
  round(mfPropProject$usageF/(mfPropProject$usageM + mfPropProject$usageF)*100, 2)
# - Bayesian Binomial test w. Beta(1,1)
# - toLog
print(paste0("Bayesian tests: ", Sys.time()))
BBT <- apply(mfPropProject[, 2:3], 1, function(x) {
  if (x[1] == 0 | x[2] == 0) {
    data.frame(pMF = NA,
               CI5 = NA,
               CI95 = NA)
  } else
  {
    mData <- rep(0, x[1] + x[2])
    mOnes <- sample(1:length(mData), x[1])
    mData[mOnes] <- 1
    fData <- rep(0, x[1] + x[2])
    fOnes <- sample(1:length(fData), size = x[2])
    fData[fOnes] <- 1
    AB1 <- summary(bayesTest(mData, fData,
                             priors = c('alpha' = 1, 'beta' = 1),
                             distribution = 'bernoulli',
                             n_samples = 1e6))
    data.frame(pMF = unlist(AB1$probability),
               CI5 = AB1$interval$Probability[1],
               CI95 = AB1$interval$Probability[2])
  }
})
BBT <- rbindlist(BBT)
mfPropProject <- cbind(mfPropProject, BBT)
write.csv(mfPropProject, 
          paste0(tempDataDir, 'mfPropProject.csv'))


# - global MF distribution
# - toLog
print(paste0("global MF distribution: ", Sys.time()))
genItems <- dataSet %>% 
  dplyr::select(item, gender, project, usage)
genItems <- as.data.frame(genItems)
genItems <- genItems[!duplicated(genItems), ]
fGenItems <- genItems[genItems$gender == gender$female, ]
mGenItems <- genItems[genItems$gender == gender$male, ]
# - sum: usage
mGenItems <- mGenItems %>% 
  dplyr::select(item, usage) %>% 
  dplyr::group_by(item) %>% 
  dplyr::summarise(usage = sum(usage))
mGenItems <- mGenItems[complete.cases(mGenItems), ]
mGenItems$gender <- 'M'
fGenItems <- fGenItems %>% 
  dplyr::select(item, usage) %>% 
  dplyr::group_by(item) %>% 
  dplyr::summarise(usage = sum(usage))
fGenItems <- fGenItems[complete.cases(fGenItems), ]
fGenItems$gender <- 'F'
# - combine M and F data sets
fGenItems <- arrange(fGenItems, desc(usage))
fGenItems$rank <- 1:dim(fGenItems)[1]
mGenItems <- arrange(mGenItems, desc(usage))
mGenItems$rank <- 1:dim(mGenItems)[1]
genItems <- rbind(fGenItems, mGenItems)
rm(mGenItems); rm(fGenItems); gc()

### -----------------------
### --- indicators
### -----------------------
totalUsage_M <- sum(genItems$usage[genItems$gender == 'M'])
totalUsage_F <- sum(genItems$usage[genItems$gender == 'F'])

### -----------------------
### --- Charts
### -----------------------
# - toLog
print(paste0("Charts: ", Sys.time()))
# - {ggplot2} M and F usage distributions
filename <- 'genderUsage_Distribution.png'
png(filename = paste0(tempDataDir, filename),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot(genItems, aes(x = rank,
                   y = log(usage),
                   fill = gender,
                   color = gender,
                   group = gender)) +
  geom_line(size = .25) +
  scale_color_manual(values = c('green', 'cadetblue3')) +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  ylab("log(Wikidata Usage)") + xlab("Rank") +
  labs(y = "log(Wikidata Usage)",
       x = "Item Usage Rank",
       title = "Wikidata Usage: Items per Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust = 1)) +
  theme(axis.text.y = element_text(size = 10, hjust = 1)) +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11)) +
  theme(plot.title = element_text(size = 12))
dev.off()

# - {ggplot2} M and F usage distributions
filename <- 'genderUsage_Distribution_jitterp.png'
png(filename = paste0(tempDataDir, filename),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot(genItems, aes(x = gender,
                     y = log(usage),
                     fill = gender,
                     color = gender,
                     group = gender)) +
  geom_jitter(aes(alpha = usage), size = .25, width = .1) +
  scale_color_manual(values = c('lightgreen', 'cadetblue3')) +
  scale_alpha(guide = 'none') +
  theme_minimal() +
  ylab("Wikidata Usage") + xlab("gender") +
  labs(y = "log(Wikidata Usage)",
       x = "Item Usage Rank",
       title = "Wikidata Usage: Items per Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, hjust = 1)) +
  theme(axis.text.y = element_text(size = 10, hjust = 1)) +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11)) +
  theme(plot.title = element_text(size = 12))
dev.off()

### -----------------------
### --- Diversity
### -----------------------
# - toLog
print(paste0("Diversity: ", Sys.time()))
### --- Gini coefficient and Lorentz curve 
### --- for M and F items Wikidata usage
fItems <- genItems %>%
  filter(gender == 'F')
mItems <- genItems %>%
  filter(gender == 'M')

### --- Gini
giniF <- round(ineq(fItems$usage, type = "Gini"), 2)
giniM <- round(ineq(mItems$usage, type = "Gini"), 2)

### --- Lorentz
# - toLog
print(paste0("Lorentz curve: ", Sys.time()))
fLor <- Lc(fItems$usage)
mLor <- Lc(mItems$usage)
pFrame <- data.frame(p = c(fLor$p, mLor$p),
                     L = c(fLor$L, mLor$L),
                     gender = c(rep('F', length(fLor$p)), 
                                rep('M', length(mLor$p)))
                     )
mfSample <- sample(1:length(pFrame$gender), 10000)
pFrame <- pFrame[mfSample, ]
add01 <- data.frame(p = c(0,1, 0, 1),
                    L = c(0,1, 0, 1),
                    gender = c('F', 'F', 'M', 'M'))
pFrame <- rbind(pFrame, add01)
# - {ggplot2} Lorenz Curves M and F usage
filename <- 'Gender_LorenzCurves.png'
png(filename = paste0(tempDataDir, filename),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot(pFrame, aes(x = p, y = L, color = gender)) +
  geom_segment(x = 0, y = 0, 
               xend = 1, yend = 1, 
               size = .02, 
               color = "black", 
               linetype = "dotted") +
  geom_line(size = 1) +
  geom_segment(x = 0, y = 0, xend = 1, 
               yend = 1, size = .1, 
               color = "black", 
               linetype = "dashed") +
  scale_color_manual(values = c("green", "cadetblue3")) +
  ggtitle(paste0("Wikidata Usage Lorenz Curves\n",
                 "Gini(F) = ", giniF, ", Gini(M) = ", giniM)) +
  xlab("Proportion of Items") + ylab("Proportion of Wikidata Usage") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_text(face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 13))
dev.off()

### -----------------------
### --- Occupation Tab
### -----------------------
# - toLog
print(paste0("Occupation Tab: ", Sys.time()))
### --- Occupation Analytics
# - loadM occupations and usage
occUsage <- read.csv(paste0(tempDataDir, 'occUsage.csv'), 
                     header = T, 
                     check.names = F,
                     stringsAsFactors = F, 
                     row.names = 1)
occUsage <- arrange(occUsage, desc(totalUsage))
# - re-arrange occUsage
occUsage <- data.frame(occupation = occUsage$occupation,
                       usageM = occUsage$usageM,
                       usageF = occUsage$usageF,
                       totalUsage = occUsage$totalUsage,
                       label = occUsage$label,
                       stringsAsFactors = F)
# - Bayesian Binomial test w. Beta(1,1)
# - toLog
print(paste0("Bayes tests: ", Sys.time()))
BBT <- apply(occUsage[, 2:3], 1, function(x) {
  if (x[1] == 0 | x[2] == 0) {
    data.frame(pMF = NA,
               CI5 = NA,
               CI95 = NA)
  } else
  {
    mData <- rep(0, x[1] + x[2])
    mOnes <- sample(1:length(mData), x[1])
    mData[mOnes] <- 1
    fData <- rep(0, x[1] + x[2])
    fOnes <- sample(1:length(fData), size = x[2])
    fData[fOnes] <- 1
    AB1 <- summary(bayesTest(mData, fData,
                             priors = c('alpha' = 1, 'beta' = 1),
                             distribution = 'bernoulli',
                             n_samples = 1e6))
    data.frame(pMF = unlist(AB1$probability),
               CI5 = AB1$interval$Probability[1],
               CI95 = AB1$interval$Probability[2])
  }
})
BBT <- rbindlist(BBT)
occUsage <- cbind(occUsage, BBT)
# - re-arrange occUsage
occUsage <- data.frame(occupation = occUsage$occupation,
                       label = occUsage$label,
                       usageM = occUsage$usageM,
                       usageF = occUsage$usageF,
                       totalUsage = occUsage$totalUsage,
                       pMF = occUsage$pMF,
                       CI5 = occUsage$CI5,
                       CI95 = occUsage$CI95,
                       stringsAsFactors = F)
write.csv(occUsage, 
          paste0(tempDataDir, 'occUsage.csv'))

### -----------------------------------
### --- write global indicators
### -----------------------------------
# - toLog
print(paste0("Write global indicators: ", Sys.time()))
globalIndicators <- data.frame(nMaleItems, nFemaleItems, 
                               nIntersexItems, nTransMItems, 
                               nTransFItems,
                               totalUsage_M, totalUsage_F, 
                               globalGenderProportion_M, globalGenderProportion_F,
                               globalGenderProportion_N, globalGenderProportion_S,
                               genderPropotion_M_N, genderPropotion_M_S,
                               genderPropotion_F_N, genderPropotion_F_S, giniM, giniF)
write.csv(globalIndicators, 
          paste0(tempDataDir, "globalIndicators.csv"))

### -----------------------------------
### --- copy to pubDataDir
### -----------------------------------
# - toLog
print(paste0("copy to pubDataDir: ", Sys.time()))
# - archive:
lF <- list.files(tempDataDir)
lapply(lF, function(x) {
  system(command = 
           paste0('cp ', tempDataDir, x, ' ', pubDataDir),
         wait = T)
})

### -----------------------------------
### --- final log
### -----------------------------------
### --------------------------------------------------
### --- copy and clean up log files:
### --------------------------------------------------
# - copy the main log file to published for timestamp
# - toRuntime log:
print("Copy main log to published; clean up log.")
# - archive:
lF <- list.files(logDir)
lF <- lF[grepl('WDCM_EngineBiasesRuntimeLog', lF)]
lapply(lF, function(x) {
  system(command = 
           paste0('cp ', logDir, x, ' ', logDir, 'archive/'),
         wait = T)
})
# - clean up
file.remove(paste0(logDir, lF))
# - conclusion
print("DONE. Exiting.")

# - toLog
print(paste0("WDCM Biases updated ended at: ", Sys.time()))


