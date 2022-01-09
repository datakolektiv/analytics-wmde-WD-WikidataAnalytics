#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Wikidata Quality Report v1.0.0
### --- Script: WD_ORES_ItemQuality.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE, 2021/12.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

# - to runtime Log:
print(paste("--- WD_ORES_ItemQuality.R RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

### --- Read WDOQR paramereters
# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = TRUE)
fPath <- unlist(strsplit(fPath, split = "/", fixed = TRUE))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")

# - renv
renv::load(project = fPath, quiet = FALSE)

# - lib
library(WMDEData)

# - pars
params <- XML::xmlParse(paste0(fPath, "wd_ORESQualityReport_Config.xml"))
params <- XML::xmlToList(params)

### --- Directory Tree
dataDir <- paste0(fPath, '_data/')
analyticsDir <- paste0(fPath, '_analytics/')
logDir <- paste0(fPath, '_log/')
hdfsPath <- params$general$hdfsPath
oresOutputPath <- params$general$ores_output_path
publicDir <- params$general$publicDir
oresSourceDir <- params$general$ores_source_dir

### --- Set proxy
WMDEData::set_proxy(http_proxy = "http://webproxy.eqiad.wmnet:8080",
                    https_proxy = "http://webproxy.eqiad.wmnet:8080")

### ------------------------------------------------------------
### --- Section 1. Update
### ------------------------------------------------------------

# - to runtime Log:
print(paste("--- Update ORES scores STARTED ON:", 
            Sys.time(), sep = " "))

### --- NOTE on datasets:
# - the latest ORES score revions must be found under:
# - oresSourceDir (on stat1005)
# - and then transfered to: dataDir (on stat1005)
lF <- list.files(oresSourceDir)
lF <- lF[grepl("tsv$", lF)]
# - determine latest ORES scores snapshot
ores_snaps <- stringr::str_extract(lF, "[[:digit:]]+")
ores_snaps <- sort(ores_snaps)
ores_snaps <- tail(ores_snaps, 1)
wlF <- which(grepl(ores_snaps, lF))
lF <- lF[wlF]
# - clean up dataDir from previous updates
system(command = paste0("rm -r ",
                        dataDir,
                        "*"),
       wait = TRUE)
# - copy latest ORES scores snapshot to dataDir
system(command = paste0("cp ",
                        paste0(oresSourceDir, lF),
                        " ",
                        dataDir),
       wait = TRUE)

### ------------------------------------------------------------
### --- Section 1. Keep only latest ORES revisions + to hdfs
### ------------------------------------------------------------

# - to runtime Log:
print(paste("--- Filter ORES scores and copy to hdfs STARTED ON:", 
            Sys.time(), sep = " "))

# - read raw ORES data
oresScoresUpdate <- data.table::fread(
  paste0(dataDir, lF), 
  sep = "\t", header = F)
oresScoresUpdate <- dplyr::select(oresScoresUpdate, 
                                  V2, V4, V5)
colnames(oresScoresUpdate) <- c('title', 'timestamp', 'prediction')
oresScoresUpdate <- oresScoresUpdate[order(title, -timestamp)]
w <- which(duplicated(oresScoresUpdate$title))
if (length(w) > 0) {
  oresScoresUpdate <- oresScoresUpdate[-w, ]
}

# - save updated ORES predictions
write.csv(oresScoresUpdate, 
          paste0(dataDir, "ORESpredictions.csv"))
rm(oresScoresUpdate); gc()

### --- copy ORES predictions to hdfs
WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata",
                       localPath = dataDir,
                       localFilename = 'ORESpredictions.csv',
                       hdfsDir = paste0(hdfsPath, 'ORESPredictions')
)


oresScoresPath <- paste0(dataDir, 'ORESpredictions.csv')
command <- paste0(
  'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
  oresScoresPath,
  ' ',
  hdfsPath,
  'ORESPredictions')
system(command, wait = T)

### ------------------------------------------------------------
### --- Section 2. Apache Spark ETL
### ------------------------------------------------------------

# - to runtime Log:
print(paste("--- Run Pyspark ETL STARTED ON:", 
            Sys.time(), sep = " "))

# - config deploy pars
paramsDeployment <- XML::xmlParse(
  paste0(fPath, "wd_ORESQualityReport_Config_Deploy.xml")
  )
paramsDeployment <- XML::xmlToList(paramsDeployment)
# - spark2-submit parameters:
sparkMaster <- paramsDeployment$spark$master
sparkDeployMode <- paramsDeployment$spark$deploy_mode
sparkNumExecutors <- paramsDeployment$spark$num_executors
sparkDriverMemory <- paramsDeployment$spark$driver_memory
sparkExecutorMemory <- paramsDeployment$spark$executor_memory
sparkConfigDynamic <- paramsDeployment$spark$config

# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Run Spark ETL
WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                            pysparkPath = paste0(fPath, "WD_ORES_ItemQuality.py"),
                            sparkMaster = sparkMaster,
                            sparkDeployMode = sparkDeployMode,
                            sparkNumExecutors = sparkNumExecutors,
                            sparkDriverMemory = sparkDriverMemory,
                            sparkExecutorMemory = sparkExecutorMemory,
                            sparkConfigDynamic = sparkConfigDynamic)

### ------------------------------------------------------------
### --- Analytics
### ------------------------------------------------------------

# - to runtime Log:
print(paste("--- Analystics STARTED ON:", 
            Sys.time(), sep = " "))

### --- Compose final usage dataset
# - to runtime Log:
print(paste("--- Collect Final Data Set STARTED ON:", 
            Sys.time(), sep = " "))

dataSet <- WMDEData::hdfs_read_from(kerberosUser = 
                                      "analytics-privatedata",
                                    localPath = dataDir,
                                    localFilenamePrefix = 
                                      "wdORESQuality_Reuse_",
                                    hdfsDir = oresOutputPath,
                                    hdfsFilenamePrefix = "",
                                    fr_header = FALSE)
# - schema
colnames(dataSet) <- c('item', 'timestamp', 'usage', 'score')

# - clean up a bit
print(paste0("Dimension of everything: ", 
             paste0(as.character(dim(dataSet)), collapse = ", ")))
# - keep items only
w <- which(!grepl("^Q[[:digit:]]+", dataSet$item))
if (length(w) > 0) {
  dataSet <- dataSet[-w, ]
}
print(paste0("Dimension of items only: ", 
             paste0(as.character(dim(dataSet)), collapse = ", ")))
# - save fundamental dataset
write.csv(dataSet, 
          paste0(analyticsDir, "fundamentalORES_Items.csv"))

# - collect statistics
stats <- list()
stats$total_n_items <- length(dataSet$item)
stats$total_n_items_used <- sum(!is.na(dataSet$usage))

### --- Aggregates
oresScoresDistribution <- dataSet[, .N ,by = score]
colnames(oresScoresDistribution) <- c('score', 'frequency')
oresScoresDistribution <- oresScoresDistribution %>% 
  dplyr::filter(score %in% c('A', 'B', 'C', 'D', 'E'))
write.csv(oresScoresDistribution, 
          paste0(analyticsDir, "dataQuality_oresScoresDistribution.csv"))

# - collect statistics
stats$total_n_items_with_ORES_prediction <- 
  sum(oresScoresDistribution$frequency)

# - timestamp for statistics:
stats$ORES_timestamp <- ores_snaps
wdcm_report <- 
  read.csv(
    "https://analytics.wikimedia.org/published/datasets/wmde-analytics-engineering/wdcm/WDCM_MainReport.csv",
    header = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE)
wdcm_timestamp <- tail(wdcm_report$Time, 1)
wdcm_timestamp <- substr(wdcm_timestamp, 1, 10)
stats$WDCM_timestamp <- wdcm_timestamp

# - save statistics
write.csv(as.data.frame(stats), 
          paste0(analyticsDir, "dataQuality_Stats.csv"))

### --- Aggregates and Plots

# - to runtime Log:
print(paste("--- Aggregates and Plots RUN STARTED ON:", 
            Sys.time(), sep = " "))

# - ORES score vs Usage statistics
scoreUsage <- dplyr::select(dataSet, 
                            score, usage)
scoreUsage <- scoreUsage[!(score == '' | is.na(score) | is.na(usage)), ]
scoreUsage <- scoreUsage[, .(meanUsage = mean(usage),
                             medianUsage = median(usage),
                             maxUsage = max(usage),
                             minUsage = min(usage)), 
                         by = score]
scoreUsage <- dplyr::arrange(scoreUsage, score)
write.csv(scoreUsage, 
          paste0(analyticsDir, "dataQuality_scoreUsage.csv"))

# - 1,000 most used items from each ORES score category
mostUsedItemsCategory <- dataSet %>%
  dplyr::select(item, score, usage)
mostUsedItemsCategory <- 
  mostUsedItemsCategory[!(score == '' | is.na(score) | is.na(usage)), ]
mostUsedItemsCategory <- 
  mostUsedItemsCategory[order(score, -usage), head(.SD, 1000), by = score]
write.csv(mostUsedItemsCategory, 
          paste0(analyticsDir, "dataQuality_mostUsedItemsCategory.csv"))

# - top 10,000 items per WDCM usage and their ORES score statistics
top10t <- dataSet %>%
  dplyr::select(item, score, usage)
top10t <- top10t[!(score == '' | is.na(score) | is.na(usage)), ]
top10t <- top10t[order(-usage)][1:10000]
write.csv(top10t,
          paste0(analyticsDir, "dataQuality_mostUsedItemsCategory_10000.csv"))
oresScoresDistribution10t <- as.data.frame(table(top10t$score))
colnames(oresScoresDistribution10t) <- c('score', 'frequency')
write.csv(oresScoresDistribution10t, 
          paste0(analyticsDir, "dataQuality_oresScoresDistribution_10000.csv"))

# - top 10,000 items per WDCM usage and their summary ORES score statistics
scoreUsage_top10t <- top10t %>% 
  dplyr::select(score, usage) %>% 
  dplyr::filter(!is.na(score)) %>% 
  dplyr::filter(score %in% c('A', 'B', 'C', 'D', 'E')) %>% 
  dplyr::group_by(score) %>% 
  dplyr::summarise(meanUsage = mean(usage, na.rm = T),
                   medianUsage = median(usage, na.rm = T),
                   maxUsage = max(usage, na.rm = T), 
                   minUsage = min(usage, na.rm = T))
write.csv(scoreUsage_top10t, 
          paste0(analyticsDir, "dataQuality_scoreUsage_10000.csv"))

# - visualizations: ORES score vs WDCM usage statistics (boxplots)
# - NOTE: +/- 1.5 * IQR are the limits of the whiskers for {ggplot2}
# - Plot A: outliers included
scoreUsage_BoxPlot <- dataSet %>% 
  dplyr::select(usage, score)
scoreUsage_BoxPlot <- 
  scoreUsage_BoxPlot[!(score == '' | is.na(score) | is.na(usage)), ]
write.csv(scoreUsage_BoxPlot, 
          paste0(analyticsDir, "scoreUsage_BoxPlot.csv"))
png(filename = paste0(analyticsDir, 'scoreUsage_BoxPlot_ggplot2.png'),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot2::ggplot(scoreUsage_BoxPlot,
                ggplot2::aes(x = score,
                             y = log(usage),
                             fill = score)) +
  ggplot2::geom_boxplot(outlier.colour = "black",
                        outlier.shape = 1,
                        outlier.size = 2) + 
  ggplot2::xlab("ORES Quality Prediction") + 
  ggplot2::ylab("WDCM Item Re-use Statistic\n(log scale)") +
  ggplot2::scale_fill_brewer(palette="Dark2") + 
  ggplot2::ggtitle("Item Quality vs Item Re-use\n(Outliers included)") + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "aliceblue"))
dev.off()

# - Plot B: outliers removed
iqrs <- scoreUsage_BoxPlot[, .(lower = quantile(usage, .25) - IQR(usage)*1.5, 
                               upper = quantile(usage, .75) + IQR(usage)*1.5), 
                           by = score]
scoreUsage_BoxPlot <- dplyr::left_join(scoreUsage_BoxPlot, 
                                       iqrs, 
                                       by = "score")
scoreUsage_BoxPlot <- data.table::as.data.table(scoreUsage_BoxPlot)
# - collect outliers per ORES score category
scoreUsageOutliers <- scoreUsage_BoxPlot[usage < lower | usage > upper]
scoreUsageOutliers <- scoreUsageOutliers %>% 
  dplyr::select(score, usage)
scoreUsageOutliers <- 
  scoreUsageOutliers[order(score, -usage), head(.SD, 1000), by = score]
write.csv(scoreUsageOutliers, 
          paste0(analyticsDir, "scoreUsageOutliers_1000.csv"))
# - filter scoreUsage_BoxPlot from outliers per ORES score category
scoreUsage_BoxPlot <- scoreUsage_BoxPlot[usage >= lower & usage <= upper]
scoreUsage_BoxPlot <- scoreUsage_BoxPlot %>% 
  dplyr::select(score, usage)
write.csv(scoreUsage_BoxPlot, 
          paste0(analyticsDir, "scoreUsage_BoxPlot_outliers_removed.csv"))
png(filename = paste0(analyticsDir, 'scoreUsage_BoxPlot_ggplot2_outliers_removed.png'),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot2::ggplot(scoreUsage_BoxPlot,
                ggplot2::aes(x = score,
                             y = log(usage),
                             fill = score)) +
  ggplot2::geom_boxplot(outlier.shape = NA) + 
  ggplot2::xlab("ORES Quality Prediction") + 
  ggplot2::ylab("WDCM Item Re-use Statistic\n(log scale)") +
  ggplot2::scale_fill_brewer(palette="Dark2") + 
  ggplot2::ggtitle("Item Quality vs Item Re-use\n(Outliers removed)") + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "aliceblue"))
dev.off()

# - Plot C: data points w. outliers removed 
scoresDistribution <- table(scoreUsage_BoxPlot$score)
scoresDistribution <- round(scoresDistribution/sum(scoresDistribution)*100, 2)
scoresDistribution <- paste0(names(scoresDistribution), " (", scoresDistribution, "%)")
scoresDistribution <- paste(scoresDistribution, collapse = ", ")
# plot only unique usage values:
scoreUsage_BoxPlot <- scoreUsage_BoxPlot[, .(count = .N), by = list(score, usage)]
colnames(scoreUsage_BoxPlot) <- c('score', 'usage', 'Num.Items')
png(filename = paste0(analyticsDir, 'scoreUsage_BoxPlot_ggplot2_outliers_removed_unique_values.png'),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot2::ggplot(scoreUsage_BoxPlot,
                ggplot2::aes(x = score,
                             y = log(usage),
                             color = score)) +
  ggplot2::geom_jitter(ggplot2::aes(size = log(`Num.Items`)), shape = 1, width = .25) +
  ggplot2::xlab("ORES Quality Prediction") + 
  ggplot2::ylab("WDCM Item Re-use Statistic\n(log scale)") +
  ggplot2::scale_color_brewer(palette="Dark2") + 
  ggplot2::ggtitle(
    paste0(
      "Item Quality vs Item Re-use\n(Unique values per category; Outliers removed)\n", 
      scoresDistribution)) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12)) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "aliceblue"))
dev.off()

# - collect (positive) outliers in general (i.e. across the quality categories):
# - usage > median usage
# - excluding quality category: 'A'
positiveOutliers <- dataSet %>% 
  dplyr::select(item, usage, score)
positiveOutliers <- 
  positiveOutliers[!(score == '' | is.na(score) | is.na(usage)), ]
positiveOutliers$lower <- quantile(positiveOutliers$usage, .25) - IQR(positiveOutliers$usage)*1.5
positiveOutliers$upper <- quantile(positiveOutliers$usage, .75) + IQR(positiveOutliers$usage)*1.5
positiveOutliers <- positiveOutliers[score != 'A' & usage > upper]
positiveOutliers <- dplyr::select(positiveOutliers, 
                                  item, score, usage)
positiveOutliers <- positiveOutliers[order(-usage), head(.SD, 1000), by = score]
write.csv(positiveOutliers, 
          paste0(analyticsDir, "positiveOutliers.csv"))
rm(positiveOutliers); rm(scoreUsage_BoxPlot); gc()

# - distribution of the WDCM usage statistic for each ORES score category
scoreUsageDistribution <- dataSet %>% 
  dplyr::select(usage, score)
scoreUsageDistribution <- 
  scoreUsageDistribution[!(score == '' | is.na(score) | is.na(usage)), ]
# - chart
png(filename = paste0(analyticsDir, 'distribution_Quality_USage.png'),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot2::ggplot(scoreUsageDistribution,
                ggplot2::aes(x = log(usage))) +
  ggplot2::geom_histogram(stat = "count", color = "red", fill = "red") +
  ggplot2::facet_wrap(~score, scales="free") +
  ggplot2::xlab("WDCM Re-use statistic (log scale)") + 
  ggplot2::ylab("Num. Wikidata items") +
  ggplot2::scale_y_continuous(labels = comma) + 
  ggplot2::ggtitle(
    "WDCM Re-use in Wikimedia Projects\n(Quality classes w. outliers included)"
    ) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12)) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "aliceblue"))
dev.off()

### --- latest revid timestamp distributions
dataSet <- dplyr::select(dataSet, 
                         score,
                         timestamp)
dataSet <- dataSet[, timestamp := substr(timestamp, 1, 7)]
dataSet <- dataSet[!(score == '' | is.na(score))]
dataSet <- dataSet[, .(`Num.items` = .N), by = list(score, timestamp)]
dataSet <- dataSet[order(score, timestamp)]
# - filter out current month:
# - due to a possibility of missing data
# - in the current snapshot of the wmf.mediawiki_history table
currentYM <- as.character(Sys.time())
currentYM <- strsplit(currentYM, split = "-")[[1]][1:2]
currentYM <- paste(currentYM, collapse = "-")
dataSet <- dataSet[!grepl(currentYM, timestamp)]
write.csv(dataSet, 
          paste0(analyticsDir, "revids_timeline_quality_classes.csv"))
# - chart: all quality categories
png(filename = paste0(analyticsDir, 'revids_timeline_quality_classes.png'),
    width = 800, height = 607, units = "px",
    bg = "white",
    res = 72,
    type = c("cairo-png")
)
ggplot2::ggplot(dataSet,
                ggplot2::aes(x = timestamp,
                             y = log(`Num.items`),
                             group = score,
                             fill = score,
                             color = score)) +
  ggplot2::geom_line(size = .25) + 
  ggplot2::geom_point(size = 1.5) + 
  ggplot2::geom_point(size = 1, color = "white") + 
  ggplot2::xlab("Year-Month") + 
  ggplot2::ylab("log(Num. Wikidata items)") +
  ggplot2::scale_y_continuous(labels = scales::comma) + 
  ggplot2::scale_color_brewer(palette="Dark2") + 
  ggplot2::ggtitle(
    "When did the item receive its latest revision?\n(NOTE. Only items with the ORES quality class prediction are considered)"
    ) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12)) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "aliceblue"))
dev.off()

# - to runtime Log:
print(paste("--- Copy to public Dir RUN STARTED ON:", 
            Sys.time(), sep = " "))

### --- copy to public dir
system(command = paste0('cp ', analyticsDir, '* ', publicDir),
       wait = T)

### - log
# - to runtime Log:
print(paste("--- WD_ORES_ItemQuality.R RUN ENDS ON:", 
            Sys.time(), sep = " "))
# - archive log
# - copy the main log file to published for timestamp
# - toRuntime log:
print("Copy main log to published; clean up log.")
system(command = 
         paste0('cp ', fPath, 'WD_ORES_ItemQuality_RUNTIME.log ' , logDir),
       wait = T)
# - clean up
file.remove(paste0(fPath, 
                   'WD_ORES_ItemQuality_RUNTIME.log'))

# - GENERAL TIMING:
generalT2 <- Sys.time()
# - GENERAL TIMING REPORT:
print(paste0("--- WD_ORES_ItemQuality.R RUN COMPLETED IN: ", 
             generalT2 - generalT1, "."))
