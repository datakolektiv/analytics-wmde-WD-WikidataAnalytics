#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- WD_IdentifierLandscape_Data.R
### --- Author(s): Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- R data wrangling and statistical procedures forWD JSON dumps in hdfs
### --- NOTE: launches WD_IdentifierLandscape_Data.py on WMF Analytics
### --- Cluster for ETL (Pyspark)
### ---------------------------------------------------------------------------

# - note: GitHub repo dselivanov/text2vec@0.5.0
# - install_github("dselivanov/text2vec@0.5.0")
# - renv::install("dselivanov/text2vec@0.5.0")

# - to runtime Log:
print(paste("--- WD_IndentifierLandscape.R RUN STARTED ON:", 
            Sys.time(), sep = " "))
# - GENERAL TIMING:
generalT1 <- Sys.time()

# - to runtime Log:
print(paste("--- WD_IndentifierLandscape.R: Init.", 
            Sys.time(), sep = " "))

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
# - to runtime Log:
print(paste("--- WD_IndentifierLandscape.R: parse parameters.", 
            Sys.time(), sep = " "))
params <- XML::xmlParse(
  paste0(
    fPath, "WDIdentifiersLandscape_Config.xml")
  )
params <- XML::xmlToList(params)

### --- Directories
# - to runtime Log:
print(paste("--- WD_IndentifierLandscape.R: dirTree.", 
            Sys.time(), sep = " "))
# - form paths:
dataDir <- params$general$dataDir
logDir <- params$general$logDir
analysisDir <- params$general$analysisDir
etl_hdfsDir <- params$general$etl_hdfsDir
# - production published-datasets:
publicDir <- params$general$publicDir
# - endpoint for Blazegraph GAS program  
endPointURL <- params$general$wdqs_endpoint

# - spark2-submit parameters:
paramsDeploy <- XML::xmlParse(
  paste0(
    fPath,
    "WDIdentifiersLandscape_Config_Deploy.xml")
  )
paramsDeploy <- XML::xmlToList(paramsDeploy)
sparkMaster <- paramsDeploy$spark$master
sparkDeployMode <- paramsDeploy$spark$deploy_mode
sparkNumExecutors <- paramsDeploy$spark$num_executors
sparkDriverMemory <- paramsDeploy$spark$driver_memory
sparkExecutorMemory <- paramsDeploy$spark$executor_memory
sparkExecutorCores <- paramsDeploy$spark$executor_cores
sparkConfigDynamic <- paramsDeploy$spark$config

### --- Fetch all Wikidata external identifiers
# - to runtime Log:
print(paste("--- WD_IndentifierLandscape.R: WDQS: fetch identifiers.", 
            Sys.time(), sep = " "))
# - Set proxy
WMDEData::set_proxy(http_proxy = params$general$http_proxy,
                    https_proxy = params$general$https_proxy)
# - Send query
# - Q19847637: Wikidata property for an identifier
query <- 'SELECT ?item ?itemLabel ?class ?classLabel {
  ?item wdt:P31/wdt:P279* wd:Q19847637 .
  ?item wdt:P31 ?class .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
  }'
rc <- WMDEData::wdqs_send_query(query = query,
                                SPARQL_Endpoint = endPointURL,
                                max_retry = 10)
# - parse rc
identifiers <- jsonlite::fromJSON(rc)
identifiers <- data.frame(property = identifiers$results$bindings$item$value, 
                          label = identifiers$results$bindings$itemLabel$value,
                          class = identifiers$results$bindings$class$value, 
                          classLabel = identifiers$results$bindings$classLabel$value,
                          stringsAsFactors = F)
rm(rc); gc()

# - clean up identifiers$property, identifiers$class
identifiers$property <- gsub("http://www.wikidata.org/entity/", 
                             "", 
                             identifiers$property)
identifiers$class <- gsub("http://www.wikidata.org/entity/", 
                          "", 
                          identifiers$class)
# - store identifiers
# - to runtime Log:
print(paste("--- WD_IndentifierLandscape.R: Store identifiers (analysisDir).", 
            Sys.time(), sep = " "))
write.csv(identifiers, 
          paste0(
            analysisDir, 
            "WD_ExternalIdentifiers_DataFrame.csv")
          )

### --- Fetch the sub-classes of all
### -- Wikidata external identifier classes
# - to runtime Log:
print(paste(
  "--- WD_IndentifierLandscape.R: ", 
  "WDQS: Fetch the sub-classes of all external identifier classes.",
  Sys.time(), 
  sep = " "))
weiClasses <- unique(identifiers$class)
weiClassesTable <- vector(mode = "list", 
                          length = length(weiClasses))
# - iterate over weiClasses
for (i in 1:length(weiClasses)) {
  # - to runtime Log:
  print(paste0(
    "Fetching sub-class: ", 
    i, 
    ". out of ", 
    length(weiClasses), 
    "."))
  query <- paste0('SELECT ?class ?classLabel { ?class wdt:P279/wdt:P279* wd:', 
                  weiClasses[i], 
                  ' . SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }}')
  res <- WMDEData::wdqs_send_query(query = query,
                                   SPARQL_Endpoint = endPointURL,
                                   max_retry = 10)
  # - parse res
  wT <- jsonlite::fromJSON(res)
  if (class(wT$results$bindings) == "data.frame") {
    weiClassesTable[[i]] <- 
      data.frame(class = weiClasses[i],
                 subClass = gsub("http://www.wikidata.org/entity/",
                                 "",
                                 wT$results$bindings$class$value),
                 subClassLabel = wT$results$bindings$classLabel$value,
                 stringsAsFactors = F)
    } else {
      weiClassesTable[[i]] <- NULL
    }
  Sys.sleep(1)
}
# - wrangle weiClasses
weiClassesTable <- data.table::rbindlist(weiClassesTable)
weiClassesTable <- 
  dplyr::filter(weiClassesTable,
                subClass %in% weiClasses)
# - store weiClassesTable
# - to runtime Log:
print(paste(
  "--- WD_IndentifierLandscape.R: ", 
  "Store the sub-classes of all external identifier classes (analysisDir).",
  Sys.time(), 
  sep = " ")
  )
write.csv(weiClassesTable, 
          paste0(
            analysisDir, 
            "WD_ExternalIdentifiers_SubClasses.csv")
          )

### --- Run ETL Procedure from WD dump:

# - toRuntime Log:
print(paste("--- WD_IndentifierLandscape.R: prepare ETL phase, clean dataDir: ", 
            Sys.time(), sep = " "))
file.remove(list.files(dataDir))
# - toRuntime Log:
print(paste("--- WD_IndentifierLandscape.R: prepare ETL phase, Kerberos init: ", 
            Sys.time(), sep = " "))
# - Kerberos init
WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
# - Run Spark ETL
WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                            pysparkPath = paste0(fPath, "WD_IdentifierLandscape_Data.py"),
                            sparkMaster = sparkMaster,
                            sparkDeployMode = sparkDeployMode,
                            sparkNumExecutors = sparkNumExecutors,
                            sparkDriverMemory = sparkDriverMemory,
                            sparkExecutorMemory = sparkExecutorMemory,
                            sparkConfigDynamic = sparkConfigDynamic)
# - toRuntime Log:
print(paste(
  "--- WD_IndentifierLandscape.R: ", 
  "Pyspark ETL, WD_IdentifierLandscape_Data.py: ends: ",
  Sys.time(), 
  sep = " "))

### --- Compose usage dataset

# - toRuntime Log:
print(paste(
  "--- WD_IndentifierLandscape.R: ",
  "Compose final usage dataset, copy from hdfs: starts: ",
  Sys.time(), sep = " "))
# - clean up dataDir
if (length(list.files(dataDir)) > 1) {
  file.remove(paste0(dataDir, list.files(dataDir)))
}

# - read splits from hdfs to local dataDir

# - from statements:
print(paste(
  "--- WD_IndentifierLandscape.R: ",
  "Compose final usage dataset, read from: statements. ",
  Sys.time(), sep = " "))
statementsSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                          localPath = dataDir,
                                          localFilenamePrefix = "wd_exts_data_stat_",
                                          hdfsDir = etl_hdfsDir,
                                          hdfsFilenamePrefix = "wd_extId_data_stat_.csv",
                                          fr_header = FALSE)

# - from references:
print(paste(
  "--- WD_IndentifierLandscape.R: ",
  "Compose final usage dataset, read from: references. ",
  Sys.time(), sep = " "))
referencesSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                          localPath = dataDir,
                                          localFilenamePrefix = "wd_exts_data_ref_",
                                          hdfsDir = etl_hdfsDir,
                                          hdfsFilenamePrefix = "wd_extId_data_ref_.csv",
                                          fr_header = FALSE)

# - from qualifiers:
print(paste(
  "--- WD_IndentifierLandscape.R: ",
  "Compose final usage dataset, read from: qualifiers. ",
  Sys.time(), sep = " "))
qualifiersSet <- WMDEData::hdfs_read_from(kerberosUser = "analytics-privatedata",
                                          localPath = dataDir,
                                          localFilenamePrefix = "wd_exts_data_qual_",
                                          hdfsDir = etl_hdfsDir,
                                          hdfsFilenamePrefix = "wd_extId_data_qual_.csv",
                                          fr_header = FALSE)

# - dataSet: collect + schema
dataSet <- data.table::rbindlist(
  list(
    statementsSet,
    referencesSet,
    qualifiersSet
    )
  )
colnames(dataSet) <- c("item", "property")

# - clean up
rm(statementsSet); rm(referencesSet); rm(qualifiersSet);
gc()

# - toRuntime Log:
print(paste(
  "--- WD_IndentifierLandscape.R: ", 
  "Load hdfs splits -> produce usage dataset, ends: ",
  Sys.time(), 
  sep = " "))

### --- wrangle usage dataset

# - clean up item column
# - how many missing data in the item column
wMissItem <- which(is.na(dataSet$item))
nMissItem <- length(wMissItem)
print(paste0("Check, N missing items: ", nMissItem))
# - clean up property column
# - how many missing data in the property column
wMissProperty <- which(is.na(dataSet$property))
nMissProperty <- length(wMissProperty)
print(paste0("Check, N missing properties: ", nMissProperty))
# - clean up dataSet from NAs
rrow <- unique(c(wMissItem, wMissProperty))
if (length(rrow) > 0) {
  dataSet <- dataSet[-rrow, ]
}
# - remove duplicated rows, if any
dataSet <- dataSet[!duplicated(dataSet), ]

### --- Enrich identifiers data from final usage dataset
identifiers$used <- F
wUsed <- 
  which(identifiers$property %in% unique(dataSet$property))
identifiers$used[wUsed] <- T
# - store identifiers
write.csv(identifiers, 
          paste0(
            analysisDir, 
            "WD_ExternalIdentifiers_DataFrame.csv"))

### --- compute similarity structure between identifiers
### --- NOTE: keep track of essential statistics

# - to runtime Log:
print(paste(
  "--- Compute Global Jaccard Similarity Matrix STARTED ON:",
  Sys.time(), 
  sep = " "))

# - stats list:
stats <- list()

# - stats: N item-identifier pairs
stats$N_item_identifier_pairs <- dim(dataSet)[1]

# - contingency table:
dat <- stats::xtabs(~ property + item,
                    data = dataSet,
                    sparse = T)

# - stats: N of used identifiers
stats$N_identifiers_used <- dim(dat)[1]
# - stats: N of items w. external identifiers
stats$N_items_w_identifiers <- dim(dat)[2]
# - stats: N total number of external identifiers
stats$N_total_identifiers <- length(unique(identifiers$property))
# - stats: N total number of identifier classes
stats$N_total_identifier_classes <- length(unique(identifiers$class))
# - stats: N total number of identifier classes used
wPropertyUsed <- which(unique(identifiers$property) %in% 
                         unique(rownames(dat))) 
classesUsed <- unique(identifiers$class[wPropertyUsed])
stats$N_total_identifier_classes_used <- length(classesUsed)

# - compute identifier usage
identifierUsage <- dataSet %>% 
  dplyr::group_by(property) %>% 
  dplyr::summarise(usage = dplyr::n())
# - clean up: dataSet
rm(dataSet); gc()
# - joing identifier usage w. identifiers to obtain classes
identifierUsage <- dplyr::left_join(identifierUsage,
                                    identifiers,
                                    by = "property")
identifierUsage$used <- NULL
# - store identifierUsage
write.csv(identifierUsage, 
          paste0(
            analysisDir, 
            "WD_ExternalIdentifiers_Usage.csv")
          )

# - compute co-occurences
co_occur <- spam::crossprod.spam(Matrix::t(dat), 
                                 y = NULL)
co_occur <- as.matrix(co_occur)
diag(co_occur) <- 0
# - sum of co-occurences for each identifier
co_identifier <- rowSums(co_occur)
# - store identifier co-occurences
write.csv(co_occur, 
          paste0(
            analysisDir, 
            "WD_ExternalIdentifiers_Co-Occurence.csv")
          )

# - comput Jaccard Similarity Matrix
t1 <- Sys.time()
print(paste0("Jaccard distance matrix, start: ", Sys.time()))
distMatrix <- text2vec::sim2(x = dat, 
                             y = NULL,
                             method = "jaccard",
                             norm = "none")
print(paste0("Jaccard distance matrix in: ", Sys.time() - t1))
rm(dat); gc()

# - Jaccard similarity index to Jaccard distance
distMatrix <- as.matrix(1 - distMatrix)
diag(distMatrix) <- 0
distMatrix <- as.data.frame(distMatrix)
rownames(distMatrix) <- rownames(distMatrix)
colnames(distMatrix) <- colnames(distMatrix)
distMatrix$coOccur <- co_identifier
idUse <- dplyr::select(identifierUsage, 
                       property, 
                       usage)
idUse <- idUse[!duplicated(idUse), ]
distMatrix$usage <- idUse$usage
# - add identifier labels
idLabs <- dplyr::select(identifierUsage, 
                        property, 
                        label)
idLabs <- idLabs[!duplicated(idLabs), ]
distMatrix$label <- idLabs$label
# - store distMatrix
write.csv(distMatrix, 
          paste0(analysisDir, 
                 "WD_ExternalIdentifiers_JaccardDistance.csv")
          )

### --- produce 2D tSNE identifier map
# - to runtime Log:
print(paste("--- tSNE on Jaccard Similarity Matrix starts:", 
            Sys.time(), sep = " "))
# - matrix
m <- dplyr::select(distMatrix, 
                   -label, 
                   -coOccur, 
                   -usage)
# - tSNE dimensionality reduction
t1 <- Sys.time()
tsneMap <- Rtsne::Rtsne(as.matrix(m),
                        theta = 0,
                        is_distance = T,
                        tsne_perplexity = 10,
                        max_iter = 5000,
                        verbose = T)
# - to runtime Log:
print(paste0("tSNE done in: ", Sys.time() - t1))
tsneMap <- tsneMap$Y
colnames(tsneMap) <- c("D1", "D2")
tsneMap <- cbind(tsneMap, 
                 dplyr::select(distMatrix, 
                               label, 
                               coOccur, 
                               usage)
                 )
tsneMap$property <- rownames(distMatrix)
tsneMap <- dplyr::arrange(tsneMap, desc(usage))
# - store tsneMap
write.csv(tsneMap, 
          paste0(analysisDir, 
                 "WD_ExternalIdentifiers_tsneMap.csv")
)
# - to runtime Log:
print(paste("--- tSNE on Jaccard Similarity Matrix ends:", 
            Sys.time(), sep = " "))
print(paste0("--- tSNE on Jaccard Similarity Matrix done in: ", 
             Sys.time() - t1))

### --- Pre-process for the WD Identifier Landscape
# - to runtime Log:
print(paste("--- Pre-process for the WD Identifier Landscape now:", 
            Sys.time(), sep = " "))
### --- Dashboard
wd_IdentifiersFrame <- identifiers
ids <- wd_IdentifiersFrame %>% 
  dplyr::select(property, 
                label, 
                used)
ids <- ids[!duplicated(ids), ]
colnames(ids) <- c("identifier", 
                   "identifierLabel", 
                   "identifierUsed")
identifierClass <- wd_IdentifiersFrame %>% 
  dplyr::select(class, classLabel)
identifierClass <- 
  identifierClass[!duplicated(identifierClass), ]
usedIdentifierLabels <- 
  unique(wd_IdentifiersFrame$label[wd_IdentifiersFrame$used == T])
usedClassLabels <- 
  unique(wd_IdentifiersFrame$classLabel[wd_IdentifiersFrame$used == T])
# - fix wd_CoOccurence
# - to runtime Log:
print(paste("--- fix wd_CoOccurence now:", 
            Sys.time(), sep = " "))
wd_CoOccurence <- co_occur
coOcCols <- data.frame(cols = colnames(wd_CoOccurence), 
                       stringsAsFactors = F)
coOcCols <- dplyr::left_join(coOcCols, 
                             ids,
                             by = c("cols" = "identifier"))
coOcCols$identifierLabel[is.na(coOcCols$identifierLabel)] <- 
  coOcCols$cols[is.na(coOcCols$identifierLabel)]
coOcCols$identifierLabel <- 
  paste0(coOcCols$identifierLabel, " (", coOcCols$cols, ")")
colnames(wd_CoOccurence) <- coOcCols$identifierLabel
wd_CoOccurence <- as.data.frame(wd_CoOccurence)
wd_CoOccurence$Identifier <- coOcCols$identifierLabel
# - identifierConnected for similarityGraph 
# - to runtime Log:
print(paste("--- produce identifierConnected for similarityGraph now:", 
            Sys.time(), sep = " "))
identifierConnected <- tidyr::gather(wd_CoOccurence,
                                     key = Code,
                                     value = coOcur,
                                     -Identifier) %>%
  dplyr::arrange(Code, Identifier, dplyr::desc(coOcur)) %>%
  dplyr::filter(coOcur != 0)
iC <- lapply(unique(identifierConnected$Code), function(x){
  d <- identifierConnected[identifierConnected$Code == x, ]
  w <- which(d$coOcur == max(d$coOcur))
  d$Identifier[w]
})
names(iC) <- unique(identifierConnected$Code)
identifierConnected <- stack(iC)
colnames(identifierConnected) <- c("Incoming", "Outgoing")
# - produce Identifier Landscape Graph
idNet <- data.frame(from = identifierConnected$Outgoing,
                    to = identifierConnected$Incoming,
                    stringsAsFactors = F)
idNet <- igraph::graph.data.frame(idNet,
                                  vertices = NULL,
                                  directed = T)
# - to runtime Log:
print(paste("--- Fruchterman and Reingold from idNet now:", 
            Sys.time(), sep = " "))
L <- igraph::layout_with_fr(idNet, grid = "nogrid")
# - store Identifier Landscape Graph
saveRDS(L, paste0(analysisDir,
                  "WD_ExternalIdentifiers_Graph.Rds"))
# - store Identifier Landscape Graph as.data.frame
L <- as.data.frame(L)
write.csv(L, paste0(analysisDir, 
                 "WD_ExternalIdentifiers_Graph.csv"))
# - store identifierConnected
write.csv(identifierConnected, 
          paste0(analysisDir, 
                 "WD_IdentifierConnected.csv"))

### --- Structure for identifier neighbourhood graphs
# - to runtime Log:
print(paste("--- Structure for identifier neighbourhood graphs now:", 
            Sys.time(), sep = " "))
identifierConnected10 <- tidyr::gather(wd_CoOccurence,
                                       key = Code,
                                       value = coOcur,
                                       -Identifier) %>%
  dplyr::arrange(Code, Identifier, dplyr::desc(coOcur)) %>%
  dplyr::filter(coOcur != 0)
iC <- lapply(unique(identifierConnected10$Code), function(x){
  d <- identifierConnected10[identifierConnected10$Code == x, ]
  w <- which(d$coOcur %in% sort(d$coOcur, decreasing = T)[1:10])
  d$Identifier[w]
})
names(iC) <- unique(identifierConnected10$Code)
identifierConnected10 <- stack(iC)
colnames(identifierConnected10) <- c("Incoming", "Outgoing")
# - store identifierConnected10
write.csv(identifierConnected10, 
          paste0(analysisDir, 
                  "WD_identifierConnected10.csv"))

### --- Final operations: copy to public dir
print(paste("--- Final operations: copy to public dir now:", 
            Sys.time(), sep = " "))

# - Update info:
updateInfo <- data.frame(Time = Sys.time())
write.csv(updateInfo, 
          paste0(
            analysisDir, 
            "WD_ExtIdentifiers_UpdateInfo.csv")
          )

### --- Copy the datasets to publicDir
print(paste("--- Copy datasets to public directory: ", 
            Sys.time(), sep = " "))
# - form stats
stats <- as.data.frame(stats)
write.csv(stats, 
          paste0(analysisDir, 
                 "WD_ExternalIdentifiers_Stats.csv")
)
# - copy
system(command = 
         paste0('cp ', analysisDir, '* ' , publicDir),
       wait = T)

# - to runtime Log:
print(paste("--- WD_IndentifierLandscape.R RUN COMPLETED ON: ", 
            Sys.time(), sep = " "))

# - GENERAL TIMING:
print(paste("--- WD_IndentifierLandscape.R TOTAL RUNTIME: ", 
            Sys.time() - generalT1, sep = " "))

