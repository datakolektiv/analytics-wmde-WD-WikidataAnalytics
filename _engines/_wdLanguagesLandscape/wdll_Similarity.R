#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- wdll_Similarity.R
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- August 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate WD Languages Landscape modules:
### --- 3. wdll_Similarity.R
### --- Contingency, Similarity, and Distances in WDLL
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of WD Languages Landscape (WDLL)
### ---
### --- WDLL is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDLL is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDLL If not, see <http://www.gnu.org/licenses/>.

### ---------------------------------------------------------------------------
### --- Script 3: wdll_Similarity.R
### ---------------------------------------------------------------------------

### --- Setup

# - to runtime Log:
print(paste("--- wdll_Similarity.R RUN STARTED ON:", 
            Sys.time(), sep = " "))

# - fPath: where the scripts is run from?
# - to runtime Log:
print(paste("--- wdll_Similarity.R. Read params.", 
            Sys.time(), sep = " "))
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
library(spam)
library(spam64)

# - params
params <- XML::xmlParse(paste0(
  fPath, "WD_LanguagesLandscape_Config.xml")
  )
params <- XML::xmlToList(params)

# - directories
dataDir <- params$general$dataDir
logDir <- params$general$logDir
outDir <- params$general$outDir
publicDir <- params$general$pubDataDir
hdfsPath <- params$general$hdfsPath

# - ML parameters
tsne_theta <- as.numeric(params$general$tSNE_Theta)

# - publicDir
publicDir <- params$general$pubDataDir

### --- Data

# - load fundamental data set
# - to runtime Log:
print(paste("--- wdll_Similarity.R. Load fundamental dataset.", 
            Sys.time(), sep = " "))
dataSet <- readRDS(paste0(outDir, "wd_entities_languages.Rds"))
# - load wd_languages_count.csv
langCount <- data.table::fread(paste0(outDir, "wd_languages_count.csv"),
                               header = TRUE)
langCount$V1 <- NULL

### --- Compute co-occurences: labels across items

# - to runtime Log:
print(paste("--- wdll_Similarity.R. Compute co-occurences: labels across items.", 
            Sys.time(), sep = " "))
t1 <- Sys.time()
# - unique languages, unique entities
l <- unique(dataSet$language)
it <- unique(dataSet$entity)
# - language x language matrix
lang <- matrix(0, 
               nrow = length(l),
               ncol = length(l))
rownames(lang) <- l
colnames(lang) <- l
# - entity batches
it <- data.frame(item = it,
                 stringsAsFactors = FALSE)
it$rand <- runif(dim(it)[1], 0, 1)
it <- dplyr::arrange(it, rand)
it <- it$item
nBatches <- 10
batchSize <- round(length(it)/nBatches)
startIx <- numeric(nBatches)
stopIx <- numeric(nBatches)
for (i in 1:nBatches) {
  startIx[i] <- i * batchSize - batchSize + 1
  stopIx[i] <- i * batchSize
  if (stopIx[i] > length(it)) {
    stopIx[i] <- length(it)
  }
}
# - batch processing
for (i in 1:length(startIx)) {
  tb1 <- Sys.time()
  print(paste0(
    "Processing now contingency batch: ", 
    i, 
    " out of: ", 
    nBatches, ".")
    )
  print(paste0(
    "Slice now contingency batch: ", 
    i, 
    " out of: ", 
    nBatches, ".")
    )
  batchData <- 
    dataSet[dataSet$entity %in% it[startIx[i]:stopIx[i]], ]
  print(paste0(
    "xtabs now for contingency batch: ", 
    i, " out of: ", 
    nBatches, "."))
  cT <- xtabs(~ language + entity,
              data = batchData, 
              sparse = TRUE)
  rm(batchData)
  print(paste0(
    "co-occurences now for contingency batch: ", 
    i, 
    " out of: ", 
    nBatches, "."))
  co_occur <- spam::crossprod.spam(t(cT), y = NULL)
  rm(cT)
  co_occur <- as.matrix(co_occur)
  diag(co_occur) <- 0
  print(paste0(
    "enter now contingency batch: ", 
    i, 
    " out of: ", 
    nBatches, ".")
    )
  w1 <- which(rownames(lang) %in% rownames(co_occur))
  w2 <- which(colnames(lang) %in% colnames(co_occur))
  lang[w1, w2] <- lang[w1, w2] + co_occur
  rm(co_occur)
  print(paste0("Contingency table batch ", i, ". done in: ", 
               difftime(Sys.time(), tb1, units = "mins")))
}
# - report
print(paste0("Co-occurence matrix DONE in: ", 
             difftime(Sys.time(), t1, units = "mins")))
# - join langCount to co-occurences
lang <- as.data.frame(lang)
lang$language <- rownames(lang)
lang <- dplyr::left_join(lang, 
                         langCount,
                         by = "language")
# - store language co-occurences
# - to runtime Log:
print(paste(
  "--- wdll_Similarity.R. Store co-occurences: WD_Languages_Co-Occurrence.csv.",
  Sys.time(), 
  sep = " "))
write.csv(lang, 
          paste0(outDir, "WD_Languages_Co-Occurrence.csv"))

### --- compute similarity and distance matrix from co-occurrences

# - to runtime Log:
print(paste
      ("--- wdll_Similarity.R. Compute similarity and distance matrix from co-occurrences",
        Sys.time(), 
        sep = " "))
langMat <- as.matrix(lang[, 1:(dim(lang)[1])])
rownames(langMat) <- colnames(langMat)
# - cosine similarity:
cosineSimMatrix = text2vec::sim2(langMat, method = "cosine", norm = "l2")
cosineSimMatrix_Frame <- as.data.frame(cosineSimMatrix)
cosineSimMatrix_Frame$language <- rownames(cosineSimMatrix_Frame)
cosineSimMatrix_Frame <- dplyr::left_join(cosineSimMatrix_Frame,
                                          langCount,
                                          by = "language")
# - store language cosine similarity matrix
write.csv(cosineSimMatrix_Frame, 
          paste0(outDir, "WD_Languages_CosineSimilarity.csv"))
# - cosine distance:
cosineDistMatrix <- 1 - cosineSimMatrix
diag(cosineDistMatrix) <- 0
cosineDistMatrix_Frame <- as.data.frame(cosineDistMatrix)
cosineDistMatrix_Frame$language <- rownames(cosineDistMatrix_Frame)
cosineDistMatrix_Frame <- dplyr::left_join(cosineDistMatrix_Frame,
                                           langCount,
                                           by = "language")
# - store language cosine distance matrix
write.csv(cosineDistMatrix_Frame, 
          paste0(
            outDir, 
            "WD_Languages_CosineDistance.csv")
          )

# - tSNE 2D map from cosineDistMatrix_Frame
# - to runtime Log:
print(paste(
  "--- wdll_Similarity.R. tSNE 2D map from cosineDistMatrix_Frame.",
  Sys.time(), 
  sep = " ")
  )
cosineDistMatrix_Frame <- 
  as.matrix(
    cosineDistMatrix_Frame[, 1:(dim(cosineDistMatrix_Frame)[1])]
    )
tsne2DMap <- Rtsne::Rtsne(cosineDistMatrix_Frame,
                          theta = 0,
                          is_distance = TRUE,
                          tsne_perplexity = 10)
tsne2DMap <- as.data.frame(tsne2DMap$Y)
colnames(tsne2DMap) <- 
  paste("D", seq(1:dim(tsne2DMap)[2]), sep = "")
tsne2DMap$language <- colnames(cosineDistMatrix_Frame)
tsne2DMap <- dplyr::left_join(tsne2DMap, 
                              langCount, 
                              by = "language")
# - store tsne2DMap from Jaccard distance matrix
# - to runtime Log:
print(paste(
  "--- wdll_Similarity.R. store tsne2DMap from Jaccard distance matrix.",
  Sys.time(), 
  sep = " ")
  )
write.csv(tsne2DMap, 
          paste0(
            outDir, 
            "WD_tsne2DMap_from_cosineDistMatrix.csv")
          )

# - clean-up:
# - to runtime Log:
print(paste("--- wdll_Similarity.R. Clean up.", 
            Sys.time(), sep = " "))
rm(cosineSimMatrix); rm(cosineDistMatrix); 
rm(cosineSimMatrix_Frame); rm(cosineDistMatrix_Frame); 
rm(lang); rm(langMat); gc()

### --- compute binary co-occurrences for the Jaccard Similarity Matrix: 
### --- languages x items

# - to runtime Log:
print(paste(
  "--- wdll_Similarity.R. compute binary co-occurrences for the Jaccard Similarity Matrix.",
  Sys.time(), 
  sep = " "))
t1 <- Sys.time()
w1 <- match(dataSet$language, l)
w2 <- match(dataSet$entity, it)
cT <- Matrix::sparseMatrix(i = w1, j = w2, x = 1,
                           dims = c(length(l), length(it)),
                           dimnames = list(l, it)
)
print(paste0("Binary contingency in: ", 
             difftime(Sys.time(), t1, units = "mins"))
)
# - clean-up dataSet
rm(dataSet); gc()
# - compute Jaccard Similarity Matrix
distMatrix <- text2vec::sim2(x = cT, y = NULL, 
                   method = "jaccard", 
                   norm = "none")
print(paste0("Jaccard Similarity Matrix in: ", 
             difftime(Sys.time(), t1, units = "mins"))
)
# - clear
rm(cT); gc()
# - store language Jaccard similarity matrix
distMatrix <- as.matrix(distMatrix)
distMatrix <- as.data.frame(distMatrix)
distMatrix$language <- rownames(distMatrix)
distMatrix <- dplyr::left_join(distMatrix,
                               langCount,
                               by = "language")
# - to runtime Log:
print(paste("--- wdll_Similarity.R. store WD_Languages_Jaccard_Similarity.csv.", 
            Sys.time(), sep = " "))
write.csv(distMatrix, 
          paste0(outDir, "WD_Languages_Jaccard_Similarity.csv"))
# - Jaccard similarity index to Jaccard distance
distMatrix <- as.matrix(1 - distMatrix[, 1:(dim(distMatrix)[1])])
distMatrix <- as.data.frame(distMatrix)
distMatrix$language <- rownames(distMatrix)
distMatrix <- dplyr::left_join(distMatrix,
                               langCount,
                               by = "language")
# - store language Jaccard distance matrix
# - to runtime Log:
print(paste(
  "--- wdll_Similarity.R. store WD_Languages_Jaccard_Distance.csv.",
  Sys.time(), 
  sep = " "))
write.csv(distMatrix, 
          paste0(outDir, "WD_Languages_Jaccard_Distance.csv"))
# - report
print(paste0("Jaccard matrices DONE in: ", 
             difftime(Sys.time(), t1, units = "mins")))

### --- tSNE 2d: distMatrix from Jaccard distances

# - to runtime Log:
print(paste(
  "--- wdll_Similarity.R. tSNE 2d: distMatrix from Jaccard distances.",
  Sys.time(), 
  sep = " ")
  )
distMatrix <- as.matrix(
  distMatrix[, 1:(dim(distMatrix)[1])]
  )
tsne2DMap <- Rtsne::Rtsne(distMatrix,
                          theta = 0,
                          is_distance = TRUE,
                          tsne_perplexity = 10)
tsne2DMap <- as.data.frame(tsne2DMap$Y)
colnames(tsne2DMap) <- 
  paste("D", seq(1:dim(tsne2DMap)[2]), sep = "")
tsne2DMap$language <- colnames(distMatrix)
tsne2DMap <- dplyr::left_join(tsne2DMap, 
                              langCount, 
                              by = "language")
# - store tsne2DMap from Jaccard distance matrix
# - to runtime Log:
print(paste(
  "--- wdll_Similarity.R. store WD_tsne2DMap_from_Jaccard_Distance.csv.",
  Sys.time(), 
  sep = " ")
  )
write.csv(tsne2DMap, 
          paste0(
            outDir, 
            "WD_tsne2DMap_from_Jaccard_Distance.csv")
          )

# - to runtime Log:
print(paste("--- wdll_Similarity.R ENDED ON:", 
            Sys.time(), sep = " "))
