#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Curious Facts
### --- Version 1.0.0
### --- Script: Q_CF_Functions.R
### --- September 2021.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: Functions to support QURATOR Qurious Facts
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of QURATOR Curious Facts
### ---
### --- QURATOR Curious Facts is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- QURATOR Curious Facts is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with QURATOR Curious Facts If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Function: wd_cluster_fetch_items_M1()
# - fetch items that are P31/P279* of class
# - from the WD JSON dump in hdfs
# - via Pyspark
wd_cluster_fetch_items_M1 <- function(class,
                                      targetProperty,
                                      referenceClass,
                                      fPath,
                                      dataDir) {

  # - parameters:
  # - class: items are P31/P279* of class
  # - targetProperty: the target property
  # - referenceClass: the expected class of the targetProperty datavalue
  # - fPath: the working directory path
  # - dataDir: the local data directory
  
  # - set parameters for wd_cluster_fetch_items.py
  print(
    "--- wd_cluster_fetch_items_M1: set parameters for wd_cluster_fetch_items.py"
    )
  params <- 
    xml2::read_xml(paste0(
      fPath, "wd_cluster_fetch_items.xml"))
  params <- xml2::as_list(params)
  hdfsDir <- params$parameters$hdfsDir[[1]]
  http_proxy <- params$parameters$http_proxy[[1]]
  https_proxy <- params$parameters$https_proxy[[1]]
  
  # - enter problem-specific parameters:
  print(
    "--- wd_cluster_fetch_items_M1: enter problem-specific parameters"
    )
  params$parameters$class[[1]] <- class
  params$parameters$targetProperty[[1]] <- targetProperty
  params$parameters$referenceClass[[1]] <- referenceClass
  params$parameters$dataDir[[1]] <- dataDir
  params <- xml2::as_xml_document(params)
  print("--- wd_cluster_fetch_items_M1: write XML parameters")
  xml2::write_xml(params,
                  paste0(fPath, "wd_cluster_fetch_items.xml"))
  
  # - Spark deployment parameters:
  paramsDeployment <- XML::xmlParse(paste0(
    fPath, "wd_cluster_fetch_items_Deployment.xml"))
  paramsDeployment <- XML::xmlToList(paramsDeployment)
  
  # - spark2-submit parameters:
  sparkMaster <- paramsDeployment$spark$master
  sparkDeployMode <- paramsDeployment$spark$deploy_mode
  sparkNumExecutors <- paramsDeployment$spark$num_executors
  sparkDriverMemory <- paramsDeployment$spark$driver_memory
  sparkExecutorMemory <- paramsDeployment$spark$executor_memory
  sparkConfigDynamic <- paramsDeployment$spark$config
  
  # - public WDQS:
  endPointURL <- 
    'https://query.wikidata.org/bigdata/namespace/wdq/sparql?query='
  
  # - clean hdfs dir
  print("--- wd_cluster_fetch_items_M1: clean hdfs dir")
  system(command = paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rmr ',
    hdfsDir, "*"),
    wait = T)
  
  # - SPARQL: collect all subclasses of class
  print("--- wd_cluster_fetch_items_M1: SPARQL: collect all subclasses of class")
  # - construct query:
  qr <- paste0('SELECT ?subclass WHERE {?subclass wdt:P279/wdt:P279* wd:',
               class, '}')
  res <- WMDEData::wdqs_send_query(query = qr, 
                                   SPARQL_Endpoint = endPointURL,
                                   max_retry = 10)
  # - parse res:
  print("--- wd_cluster_fetch_items_M1: SPARQL: parse WDQS results")
  res <- jsonlite::fromJSON(res, simplifyDataFrame = T)
  res <- res$results$bindings
  res <- data.frame(subclass = res$subclass$value,
                    stringsAsFactors = F)
  res$subclass <- gsub("http://www.wikidata.org/entity/", "", res$subclass)
  # - move to hdfs directory:
  print("--- wd_cluster_fetch_items_M1: move to hdfs directory:")
  write.csv(res, 
            paste0(fPath, "subclasses.csv"))
  WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata", 
                         localPath = fPath,
                         localFilename = "subclasses.csv", 
                         hdfsDir = hdfsDir)
  
  # - SPARQL: collect all subclasses of referenceClass
  print("--- wd_cluster_fetch_items_M1: collect all subclasses of referenceClass:")
  # - construct query:
  qr <- paste0('SELECT ?subclass WHERE {?subclass wdt:P279/wdt:P279* wd:',
               referenceClass, '}')
  res <- WMDEData::wdqs_send_query(query = qr, 
                                   SPARQL_Endpoint = endPointURL,
                                   max_retry = 10)
  # - parse res:
  print("--- wd_cluster_fetch_items_M1: parse WDQS result.")
  res <- jsonlite::fromJSON(res, simplifyDataFrame = T)
  res <- res$results$bindings
  res <- data.frame(subclass = res$subclass$value,
                    stringsAsFactors = F)
  res$subclass <- gsub("http://www.wikidata.org/entity/", "", res$subclass)
  # - move to hdfs directory:
  print("--- wd_cluster_fetch_items_M1: move to hdfs directory.")
  write.csv(res, 
            paste0(fPath, "refClassSubclasses.csv"))
  WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata", 
                         localPath = fPath,
                         localFilename = "refClassSubclasses.csv", 
                         hdfsDir = hdfsDir)

  # - Pyspark ETL:
  
  # - Kerberos init
  print("--- wd_cluster_fetch_items_M1: Kerberos init.")
  # - Kerberos init
  WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
  # - Run Spark ETL
  print("--- wd_cluster_fetch_items_M1: Run PySpark ETL (wd_cluster_fetch_items_M1.py)")
  WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                              pysparkPath = paste0(fPath, "wd_cluster_fetch_items_M1.py"),
                              sparkMaster = sparkMaster,
                              sparkDeployMode = sparkDeployMode,
                              sparkNumExecutors = sparkNumExecutors,
                              sparkDriverMemory = sparkDriverMemory,
                              sparkExecutorMemory = sparkExecutorMemory,
                              sparkConfigDynamic = sparkConfigDynamic)
  # - report
  print("--- wd_cluster_fetch_items_M1: DONE; Exit.)")
  return(TRUE)
  
}


### --- Function: wd_cluster_fetch_items_M2()
wd_cluster_fetch_items_M2 <- function(targetProperty,
                                      referenceProperty,
                                      referenceClasses,
                                      fPath,
                                      dataDir) {
  
  # - parameters:
  # - targetPropety: items with this property
  # - their values on referenceProperty
  # - ARE NOT IN: referenceClasses
  # - fPath: the working directory path
  # - dataDir: local filesystem dataDir
  
  # - set parameters for wd_cluster_fetch_items_M2.py
  print(
    "--- wd_cluster_fetch_items_M2: set parameters for wd_cluster_fetch_items_M2.py"
    )
  params <- xml2::read_xml(paste0(
    fPath, "wd_cluster_fetch_items_M2.xml"))
  params <- xml2::as_list(params)
  hdfsDir <- params$parameters$hdfsDir[[1]]
  http_proxy <- params$parameters$http_proxy
  https_proxy <- params$parameters$https_proxy
  # - enter problem-specific parameters:
  print(
    "--- wd_cluster_fetch_items_M2: enter problem-specific parameters"
    )
  params$parameters$targetProperty[[1]] <- targetProperty
  params$parameters$referenceProperty[[1]] <- referenceProperty
  params$parameters$referenceClasses[[1]] <- referenceClasses
  params$parameters$dataDir[[1]] <- dataDir
  params <- xml2::as_xml_document(params)
  print("--- wd_cluster_fetch_items_M2: write XML parameters")
  xml2::write_xml(params,
                  paste0(fPath, "wd_cluster_fetch_items_M2.xml"))
  
  # - Spark deployment parameters:
  paramsDeployment <- XML::xmlParse(
    paste0(fPath, "wd_cluster_fetch_items_Deployment.xml"))
  paramsDeployment <- XML::xmlToList(paramsDeployment)
  # - spark2-submit parameters:
  sparkMaster <- paramsDeployment$spark$master
  sparkDeployMode <- paramsDeployment$spark$deploy_mode
  sparkNumExecutors <- paramsDeployment$spark$num_executors
  sparkDriverMemory <- paramsDeployment$spark$driver_memory
  sparkExecutorMemory <- paramsDeployment$spark$executor_memory
  sparkConfigDynamic <- paramsDeployment$spark$config
  
  # - public WDQS:
  endPointURL <- 'https://query.wikidata.org/bigdata/namespace/wdq/sparql?query='
  
  # - clean hdfs dir
  print("--- wd_cluster_fetch_items_M2: clean hdfs dir")
  system(command = paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rmr ',
    hdfsDir, "*"),
    wait = T)
  
  # - fetch all subclasses of referenceClasses
  # - SPARQL: collect all subclasses of referenceClass
  print("--- wd_cluster_fetch_items_M2: collect all subclasses of referenceClasses:")
  WMDEData::set_proxy(http_proxy = http_proxy, 
                      https_proxy = https_proxy)
  referenceClasses <- strsplit(referenceClasses, 
                               split = ", ")[[1]]
  referenceSubClasses <- vector(mode = "list", 
                                length = length(referenceClasses))
  checkWDQS <- TRUE
  for (i in 1:length(referenceClasses)) {
    print(paste0("WDQS: ", 
                 referenceClasses[i], 
                 "; ", i, ". out of: ", 
                 length(referenceClasses)))
    # - construct query:
    qr <- paste0('SELECT ?subclass WHERE {?subclass wdt:P279/wdt:P279* wd:',
                 referenceClasses[i], '}')
    # - run query:
    res <- WMDEData::wdqs_send_query(query = qr, 
                                     SPARQL_Endpoint = endPointURL,
                                     max_retry = 10)
    # - parse res:
    print("--- wd_cluster_fetch_items_M2: parse WDQS result.")
    res <- tryCatch({
      jsonlite::fromJSON(res, simplifyDataFrame = T)},
      error = function(condition) {
        message("--- wd_cluster_fetch_items_M2: CRITICAL WDQS parse failed.")
        return(NULL)
      })
    if (!is.null(res)) {
      res <- res$results$bindings
      res <- data.frame(subclass = res$subclass$value,
                        stringsAsFactors = F)
      res$subclass <- gsub("http://www.wikidata.org/entity/", "", res$subclass)
      referenceSubClasses[[i]] <- res
    } else {
      checkWDQS <- FALSE
      break
    }
  }
  if (checkWDQS) {
    referenceSubClasses <- 
      data.table::rbindlist(referenceSubClasses)
    referenceClasses <- data.frame(subclass = referenceClasses, 
                                   stringsAsFactors = F)
    referenceSubClasses <- 
      rbind(referenceSubClasses, referenceClasses) 
    referenceSubClasses <- 
      referenceSubClasses[!duplicated(referenceSubClasses), ]
    # - move all referenceClasses to hdfs directory:
    print("--- wd_cluster_fetch_items_M2: move to hdfs directory.")
    write.csv(referenceSubClasses, 
              paste0(fPath, "refClassSubclasses.csv"))
    WMDEData::hdfs_copy_to(kerberosUser = "analytics-privatedata", 
                           localPath = fPath,
                           localFilename = "refClassSubclasses.csv", 
                           hdfsDir = hdfsDir)
    
    # - Pyspark ETL:
    # - Kerberos init
    WMDEData::kerberos_init(kerberosUser = "analytics-privatedata")
    # - Run Spark ETL
    print(
      "--- wd_cluster_fetch_items_M2: Run PySpark ETL (wd_cluster_fetch_items_M2.py)"
    )
    WMDEData::kerberos_runSpark(kerberosUser = "analytics-privatedata",
                                pysparkPath = paste0(fPath, "wd_cluster_fetch_items_M2.py"),
                                sparkMaster = sparkMaster,
                                sparkDeployMode = sparkDeployMode,
                                sparkNumExecutors = sparkNumExecutors,
                                sparkDriverMemory = sparkDriverMemory,
                                sparkExecutorMemory = sparkExecutorMemory,
                                sparkConfigDynamic = sparkConfigDynamic)
    
    # - exit
    print("--- wd_cluster_fetch_items_M2: DONE; Exit.)")
    return(TRUE)
  } else {
    print("--- wd_cluster_fetch_items_M2: FATAL; WDQS FAILURE.)")
    return(FALSE)
  }
  
}


### --- Function: wd_fetchPropertyConstraints()
wd_fetchPropertyConstraints <- function(sparqlEndPointURL) {
  
  # - Construct Query:
  query <- paste0('SELECT ?Property_ ?Property_Label ?Property_Description ?class_ ?class_Label ?relation_ ?relation_Label
                      WHERE {
                        ?Property_ p:P2302 ?constraint_statement .
                        ?constraint_statement ps:P2302 wd:Q21503250 .
                        OPTIONAL {?constraint_statement pq:P2308 ?class_ .}
                        OPTIONAL {?constraint_statement pq:P2309 ?relation_ .}
                        SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                            } ')
  
  # - Run Query:
  repeat {
    res <- tryCatch({
      httr::GET(url = paste0(sparqlEndPointURL, URLencode(query)))
    },
    error = function(condition) {
      message("wd_fetchPropertyConstraints: something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      httr::GET(url = paste0(sparqlEndPointURL, URLencode(query)))
    },
    warning = function(condition) {
      message("wd_fetchPropertyConstraints: something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      httr::GET(url = paste0(sparqlEndPointURL, URLencode(query)))
    }
    )  
    if (res$status_code == 200) {
      message('wd_fetchPropertyConstraints: success.')
      break
    } else {
      message('wd_GAS_fetchItems: failed; retry.')
      Sys.sleep(10)
    }
  }
  
  # - Extract data:
  if (res$status_code == 200) {
    
    # - tryCatch rawToChar
    # - NOTE: might fail for very long vectors
    rc <- tryCatch(
      {
        rawToChar(res$content)
      },
      error = function(condition) {
        return(FALSE)
      }
    )
  }
  
  rm(res)
  
  if (rc == FALSE) {
    message('wd_fetchPropertyConstraints: rawToChar conversion failed.')
    stop()
  }
  
  # - is.ExceptionTimeout
  queryTimeout <- grepl("timeout", rc, ignore.case = TRUE)
  if (queryTimeout) {
    message("wd_GAS_fetchItems: query timeout detected; results are most probably incomplete.")
  }
  
  # - rc to data.frame
  rc <- jsonlite::fromJSON(rc, simplifyDataFrame = T)
  rc <- rc$results$bindings
  rc <- jsonlite::flatten(rc)
  
  # - output
  return(rc)
  
}
