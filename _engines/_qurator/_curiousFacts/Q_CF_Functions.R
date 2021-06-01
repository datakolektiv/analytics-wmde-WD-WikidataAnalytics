#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: QURATOR Current Events
### --- Script: Q_CE_Functions.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: functions to support the Qurator Project(s) 
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of QURATOR Current Events
### ---
### --- QURATOR Current Events is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- QURATOR Current Events is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with QURATOR Current Events If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Packages
library(httr)
library(XML)
library(xml2)
library(data.table)
library(jsonlite)
library(tidyverse)

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
  # - fPath: the working directory path
  
  # - set parameters for wd_cluster_fetch_items.py
  print("--- wd_cluster_fetch_items_M1: set parameters for wd_cluster_fetch_items.py")
  params <- xml2::read_xml(paste0(fPath, "wd_cluster_fetch_items.xml"))
  params <- xml2::as_list(params)
  hdfsDir <- params$parameters$hdfsDir[[1]]
  http_proxy <- params$parameters$http_proxy
  https_proxy <- params$parameters$https_proxy
  # - enter problem-specific parameters:
  print("--- wd_cluster_fetch_items_M1: enter problem-specific parameters")
  params$parameters$class[[1]] <- class
  params$parameters$targetProperty[[1]] <- targetProperty
  params$parameters$referenceClass[[1]] <- referenceClass
  params$parameters$dataDir[[1]] <- dataDir
  params <- as_xml_document(params)
  print("--- wd_cluster_fetch_items_M1: write XML parameters")
  write_xml(params,
            paste0(fPath, "wd_cluster_fetch_items.xml"))
  # - Spark deployment parameters:
  paramsDeployment <- xmlParse(paste0(fPath, "wd_cluster_fetch_items_Deployment.xml"))
  paramsDeployment <- xmlToList(paramsDeployment)
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
  print("--- wd_cluster_fetch_items_M1: clean hdfs dir")
  system(command = paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -rmr ',
    hdfsDir, "*"),
    wait = T)
  
  # - SPARQL: collect all subclasses of class
  print("--- wd_cluster_fetch_items_M1: SPARQL: collect all subclasses of class")
  Sys.setenv(
    http_proxy = http_proxy,
    https_proxy = https_proxy)
  # - construct query:
  query <- paste0('SELECT ?subclass WHERE {?subclass wdt:P279/wdt:P279* wd:',
                  class, '}')
  # - run query:
  repeat {
    res <- tryCatch({
      GET(url = paste0(endPointURL, URLencode(query)))
    },
    error = function(condition) {
      print("Something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      GET(url = paste0(endPointURL, URLencode(query)))
    },
    warning = function(condition) {
      print("Something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      GET(url = paste0(endPointURL, URLencode(query)))
    }
    )  
    if (res$status_code == 200) {
      print(": success.")
      break
    } else {
      print(": failed.")
    }
  }
  # - parse res:
  print("--- wd_cluster_fetch_items_M1: SPARQL: parse WDQS results")
  res <- rawToChar(res$content)
  res <- fromJSON(res, simplifyDataFrame = T)
  res <- res$results$bindings
  res <- data.frame(subclass = res$subclass$value,
                    stringsAsFactors = F)
  res$subclass <- gsub("http://www.wikidata.org/entity/", "", res$subclass)
  # - move to hdfs directory:
  print("--- wd_cluster_fetch_items_M1: move to hdfs directory:")
  write.csv(res, 
            paste0(fPath, "subclasses.csv"))
  system(command = paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
    paste0(fPath, "subclasses.csv"), " ",
    hdfsDir),
    wait = T)
  
  # - SPARQL: collect all subclasses of referenceClass
  print("--- wd_cluster_fetch_items_M1: collect all subclasses of referenceClass:")
  Sys.setenv(
    http_proxy = http_proxy,
    https_proxy = https_proxy)
  # - construct query:
  query <- paste0('SELECT ?subclass WHERE {?subclass wdt:P279/wdt:P279* wd:',
                  referenceClass, '}')
  # - run query:
  repeat {
    res <- tryCatch({
      GET(url = paste0(endPointURL, URLencode(query)))
    },
    error = function(condition) {
      print("Something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      GET(url = paste0(endPointURL, URLencode(query)))
    },
    warning = function(condition) {
      print("Something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      GET(url = paste0(endPointURL, URLencode(query)))
    }
    )  
    if (res$status_code == 200) {
      print(": success.")
      break
    } else {
      print(": failed.")
    }
  }
  # - parse res:
  print("--- wd_cluster_fetch_items_M1: parse WDQS result.")
  res <- rawToChar(res$content)
  res <- fromJSON(res, simplifyDataFrame = T)
  res <- res$results$bindings
  res <- data.frame(subclass = res$subclass$value,
                    stringsAsFactors = F)
  res$subclass <- gsub("http://www.wikidata.org/entity/", "", res$subclass)
  # - move to hdfs directory:
  print("--- wd_cluster_fetch_items_M1: move to hdfs directory.")
  write.csv(res, 
            paste0(fPath, "refClassSubclasses.csv"))
  system(command = paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
    paste0(fPath, "refClassSubclasses.csv"), " ",
    hdfsDir),
    wait = T)
  
  # - Pyspark ETL:
  
  # - Kerberos init
  print("--- wd_cluster_fetch_items_M1: Kerberos init.")
  system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
         wait = T)
  
  # - Run PySpark ETL
  print("--- wd_cluster_fetch_items_M1: Run PySpark ETL (wd_cluster_fetch_items_M1.py)")
  system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                          sparkMaster, ' ',
                          sparkDeployMode, ' ',
                          sparkNumExecutors, ' ',
                          sparkDriverMemory, ' ',
                          sparkExecutorMemory, ' ',
                          sparkConfigDynamic, ' ',
                          paste0(fPath, 'wd_cluster_fetch_items_M1.py')
  ),
  wait = T)
  
  # - exit
  print("--- wd_cluster_fetch_items_M1: DONE; Exit.)")
  return('Done.')
  
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
  print("--- wd_cluster_fetch_items_M2: set parameters for wd_cluster_fetch_items_M2.py")
  params <- xml2::read_xml(paste0(fPath, "wd_cluster_fetch_items_M2.xml"))
  params <- xml2::as_list(params)
  hdfsDir <- params$parameters$hdfsDir[[1]]
  http_proxy <- params$parameters$http_proxy
  https_proxy <- params$parameters$https_proxy
  # - enter problem-specific parameters:
  print("--- wd_cluster_fetch_items_M2: enter problem-specific parameters")
  params$parameters$targetProperty[[1]] <- targetProperty
  params$parameters$referenceProperty[[1]] <- referenceProperty
  params$parameters$referenceClasses[[1]] <- referenceClasses
  params$parameters$dataDir[[1]] <- dataDir
  params <- as_xml_document(params)
  print("--- wd_cluster_fetch_items_M2: write XML parameters")
  write_xml(params,
            paste0(fPath, "wd_cluster_fetch_items_M2.xml"))
  # - Spark deployment parameters:
  paramsDeployment <- xmlParse(paste0(fPath, "wd_cluster_fetch_items_Deployment.xml"))
  paramsDeployment <- xmlToList(paramsDeployment)
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
  print("--- wd_cluster_fetch_items_M1: collect all subclasses of referenceClasses:")
  Sys.setenv(
    http_proxy = http_proxy,
    https_proxy = https_proxy)
  referenceClasses <- strsplit(referenceClasses, 
                               split = ", ")[[1]]
  referenceSubClasses <- vector(mode = "list", length = length(referenceClasses))
  for (i in 1:length(referenceClasses)) {
    print(paste0("WDQS: ", referenceClasses[i], "; ", i, ". out of: ", length(referenceClasses)))
    # - construct query:
    query <- paste0('SELECT ?subclass WHERE {?subclass wdt:P279/wdt:P279* wd:',
                    referenceClasses[i], '}')
    # - run query:
    repeat {
      res <- tryCatch({
        GET(url = paste0(endPointURL, URLencode(query)))
      },
      error = function(condition) {
        print("Something's wrong on WDQS: wait 10 secs, try again.")
        Sys.sleep(10)
        GET(url = paste0(endPointURL, URLencode(query)))
      },
      warning = function(condition) {
        print("Something's wrong on WDQS: wait 10 secs, try again.")
        Sys.sleep(10)
        GET(url = paste0(endPointURL, URLencode(query)))
      }
      )  
      if (res$status_code == 200) {
        print(": success.")
        break
      } else {
        print(": failed.")
      }
    }
    # - parse res:
    print("--- wd_cluster_fetch_items_M2: parse WDQS result.")
    res <- rawToChar(res$content)
    res <- fromJSON(res, simplifyDataFrame = T)
    res <- res$results$bindings
    res <- data.frame(subclass = res$subclass$value,
                      stringsAsFactors = F)
    res$subclass <- gsub("http://www.wikidata.org/entity/", "", res$subclass)
    referenceSubClasses[[i]] <- res
  }
  referenceSubClasses <- rbindlist(referenceSubClasses)
  referenceClasses <- data.frame(subclass = referenceClasses, 
                                 stringsAsFactors = F)
  referenceSubClasses <- rbind(referenceSubClasses, referenceClasses) 
  referenceSubClasses <- referenceSubClasses[!duplicated(referenceSubClasses), ]
  # - move all referenceClasses to hdfs directory:
  print("--- wd_cluster_fetch_items_M1: move to hdfs directory.")
  write.csv(referenceSubClasses, 
            paste0(fPath, "refClassSubclasses.csv"))
  system(command = paste0(
    'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -put -f ',
    paste0(fPath, "refClassSubclasses.csv"), " ",
    hdfsDir),
    wait = T)
  
  # - Pyspark ETL:
  
  # - Kerberos init
  print("--- wd_cluster_fetch_items_M2: Kerberos init.")
  system(command = 'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls', 
         wait = T)
  
  # - Run PySpark ETL
  print("--- wd_cluster_fetch_items_M1: Run PySpark ETL (wd_cluster_fetch_items_M1.py)")
  system(command = paste0('sudo -u analytics-privatedata spark2-submit ', 
                          sparkMaster, ' ',
                          sparkDeployMode, ' ',
                          sparkNumExecutors, ' ',
                          sparkDriverMemory, ' ',
                          sparkExecutorMemory, ' ',
                          sparkConfigDynamic, ' ',
                          paste0(fPath, 'wd_cluster_fetch_items_M2.py')
  ),
  wait = T)
  
  # - exit
  print("--- wd_cluster_fetch_items_M2: DONE; Exit.)")
  return('Done.')
  
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
      GET(url = paste0(sparqlEndPointURL, URLencode(query)))
    },
    error = function(condition) {
      message("wd_fetchPropertyConstraints: something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      GET(url = paste0(sparqlEndPointURL, URLencode(query)))
    },
    warning = function(condition) {
      message("wd_fetchPropertyConstraints: something's wrong on WDQS: wait 10 secs, try again.")
      Sys.sleep(10)
      GET(url = paste0(sparqlEndPointURL, URLencode(query)))
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
  rc <- fromJSON(rc, simplifyDataFrame = T)
  rc <- rc$results$bindings
  rc <- jsonlite::flatten(rc)
  
  # - output
  return(rc)
  
}


### --- Function: wd_api_fetch_labels()
# - fetch item labels in batches 
# - (max values = 50, MediaWiki API constraint)
wd_api_fetch_labels <- function(items, language, fallback) {
  
  # - params:
  # - items - character vector of Wikidata identifiers
  # - language - character, ISO 639-1 two-letter language code
  # - fallback - to use or not to use the Wikidata language fallback 
  
  # - API prefix    
  APIprefix <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&'
  
  # - enforce item uniqueness
  items <- unique(items)
  # - iLabs: store batches
  iLabs <- list()
  
  # fetch items
  # - counter
  c <- 0
  # - batch start
  ixStart <- 1
  repeat {
    ixEnd <- ixStart + 50 - 1
    searchItems <- items[ixStart:ixEnd]
    w <- which(is.na(searchItems))
    if (length(w) > 0) {searchItems <- searchItems[-w]}
    ids <- paste(searchItems, collapse = "|")
    if (fallback == T) {
      query <- paste0(APIprefix, 
                      'ids=', ids, '&',
                      'props=labels&languages=', 
                      language, 
                      '&languagefallback=&sitefilter=wikidatawiki&format=json')
    } else {
      query <- paste0(APIprefix, 
                      'ids=', ids, '&',
                      'props=labels&languages=', 
                      language, 
                      '&sitefilter=wikidatawiki&format=json')
    }
    res <- tryCatch(
      {
        GET(url = URLencode(query))
      },
      error = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      },
      warning = function(condition) {
        Sys.sleep(10)
        GET(url = URLencode(query))
      }
    )
    rclabs <- rawToChar(res$content)
    rclabs <- fromJSON(rclabs)
    itemLabels <- unlist(lapply(rclabs$entities, function(x) {
      if (length(x$labels) > 0) {
       return(x$labels[[1]]$value) 
      } else {
        return("")
      }
    }))
    itemLabels <- data.frame(title = names(itemLabels), 
                             en_label = itemLabels, 
                             stringsAsFactors = F, 
                             row.names = c())
    c <- c + 1
    iLabs[[c]] <- itemLabels
    if (length(searchItems) < 50) {
      break
    } else {
      ixStart <- ixStart + 50
      # - pause here 1 sec
      Sys.sleep(1)
    }
  }
  iLabs <- rbindlist(iLabs)
  iLabs <- as.data.frame(iLabs)
  iLabs$en_label[nchar(iLabs$en_label) == 0] <- 'No label defined'
  return(iLabs)
}

  
### --- wd_Superclasses_Recurrently
# - fetch all superclasses (inverse P279 search) 
# - or parts (inverse P361 search)
# - up to the P279 constraint target: Q35120 (entity)
wd_Superclasses_Recurrently <- function(entity, 
                                        language = 'en', 
                                        cleanup = T,
                                        fetchSubClasses = T,
                                        fetchCounts = T,
                                        SPARQL_Endpoint = 'https://query.wikidata.org/bigdata/namespace/wdq/sparql?query=',
                                        verbose = F) {
  
  # - to store results
  results <- vector(length(entity), mode = "list")
  # - to store the function output
  output <- list()
  
  # - fetch entity labels
  entityLab <- character(length(entity))
  for (i in 1:length(entity)) {
    
    if (verbose) {
      message(paste0("Fetching labels......", round(i/length(entity)*100, 2), "%"))
    }
    
    # - construct SPARQL query
    query <- paste0('SELECT * WHERE { wd:', entity[i], ' rdfs:label ?label . FILTER (langMatches( lang(?label), "',
                    language, '" ))} LIMIT 1')
    # - run Query
    repeat {
      res <- tryCatch({
        GET(paste0(SPARQL_Endpoint, URLencode(query)))
      }, 
      error = function(condition) {
        return(FALSE)
      }, 
      warning = function(condition) {
        return(FALSE)
      })
      if (class(res) != 'logical') {
        break
      }
    }
    # - fromJSON
    res <- fromJSON(rawToChar(res$content))
    if (!is.null(res$results$bindings$label$value)) {
      entityLab[i] <- res$results$bindings$label$value
    } else {
      entityLab[i] <- entity[i]
    }
  }
  
  # - fetch entity P279, P361, and P31 superclasses
  fresults <- vector(mode = "list", length = length(entity))
  for (i in 1:length(entity)) {
    
    results <- vector(mode = "list", length = 6)
    if (verbose) {
      message(paste0("Fetching super-classes......", round(i/length(entity)*100, 2), "%"))
    }
    # - compose SPARQL query1: P279 SubclassOf
    query1 <- paste0(
      'SELECT ?item ?itemLabel ?Superclass ?SuperclassLabel ', 
      'WHERE { wd:', entity[i] ,' ((wdt:P279|wdt:P31|wdt:P361)/((wdt:P279|wdt:P31|wdt:P361)*))|(wdt:P279)|(wdt:P31)|(wdt:P361) ?item . ?item wdt:P279 ?Superclass .
      SERVICE wikibase:label { bd:serviceParam wikibase:language "', language, '". }}'
    )
    # - run query 1
    repeat {
      res <- tryCatch({
        GET(paste0(SPARQL_Endpoint, URLencode(query1)))
      }, 
      error = function(condition) {
        return(FALSE)
      }, 
      warning = function(condition) {
        return(FALSE)
      })
      if (class(res) != 'logical') {
        break
      }
    }    # - fromJSON
    results[[1]] <- fromJSON(rawToChar(res$content))
    if (length(results[[1]]$results$bindings) == 0) {
      results[[1]] <- NULL
    } else {
      results[[1]] <- data.frame(item = results[[1]]$results$bindings$item$value,
                                 itemLabel = results[[1]]$results$bindings$itemLabel$value,
                                 superClass = results[[1]]$results$bindings$Superclass$value,
                                 superClassLabel = results[[1]]$results$bindings$SuperclassLabel$value,
                                 stringsAsFactors = F)
      results[[1]]$relation <- rep('P279', dim(results[[1]])[1])
    }
    
    # - compose SPARQL query2: P31 InstanceOf
    query2 <- paste0(
      'SELECT ?item ?itemLabel ?Superclass ?SuperclassLabel ', 
      'WHERE { wd:', entity[i] ,' ((wdt:P279|wdt:P31|wdt:P361)/((wdt:P279|wdt:P31|wdt:P361)*))|(wdt:P279)|(wdt:P31)|(wdt:P361) ?item . ?item wdt:P31 ?Superclass .
      SERVICE wikibase:label { bd:serviceParam wikibase:language "', language, '". }}'
    )
    # - run query 2
    repeat {
      res <- tryCatch({
        GET(paste0(SPARQL_Endpoint, URLencode(query2)))
      }, 
      error = function(condition) {
        return(FALSE)
      }, 
      warning = function(condition) {
        return(FALSE)
      })
      if (class(res) != 'logical') {
        break
      }
    }    # - fromJSON
    results[[2]] <- fromJSON(rawToChar(res$content))
    if (length(results[[2]]$results$bindings) == 0) {
      results[[2]] <- NULL
    } else {
      results[[2]] <- data.frame(item = results[[2]]$results$bindings$item$value,
                                 itemLabel = results[[2]]$results$bindings$itemLabel$value,
                                 superClass = results[[2]]$results$bindings$Superclass$value,
                                 superClassLabel = results[[2]]$results$bindings$SuperclassLabel$value,
                                 stringsAsFactors = F)
      results[[2]]$relation <- rep('P31', dim(results[[2]])[1])
    }
    
    # - compose SPARQL query3: P361 PartOf
    query3 <- paste0(
      'SELECT ?item ?itemLabel ?PartOf ?PartOfLabel ', 
      'WHERE { wd:', entity[i] ,' ((wdt:P279|wdt:P31|wdt:P361)/((wdt:P279|wdt:P31|wdt:P361)*))|(wdt:P279)|(wdt:P31)|(wdt:P361) ?item . ?item wdt:P361 ?PartOf .
      SERVICE wikibase:label { bd:serviceParam wikibase:language "', language, '". }}'
    )
    # - run query 3
    repeat {
      res <- tryCatch({
        GET(paste0(SPARQL_Endpoint, URLencode(query3)))
      }, 
      error = function(condition) {
        return(FALSE)
      }, 
      warning = function(condition) {
        return(FALSE)
      })
      if (class(res) != 'logical') {
        break
      }
    }    # - fromJSON
    results[[3]] <- fromJSON(rawToChar(res$content))
    if (length(results[[3]]$results$bindings) == 0) {
      results[[3]] <- NULL
    } else {
      results[[3]] <- data.frame(item = results[[3]]$results$bindings$item$value,
                                 itemLabel = results[[3]]$results$bindings$itemLabel$value,
                                 superClass = results[[3]]$results$bindings$PartOf$value,
                                 superClassLabel = results[[3]]$results$bindings$PartOfLabel$value,
                                 stringsAsFactors = F)
      results[[3]]$relation <- rep('P361', dim(results[[3]])[1])
    }
    
    # - query to fetch immediate P31 superclasses:
    # - compose SPARQL query
    query4 <- paste0(
      'SELECT ?Superclass ?SuperclassLabel ', 
      'WHERE { wd:', entity[i] ,' wdt:P31 ?Superclass .
      SERVICE wikibase:label { bd:serviceParam wikibase:language "', language, '". }}'
    )
    # - run Query
    repeat {
      res <- tryCatch({
        GET(paste0(SPARQL_Endpoint, URLencode(query4)))
      }, 
      error = function(condition) {
        return(FALSE)
      }, 
      warning = function(condition) {
        return(FALSE)
      })
      if (class(res) != 'logical') {
        break
      }
    }
    # - fromJSON
    results[[4]] <- fromJSON(rawToChar(res$content))
    if (length(results[[4]]$results$bindings) == 0) {
      results[[4]] <- NULL
    } else {
      results[[4]] <- results[[4]]$results$bindings
      results[[4]] <- data.frame(item = paste0('http://www.wikidata.org/entity/', rep(entity[i], dim(results[[4]])[1])),
                                 itemLabel = rep(entityLab[i], dim(results[[4]])[1]),
                                 superClass = results[[4]]$Superclass$value,
                                 superClassLabel = results[[4]]$SuperclassLabel$value,
                                 relation = 'P31',
                                 stringsAsFactors = F)
    }
    
    # - query to fetch immediate P279 superclasses:
    # - compose SPARQL query
    query5 <- paste0(
      'SELECT ?Superclass ?SuperclassLabel ', 
      'WHERE { wd:', entity[i] ,' wdt:P279 ?Superclass .
      SERVICE wikibase:label { bd:serviceParam wikibase:language "', language, '". }}'
    )
    # - run Query
    repeat {
      res <- tryCatch({
        GET(paste0(SPARQL_Endpoint, URLencode(query5)))
      }, 
      error = function(condition) {
        return(FALSE)
      }, 
      warning = function(condition) {
        return(FALSE)
      })
      if (class(res) != 'logical') {
        break
      }
    }    # - fromJSON
    results[[5]] <- fromJSON(rawToChar(res$content))
    if (length(results[[5]]$results$bindings) == 0) {
      results[[5]] <- NULL
    } else {
      results[[5]] <- results[[5]]$results$bindings
      results[[5]] <- data.frame(item = paste0('http://www.wikidata.org/entity/', rep(entity[i], dim(results[[5]])[1])),
                                 itemLabel = rep(entityLab[i], dim(results[[5]])[1]),
                                 superClass = results[[5]]$Superclass$value,
                                 superClassLabel = results[[5]]$SuperclassLabel$value,
                                 relation = 'P279',
                                 stringsAsFactors = F)
    }
    
    # - query to fetch immediate P361 superclasses:
    # - compose SPARQL query
    query6 <- paste0(
      'SELECT ?PartOf ?PartOfLabel ', 
      'WHERE { wd:', entity[i] ,' wdt:P361 ?PartOf .
      SERVICE wikibase:label { bd:serviceParam wikibase:language "', language, '". }}'
    )
    # - run Query
    repeat {
      res <- tryCatch({
        GET(paste0(SPARQL_Endpoint, URLencode(query6)))
      }, 
      error = function(condition) {
        return(FALSE)
      }, 
      warning = function(condition) {
        return(FALSE)
      })
      if (class(res) != 'logical') {
        break
      }
    }    # - fromJSON
    results[[6]] <- fromJSON(rawToChar(res$content))
    if (length(results[[6]]$results$bindings) == 0) {
      results[[6]] <- NULL
    } else {
      results[[6]] <- results[[6]]$results$bindings
      results[[6]] <- data.frame(item = paste0('http://www.wikidata.org/entity/', rep(entity[i], dim(results[[6]])[1])),
                                 itemLabel = rep(entityLab[i], dim(results[[6]])[1]),
                                 superClass = results[[6]]$PartOf$value,
                                 superClassLabel = results[[6]]$PartOfLabel$value,
                                 relation = 'P361',
                                 stringsAsFactors = F)
    }
    
    # - rbindlist results
    fresults[[i]] <- as.data.frame(rbindlist(results, fill = T, use.names = T))
    fresults[[i]] <- fresults[[i]][!duplicated(fresults[[i]]), ]
    
    # - cleanup
    if (cleanup) {
      fresults[[i]]$item <- gsub('http://www.wikidata.org/entity/', '', fresults[[i]]$item)
      fresults[[i]]$superClass <- gsub('http://www.wikidata.org/entity/', '', fresults[[i]]$superClass)
    }
  }
  
  # - rbindlist()
  results <- rbindlist(fresults, fill = T, use.names = T)
  results$itemLabel <- tolower(results$itemLabel)
  results$superClassLabel <- tolower(results$superClassLabel)
  results <- results[!duplicated(results), ]
  # - arrange()
  results <- arrange(results, item)
  output$structure <- results
  output$entity <- entity
  
  # - fetch all immediate subclasses of the classes under consideration
  if (fetchSubClasses) {
    
    classes <- c(unique(output$structure$item), unique(output$structure$superClass))
    imSubClass <- vector(length(classes), mode = "list")
    for (i in 1:length(classes)) {
      
      if (verbose) {
        message(paste0("Fetching sub-classes......", round(i/length(classes)*100, 2), "%"))
      }
      # - compose SPARQL query
      query <- paste0(
        'SELECT ?subClass ?subClassLabel ', 
        "WHERE { ?subClass wdt:P279 wd:" , classes[i], " . ",
        "SERVICE wikibase:label { bd:serviceParam wikibase:language '", language, "'. }}"
      )
      
      # - run Query
      repeat {
        res <- tryCatch({
          GET(paste0(SPARQL_Endpoint, URLencode(query)))
        }, 
        error = function(condition) {
          return(FALSE)
        }, 
        warning = function(condition) {
          return(FALSE)
        })
        if (class(res) != 'logical') {
          break
        }
      }      
      # - fromJSON
      sClass <- fromJSON(rawToChar(res$content))$results$bindings
      
      # - data.frame:
      if (class(sClass) == "data.frame") {
        iLabel <- output$structure$itemLabel[which(output$structure$item %in% classes[i])[1]]
        imSubClass[[i]] <- data.frame(item = rep(classes[i], dim(sClass)[1]),
                                      itemLabel = rep(iLabel, dim(sClass)[1]),
                                      subClass = sClass$subClass$value,
                                      subClassLabel = sClass$subClassLabel$value,
                                      stringsAsFactors = F
                                      
                                      
        )
        
        # - cleanup
        if (cleanup) {
          imSubClass[[i]]$item <- gsub('http://www.wikidata.org/entity/', '', imSubClass[[i]]$item)
        }
        
      } else {
        imSubClass[[i]] <- NULL
      }
      
    }
    
    # - merge imSubClass to output
    # - rbindlist() imSubClass first
    imSubClass <- rbindlist(imSubClass, fill = T, use.names = T)
    imSubClass$itemLabel <- tolower(imSubClass$itemLabel)
    imSubClass$subClassLabel <- tolower(imSubClass$subClassLabel)
    # - arrange()
    imSubClass <- arrange(imSubClass, item)
    imSubClass <- imSubClass[!duplicated(imSubClass), ]
    output$subClasses <- imSubClass
    rm(imSubClass)
    
  }
  
  # - fetch item counts for all classes under consideration
  if (fetchCounts) {
    
    classes <- c(unique(output$structure$item), unique(output$structure$superClass))
    classesCount <- vector(length(classes), mode = "list")
    for (i in 1:length(classes)) {
      
      if (verbose) {
        message(paste0("Fetching counts......", round(i/length(classes)*100, 2), "%... ", classes[i]))
      }
      # - compose SPARQL query to fetch COUNT(?subClass)
      query1 <- paste0(
        'SELECT (COUNT(?subClass) AS ?subClassCount) ',  
        "WHERE { ?subClass wdt:P279 wd:" , classes[i], " . }"
      )
      
      # - run Query
      repeat {
        res1 <- tryCatch({
          GET(paste0(SPARQL_Endpoint, URLencode(query1)))
        }, 
        error = function(condition) {
          return(FALSE)
        }, 
        warning = function(condition) {
          return(FALSE)
        })
        if (class(res1) != 'logical') {
          break
        }
      }      
      # - compose SPARQL query to fetch COUNT(?item)
      query2 <- paste0(
        "SELECT (COUNT(?item) AS ?itemCount)  WHERE {?item wdt:P31 wd:" , classes[i], " . }"
      )
      
      # - run Query 2
      repeat {
        res2 <- tryCatch({
          GET(paste0(SPARQL_Endpoint, URLencode(query2)))
        }, 
        error = function(condition) {
          return(FALSE)
        }, 
        warning = function(condition) {
          return(FALSE)
        })
        if (class(res2) != 'logical') {
          break
        }
      }      
      # - fromJSON
      counts1 <- fromJSON(rawToChar(res1$content))$results$bindings
      counts2 <- fromJSON(rawToChar(res2$content))$results$bindings
      
      # - data.frame:
      iLabel <- output$structure$itemLabel[which(output$structure$item %in% classes[i])[1]]
      classesCount[[i]] <- data.frame(item = classes[i], 
                                      itemLabel = iLabel,
                                      numSubClass = ifelse(class(counts1) == "data.frame", counts1$subClassCount$value, 0),
                                      numItems = ifelse(class(counts2) == "data.frame", counts2$itemCount$value, 0),
                                      stringsAsFactors = F
      )
      
    }
    
    # - merge w. output
    classesCount <- rbindlist(classesCount, fill = T, use.names = T)
    output$counts <- classesCount
    rm(classesCount)
    
  }
  
  # - return
  return(output)
  
}