
### ---------------------------------------------------------------------------
### --- wdcmModule_ETL.py
### --- Version 1.0.0
### --- Authors: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE;
### --- Andrew Otto, Senior Systems Engineer — Operations/Analytics,
### --- Wikimedia Foundation (authored: function list_files())
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Pyspark ETL procedures for the WDCM system
### --- NOTE: the execution of this WDCM script is always dependent upon the
### --- previous run of the WDCM_Sqoop_Clients.R script.
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
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
### --- along with WDCM. If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- Script 2: wdcmModule_ETL.py
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- wdcmModule_ETL.py performs ETL procedures
### --- from the goransm.wdcm_clients_wb_entity_usage
### --- table (lives in: Hadoop, WMF Data Lake).
### ---------------------------------------------------------------------------

### --- Modules
import pyspark
from pyspark.sql import SparkSession
from pyspark.sql.window import Window
from pyspark.sql.functions import rank, col, round, explode, row_number, lit
from pyspark import SparkFiles
from pyspark.sql.types import *
import numpy as np
import pandas as pd
import os
import subprocess
import gc
import re
from sys import stdin
import sys
from itertools import compress
import datetime
import xml.etree.ElementTree as ET
import requests
import json


### --- Functions
# - list files to work with hdfs
def list_files(dir):
    if (dir.startswith('hdfs://')):
        # use hdfs -stat %n to just get file names in dir/*
        cmd = ['sudo', '-u', 'analytics-privatedata', 'kerberos-run-command', 'analytics-privatedata', 'hdfs', 'dfs', '-stat', '%n', dir + "/*"]
        files = [str(f, 'utf-8') for f in subprocess.check_output(cmd).split(b"\n")]
    else:
        files = os.listdir(path=dir)
    return [os.path.join(dir, f) for f in files]

### --- start Time
startTime = datetime.datetime.now()
# - to runtime log:
print("WDCM ETL Module start at: " + str(startTime))

### --- parse WDCM parameters: wdcmConfig.xml
parsFile = "/home/goransm/Analytics/WDCM/WDCM_Scripts/wdcmConfig.xml"
# - parse wdcmConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))
### --- dir structure and params
tempDir = params['tempDir']
dataDir = params['etlDir']
dataGeoDir = params['etlDirGeo']
logDir = params['logDir']
publicDir = params['publicDir']
itemsDir = params['itemsDir']
http_proxy = params['http_proxy']
https_proxy = params['https_proxy']
hdfsDir_WDCMCollectedItemsDir = params['hdfsCollectedItemsDir']
hdfsPATH_WDCMCollectedItems = params['hdfsPATH_WDCMCollectedItems']
hdfsPATH_WDCMGeoCollectedItems = params['hdfsPATH_WDCMCollectedGeoItems']
hdfsPATH_WDCM_ETL = params['hdfsPATH_WDCM_ETL']
hdfsPATH_WDCM_ETL_GEO = params['hdfsPATH_WDCM_ETL_GEO']

### --- parse WDCM parameters: wdcmConfig_Deployment.xml
parsFile = "/home/goransm/Analytics/WDCM/WDCM_Scripts/wdcmConfig_Deployment.xml"
# - parse wdcmConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))
### --- ML/ETL parameters
topNpopularWDIitems = params['topNpopularWDIitems']
topNpopularCategoryWDItems = params['topNpopularCategoryWDItems']
NItemsTFMatrix = params['NItemsTFMatrix']

### --- Init Spark

# - Spark Session
sc = SparkSession\
    .builder\
    .appName("Wikidata Concepts Monitor ETL")\
    .enableHiveSupport()\
    .getOrCreate()

# - Spark Session Log Level: INFO
sc.sparkContext.setLogLevel("INFO")

# - SQL Context
sqlContext = pyspark.SQLContext(sc)

### --- get wmf.wikidata_entity snapshot
snaps = sqlContext.sql('SHOW PARTITIONS wmf.wikidata_entity')
snaps = snaps.toPandas()
wikidataEntitySnapshot = snaps.tail(1)['partition'].to_string()
wikidataEntitySnapshot = wikidataEntitySnapshot[-10:]
### --- get wmf.mediawiki_history snapshot
snaps = sqlContext.sql('SHOW PARTITIONS wmf.mediawiki_history')
snaps = snaps.toPandas()
mwwikiSnapshot = snaps.tail(1)['partition'].to_string()
mwwikiSnapshot = mwwikiSnapshot[-7:]

### --- Produce WDCM_MainTableRaw

# - from wdcm_clients_wb_entity_usage
WDCM_MainTableRaw = sqlContext.sql('SELECT eu_entity_id, wiki_db AS eu_project, COUNT(*) AS eu_count FROM \
                                        (SELECT DISTINCT eu_entity_id, eu_page_id, wiki_db \
                                        FROM goransm.wdcm_clients_wb_entity_usage) \
                                        AS t WHERE eu_entity_id RLIKE "^Q" GROUP BY wiki_db, eu_entity_id')
# - cache WDCM_MainTableRaw
WDCM_MainTableRaw.cache()

### --- Produce WDCM_labels: English
### --- Access wmf.wikidata_entity (copy of the Wikidata JSON dump) for labels
WDCM_labels = sqlContext.sql('SELECT id, labels FROM wmf.wikidata_entity WHERE snapshot="' + wikidataEntitySnapshot + '"')
### --- Cache WDCM_labels
WDCM_labels.cache()
### --- Explode labels & select English
WDCM_labels = WDCM_labels.select('id', explode('labels').alias("language", "label"))
WDCM_labels = WDCM_labels.where(col('language').isin("en"))
WDCM_labels = WDCM_labels.select('id', 'label')
WDCM_labels = WDCM_labels.withColumnRenamed("id", "eu_entity_id").withColumnRenamed("label", "eu_label")

### --- Join en labels to WDCM_MainTableRaw
WDCM_MainTableRaw = WDCM_MainTableRaw.join(WDCM_labels, ["eu_entity_id"], how='left')

# - create View from WDCM_MainTableRaw
WDCM_MainTableRaw.createTempView("wdcmmain")

### --- Produce WDCM Analytical Tables

### - produce: wdcm_topItems
# - description: top 100,000 most popular Wikidata items
wdcm_item = sqlContext.sql('SELECT eu_entity_id, SUM(eu_count) AS eu_count FROM wdcmmain GROUP BY eu_entity_id \
                                    ORDER BY eu_count DESC LIMIT ' + topNpopularWDIitems)
fileName = "wdcm_topItems.csv"
wdcm_item.cache().coalesce(1).toPandas().to_csv(dataDir + fileName, header=True, index=False)

### - produce: wdcm_project
# - description: total item usage as sum(eu_count) per project
wdcm_project = sqlContext.sql('SELECT eu_project, SUM(eu_count) AS eu_count FROM wdcmmain GROUP BY eu_project \
                                    ORDER BY eu_count DESC')
fileName = "wdcm_project.csv"
wdcm_project.cache().coalesce(1).toPandas().to_csv(dataDir + fileName, header=True, index=False)

### - produce: wdcm_project_item100
# - description: total item usage as sum(eu_count) per project
wdcm_project_item100 = WDCM_MainTableRaw.\
    select(col('eu_project'), col('eu_entity_id'), col('eu_count'), col('eu_label'))\
    .filter(col('eu_count') > 1)
# - coalesce, filter, to Pandas, save
wdcm_project_item100 = wdcm_project_item100.coalesce(1).toPandas()
wdcm_project_item100 = wdcm_project_item100.sort_values('eu_count', ascending=False).groupby('eu_project').head(100)
wdcm_project_item100.sort_values(['eu_project', 'eu_entity_id'], ascending=[True, True])
fileName = "wdcm_project_item100.csv"
wdcm_project_item100.to_csv(dataDir + fileName, header=True, index=False)

### --- wdcm analytical tables PER CATEGORY
itemFiles = list_files(hdfsPATH_WDCMCollectedItems)
itemFiles = itemFiles[0:-1]
for itemFile in itemFiles:

    # category name:
    catName = itemFile.split("/")[len(itemFile.split("/")) - 1]
    catName = catName.split(".")[0]
    catName = catName.split("-")[0:-1]
    catName = " ".join(catName)

    # read itemFile: list of items in category
    items = sqlContext.read.csv(itemFile, header=True)
    # - items: rename columns and cache
    items.toDF('item', 'category').cache()
    # - create View from WDCM_MainTableRaw
    items.createTempView("itemview")
    # - left join items <- wdcmmain
    items = sqlContext.sql('SELECT itemview.item AS eu_entity_id, itemview.category AS category,\
                            wdcmmain.eu_label AS eu_label, wdcmmain.eu_project AS eu_project,\
                            wdcmmain.eu_count AS  eu_count\
                            FROM itemview \
                            LEFT JOIN wdcmmain \
                            ON itemview.item = wdcmmain.eu_entity_id')

    # - produce: wdcm_item_category_
    # - description: total usage of the top topNpopularCategoryWDItems entities usage from a category,
    # - per entity, one file per category
    filename = "wdcm_category_item_" + catName + '.csv'
    filename = dataDir + filename
    wdcm_item_category = items.select(col('eu_entity_id'), col('eu_count'), col('eu_label')) \
        .groupBy("eu_entity_id", "eu_label").sum("eu_count") \
        .withColumnRenamed("sum(eu_count)", "eu_count")
    wdcm_item_category = wdcm_item_category.orderBy(wdcm_item_category['eu_count'].desc())
    # - save a version of wdcm_item_category limited to topNpopularCategoryWDItems:
    wdcm_item_category_save = wdcm_item_category.limit(int(topNpopularCategoryWDItems))
    # - toPandas, save locally:
    wdcm_item_category_save.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
    # - delete wdcm_item_category_save
    del wdcm_item_category_save

    # - Term-Frequency Matrix
    # - filename
    filename = "tfMatrix_" + catName + '.csv'
    filename = dataDir + filename
    # - keep only NItemsTFMatrix items
    topItems = wdcm_item_category.limit(int(NItemsTFMatrix)).select('eu_entity_id').collect()
    topItems = [row.eu_entity_id for row in topItems]
    # - filter from WDCM_MainTableRaw:
    tf_category = WDCM_MainTableRaw.where(WDCM_MainTableRaw.eu_entity_id.isin(topItems))
    # - toPandas, save locally:
    tf_category.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
    # - delete tf_category
    del tf_category
    # delete wdcm_item_category
    del wdcm_item_category

    # - produce: wdcm_category_sum_*
    # - description: sum(eu_count) for the category, one file per category
    filename = "wdcm_category_sum_" + catName + '.csv'
    wdcm_category = items.select(col('category'), col('eu_count')) \
        .groupBy("category").sum("eu_count").withColumnRenamed("sum(eu_count)", "eu_count")
    # - filename:
    filename = dataDir + filename
    # - toPandas, save locally:
    wdcm_category.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
    # delete wdcm_category
    del wdcm_category

    # - produce: wdcm_project_category
    # - description: sum(eu_count) aggregated per project, one file per category
    filename = "wdcm_project_category_" + catName + '.csv'
    wdcm_project_category = items.select(col('category'), col('eu_project'), col('eu_count')) \
        .groupBy("category", "eu_project").sum("eu_count") \
        .withColumnRenamed("sum(eu_count)", "eu_count").orderBy(['eu_count'], ascending=False)
    # - filename:
    filename = dataDir + filename
    # - toPandas, save locally:
    wdcm_project_category.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
    # delete wdcm_project_category
    del wdcm_project_category

    # - produce: wdcm_project_category_item100
    # - description: top 100 WD items per project and per category, one file per category
    filename = "wdcm_project_category_item100_" + catName + '.csv'
    filename = dataDir + filename
    wdcm_project_category_item100 = items \
        .groupBy(['eu_project', 'category', 'eu_entity_id', 'eu_label']).sum('eu_count') \
        .withColumnRenamed('sum(eu_count)', 'eu_count') \
        .filter(col('eu_count') > 1)
    # - group, sort and limit
    window = Window.partitionBy(wdcm_project_category_item100.eu_project, \
                                wdcm_project_category_item100.category). \
        orderBy(wdcm_project_category_item100.eu_count.desc())
    wdcm_project_category_item100 = wdcm_project_category_item100.select('*', row_number().over(window).alias('rank')) \
        .filter(col('rank') <= 100)
    wdcm_project_category_item100 = wdcm_project_category_item100.drop("rank")
    # - toPandas, save locally:
    wdcm_project_category_item100.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
    # - delete wdcm_project_category_item100
    del wdcm_project_category_item100

    # - unpersist items
    items.unpersist()
    # - detach itemview
    sqlContext.sql("DROP VIEW itemview")


### --- wdcm analytical tables: OTHER category

catName = "Other"
# - produce completeItems
itemFiles = list_files(hdfsPATH_WDCMCollectedItems)
itemFiles = itemFiles[0:-1]
for itemFile in itemFiles:
    # read itemFile: list of items in category
    items = sqlContext.read.csv(itemFile, header=True)
    # - items: rename columns and cache
    items.toDF('item', 'category').cache()
    # - union:
    if itemFile == itemFiles[0]:
        completeItems = items
    else:
        completeItems = completeItems.union(items)

# - Access wmf.wikidata_entity (copy of the Wikidata JSON dump) for items
WD_items = sqlContext.sql('SELECT id FROM wmf.wikidata_entity WHERE snapshot="' + wikidataEntitySnapshot + '"')
# - Cache WD_items
WD_items.cache()
# - Left Anti Join to filter out completeItems
WD_items = WD_items.withColumnRenamed('id', 'item').join(completeItems, on=['item'], how='left_anti')
del completeItems

# - left join WD_items <- wdcmmain
items = WD_items
del WD_items
# - add category column to items
items = items.withColumn('category', lit('Other'))
# - create View from items: itemview
items.createTempView("itemview")
# - left join items <- wdcmmain
items = sqlContext.sql('SELECT itemview.item AS eu_entity_id, itemview.category AS category,\
                        wdcmmain.eu_label AS eu_label, wdcmmain.eu_project AS eu_project,\
                        wdcmmain.eu_count AS  eu_count\
                        FROM itemview \
                        LEFT JOIN wdcmmain \
                        ON itemview.item = wdcmmain.eu_entity_id')
# - produce: wdcm_item_category_
# - description: total usage of the top topNpopularCategoryWDItems entities usage from a category,
# - per entity, one file per category
filename = "wdcm_category_item_" + catName + '.csv'
filename = dataDir + filename
wdcm_item_category = items.select(col('eu_entity_id'), col('eu_count'), col('eu_label')) \
    .groupBy("eu_entity_id", "eu_label").sum("eu_count") \
    .withColumnRenamed("sum(eu_count)", "eu_count")
wdcm_item_category = wdcm_item_category.orderBy(wdcm_item_category['eu_count'].desc())
# - save a version of wdcm_item_category limited to topNpopularCategoryWDItems:
wdcm_item_category_save = wdcm_item_category.limit(int(topNpopularCategoryWDItems))
# - toPandas, save locally:
wdcm_item_category_save.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
# - delete wdcm_item_category_save
del wdcm_item_category_save

# - Term-Frequency Matrix
# - filename
filename = "tfMatrix_" + catName + '.csv'
filename = dataDir + filename
# - keep only NItemsTFMatrix items
topItems = wdcm_item_category.limit(int(NItemsTFMatrix)).select('eu_entity_id').collect()
topItems = [row.eu_entity_id for row in topItems]
# - filter from WDCM_MainTableRaw:
tf_category = WDCM_MainTableRaw.where(WDCM_MainTableRaw.eu_entity_id.isin(topItems))
# - toPandas, save locally:
tf_category.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
# - delete tf_category
del tf_category
# delete wdcm_item_category
del wdcm_item_category

# - produce: wdcm_category_sum_*
# - description: sum(eu_count) for the category, one file per category
filename = "wdcm_category_sum_" + catName + '.csv'
wdcm_category = items.select(col('category'), col('eu_count')) \
    .groupBy("category").sum("eu_count").withColumnRenamed("sum(eu_count)", "eu_count")
# - filename:
filename = dataDir + filename
# - toPandas, save locally:
wdcm_category.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
# delete wdcm_category
del wdcm_category

# - produce: wdcm_project_category
# - description: sum(eu_count) aggregated per project, one file per category
filename = "wdcm_project_category_" + catName + '.csv'
wdcm_project_category = items.select(col('category'), col('eu_project'), col('eu_count')) \
    .groupBy("category", "eu_project").sum("eu_count") \
    .withColumnRenamed("sum(eu_count)", "eu_count").orderBy(['eu_count'], ascending=False)
# - filename:
filename = dataDir + filename
# - toPandas, save locally:
wdcm_project_category.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
# delete wdcm_project_category
del wdcm_project_category

# - produce: wdcm_project_category_item100
# - description: top 100 WD items per project and per category, one file per category
filename = "wdcm_project_category_item100_" + catName + '.csv'
filename = dataDir + filename
wdcm_project_category_item100 = items \
    .groupBy(['eu_project', 'category', 'eu_entity_id', 'eu_label']).sum('eu_count') \
    .withColumnRenamed('sum(eu_count)', 'eu_count') \
    .filter(col('eu_count') > 1)
# - group, sort and limit
window = Window.partitionBy(wdcm_project_category_item100.eu_project, \
                            wdcm_project_category_item100.category). \
    orderBy(wdcm_project_category_item100.eu_count.desc())
wdcm_project_category_item100 = wdcm_project_category_item100.select('*', row_number().over(window).alias('rank')) \
    .filter(col('rank') <= 100)
wdcm_project_category_item100 = wdcm_project_category_item100.drop("rank")
# - toPandas, save locally:
wdcm_project_category_item100.coalesce(1).toPandas().to_csv(filename, header=True, index=False)
# - delete wdcm_project_category_item100
del wdcm_project_category_item100

# - unpersist items
items.unpersist()
# - detach itemview
sqlContext.sql("DROP VIEW itemview")

### --- process GEO items

# - reduce WDCM_MainTableRaw for further processing
WDCM_MainReduced = sqlContext.sql('SELECT eu_entity_id, SUM(eu_count) AS eu_count FROM wdcmmain GROUP BY eu_entity_id')
WDCM_MainReduced.cache()

# - wdcm analytical tables per category
itemFiles = list_files(hdfsPATH_WDCMGeoCollectedItems)
itemFiles = itemFiles[0:-1]
for itemFile in itemFiles:

    # category name:
    catName = itemFile.split("/")[len(itemFile.split("/")) - 1]
    catName = catName.split(".")[0]
    catName = catName.split("-")[0:-1]
    catName = " ".join(catName)

    # read itemFile: list of items in category
    items = sqlContext.read.csv(itemFile, header=True)

    # left.join(newItem, WDCM_MainReduced)
    items = items.withColumnRenamed("item", "eu_entity_id").join(WDCM_MainReduced,
                                                                 ["eu_entity_id"],
                                                                 how='left').na.fill(0)

    # - produce: wdcm_geoitem_ files
    # - description: total usage of the top topNpopularCategoryWDItems entities usage from a category,
    # - per entity, one file per category
    filename = dataGeoDir + "wdcm_geoitem_" + catName + '.csv'
    items = items.sort(col("eu_count").desc()).coalesce(1).cache().toPandas()
    items.to_csv(filename, header=True, index=False)
    # - remove items
    del items

### --- end Time
endTime = datetime.datetime.now()
# - to runtime log:
print("WDCM ETL Module completed at: " + str(endTime))
sdiff = endTime - startTime
print("WDCM ETL Module total runtime: " + str(sdiff.total_seconds() / 60) + " minutes.")
