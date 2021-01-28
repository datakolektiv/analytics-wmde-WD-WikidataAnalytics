
### ---------------------------------------------------------------------------
### --- wdcmModule_Biases_ETL.py
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- April 2019.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- Pyspark ETL procedures for the WD JSON dumps in hdfs
### --- to extract the data sets for the WDCM Biases Dashboard
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of the Wikidata Concepts Monitor (WDCM)
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
### --- Script: wdcmModule_Biases_ETL.py
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- wdcmModule_Biases_ETL.py performs ETL procedures
### --- over the Wikidata JSON dumps in hdfs.
### ---------------------------------------------------------------------------

### --- Modules
import pyspark
from pyspark.sql import SparkSession
from pyspark.sql.window import Window
from pyspark.sql.functions import rank, col, explode, regexp_extract
from pyspark import SparkFiles
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


### --- parse WDCM parameters: wdcmConfig.xml
parsFile = "/home/goransm/Analytics/WDCM/WDCM_Scripts/wdcmConfig.xml"
# - parse wdcmConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))
### --- dir structure and params
# - HDFS dir
hdfsDir = params['biases_hdfsDir']

### --- Init Spark
# - Spark Session
sc = SparkSession\
    .builder\
    .appName("Wikidata Concepts Monitor Biases ETL")\
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

### ---------------------------------------------------------------------------
### --- Import WD JSON dump from hdfs
### ---------------------------------------------------------------------------

### --- Access WD dump
WD_dump = sqlContext.sql('SELECT id, claims.mainSnak FROM wmf.wikidata_entity WHERE snapshot="' + wikidataEntitySnapshot + '"')
### --- Cache WD dump
WD_dump.cache()
### --- Explode mainSnak for properties
WD_dump = WD_dump.withColumn('mainSnak', explode('mainSnak'))

### --- Extract all items w. P21 (sex or gender)
### --- Explode claims & select
items_P21 = WD_dump.select('id', col("mainSnak.property").alias("propertyP21"), \
                         col('mainSnak.dataValue').alias("dataValue"))
items_P21 = items_P21.select('id', 'propertyP21', \
                         col('dataValue.value').alias("valueP21"))
items_P21 = items_P21.where(col('propertyP21').isin("P21"))
items_P21 = items_P21.select('id', \
                            regexp_extract('valueP21', '"id":"(Q\d+)"', 1).alias("valueP21"))

### --- Extract all items w. P19 (place of birth)
### --- Explode claims & select
items_P19 = WD_dump.select('id', col("mainSnak.property").alias("propertyP19"), \
                         col('mainSnak.dataValue').alias("dataValue"))
items_P19 = items_P19.select('id', 'propertyP19', \
                         col('dataValue.value').alias("valueP19"))
items_P19 = items_P19.where(col('propertyP19').isin("P19"))
items_P19 = items_P19.select('id', \
                            regexp_extract('valueP19', '"id":"(Q\d+)"', 1).alias("valueP19"))

### --- left join: items_P21 <- items_P19
items = items_P21.join(items_P19, ["id"], how='left')
del(items_P21)
del(items_P19)

### --- Extract all items w. P106 (occupation)
### --- Explode claims & select
items_P106 = WD_dump.select('id', col("mainSnak.property").alias("propertyP106"), \
                         col('mainSnak.dataValue').alias("dataValue"))
items_P106 = items_P106.select('id', 'propertyP106', \
                         col('dataValue.value').alias("valueP106"))
items_P106 = items_P106.where(col('propertyP106').isin("P106"))
items_P106 = items_P106.select('id', \
                            regexp_extract('valueP106', '"id":"(Q\d+)"', 1).alias("valueP106"))

### --- left join: items <- items_P106
items = items.join(items_P106, ["id"], how='left')
del(items_P106)

### --- Extract all geo-coded items to join w. P19 place of birth
### --- Explode claims & select
geo_items = WD_dump.select('id', col("mainSnak.property").alias("property"), \
                         col('mainSnak.dataValue').alias("dataValue"))
geo_items = geo_items.where(col('property').isin("P625"))
geo_items = geo_items.select('id', 'dataValue.value')
geo_items = geo_items.select('id', \
                             regexp_extract('value', '"latitude":(-*\d*\.*\d*)', 1).alias("latitude"), \
                             regexp_extract('value', '"longitude":(-*\d*\.*\d*)', 1).alias("longitude"))

### --- left join: items <- geo_items
items = items.join(geo_items.withColumnRenamed("id", "valueP19"), \
                   ["valueP19"], how='left')
del(geo_items)

### ---------------------------------------------------------------------------
### --- Add WDCM usage statistics
### ---------------------------------------------------------------------------

# - from wdcm_clients_wb_entity_usage
WDCM_MainTableRaw = sqlContext.sql('SELECT eu_entity_id AS id, wiki_db AS project, COUNT(*) AS usage FROM \
                                        (SELECT DISTINCT eu_entity_id, eu_page_id, wiki_db \
                                        FROM goransm.wdcm_clients_wb_entity_usage) \
                                        AS t WHERE eu_entity_id RLIKE "^Q" GROUP BY wiki_db, eu_entity_id')

# - cache WDCM_MainTableRaw
WDCM_MainTableRaw.cache()

### --- left join: items <- WDCM_MainTableRaw
items = items.join(WDCM_MainTableRaw, ["id"], how='left')
del(WDCM_MainTableRaw)

### --- repartition and save to hdfs
items = items.repartition(10)
# - save to csv:
items.write.format('csv').mode("overwrite").save(hdfsDir+'WDCM_Biases_ETL')

### --- clear
sc.catalog.clearCache()
