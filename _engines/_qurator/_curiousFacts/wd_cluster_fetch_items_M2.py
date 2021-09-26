
### ---------------------------------------------------------------------------
### --- wd_cluster_fetch_items_M2.py
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE;
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- December 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Pyspark ETL procedures for the WMDE/QURATOR system
### --- M2 type anomaly solver
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of WMDE/QURATOR (WMDEQ)
### ---
### --- WMDEQ is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WMDEQ is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WMDEQ. If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Modules
import pyspark
from pyspark.sql import SparkSession
from pyspark.sql.window import Window
from pyspark.sql.functions import rank, col, round, \
   explode, row_number, regexp_extract
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

### --- parse WDCM parameters: wdcmConfig.xml
parsFile = \
   "/home/goransm/Analytics/Qurator/CuriousFacts/wd_cluster_fetch_items_M2.xml"
# - parse wdcmConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))
### --- dir structure and params
http_proxy = params['http_proxy']
https_proxy = params['https_proxy']
hdfsDir = params['hdfsDir']
dataDir = params['dataDir']
targetProperty = params['targetProperty']
referenceProperty = params['referenceProperty']
referenceClasses = params['referenceClasses']

### --- Init Spark
# - Spark Session
sc = SparkSession\
    .builder\
    .appName("WMDE Qurator ETL M2")\
    .enableHiveSupport()\
    .getOrCreate()
# - Spark Session Log Level: INFO
sc.sparkContext.setLogLevel("ERROR")
# - SQL Context
sqlContext = pyspark.SQLContext(sc)

### --- get wmf.wikidata_entity snapshot
snaps = sqlContext.sql('SHOW PARTITIONS wmf.wikidata_entity')
snaps = snaps.toPandas()
wikidataEntitySnapshot = snaps.tail(1)['partition'].to_string()
wikidataEntitySnapshot = wikidataEntitySnapshot[-10:]

### --- Section 1. All items with targetProperty
# - initiate dump for items
WD_dump = \
   sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
# - cache WD dump for items
WD_dump.cache()
# - explode properties
WD_dump = WD_dump\
   .select('id', explode('claims').alias('claims'))\
   .select('id', 'claims.mainSnak')
WD_dump = WD_dump.select('id', 'mainSnak.property')
# - keep targetProperty only
WD_dump = WD_dump\
   .filter(WD_dump.property == targetProperty)
# - keep items only
WD_dump = WD_dump\
   .filter(WD_dump["id"].rlike('Q\d+'))

### --- Section 2. All items with referenceProperty in referenceClasses
# - initiate dump for items
WD_items = \
   sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
# - cache WD dump for items
WD_items.cache()
# - explode properties
WD_items = WD_items\
   .select('id', explode('claims').alias('claims'))\
   .select('id', 'claims.mainSnak')
WD_items = WD_items\
   .select('id', 'mainSnak.property', 'mainSnak.dataValue.value')
# - keep items on 'value' only
WD_items = WD_items\
   .select('id', 'property', \
   regexp_extract(col('value'), '(Q\d+)', 1).alias('value'))
# - keep items on 'id' only and referenceProperty
WD_items = WD_items\
   .filter((WD_items["id"].rlike('Q\d+')) & \
   (WD_items.property == referenceProperty))
# - read subclasses
subclassesFile = hdfsDir +  'refClassSubclasses.csv'
subclasses = sqlContext.read.csv(subclassesFile, header=True)
subclasses = subclasses.select('subclass')
# - subclasses to list 
subclasses = subclasses.toPandas()
subclasses =  subclasses['subclass'].tolist()
# - unique elements only
subclasses = list(set(subclasses))
# - filter by: referenceClasses list
WD_items = WD_items\
   .filter(WD_items['value'].isin(subclasses))
WD_items = WD_items.select('id')

### --- Section 3. All wd_dump that are not in WD_items
WD_dump  = WD_dump\
   .join(WD_items, on=['id'], how='left_anti')

### --- Section 4. Store csv to hdfs
filename = "result_M2_" + wikidataEntitySnapshot + ".csv"
WD_dump\
   .repartition(10)\
   .write.format('csv')\
   .mode("overwrite")\
   .save(hdfsDir + filename)
