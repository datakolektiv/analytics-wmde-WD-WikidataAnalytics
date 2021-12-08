
### ---------------------------------------------------------------------------
### --- wd_cluster_fetch_items_M1.py
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE;
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- December 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Pyspark ETL procedures for the WMDE/QURATOR system
### --- M1 type anomaly solver
### ---------------------------------------------------------------------------

### --- Modules
import pyspark
from pyspark.sql import SparkSession
from pyspark.sql.window import Window
from pyspark.sql.functions import rank, col,\
   round, explode, row_number, regexp_extract
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
   "/home/goransm/Analytics/Qurator/CuriousFacts/wd_cluster_fetch_items.xml"
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
tclass = params['class']
rclass = params['referenceClass']
targetProperty = params['targetProperty']

### --- Init Spark
# - Spark Session
sc = SparkSession\
    .builder\
    .appName("WMDE Qurator ETL M1")\
    .enableHiveSupport()\
    .getOrCreate()
# - Spark Session Log Level: ERROR
sc.sparkContext.setLogLevel("ERROR")
# - SQL Context
sqlContext = pyspark.SQLContext(sc)

### --- get wmf.wikidata_entity snapshot
snaps = sqlContext.sql('SHOW PARTITIONS wmf.wikidata_entity')
snaps = snaps.toPandas()
wikidataEntitySnapshot = snaps\
   .tail(1)['partition']\
   .to_string()
wikidataEntitySnapshot = wikidataEntitySnapshot[-10:]

### --- Section 1. All items belonging to class

# - read subclasses
subclassesFile = hdfsDir +  'subclasses.csv'
subclasses = sqlContext\
   .read.csv(subclassesFile, header=True)
subclasses = subclasses.select('subclass')
# - subclasses to list + add tclass 
subclasses = subclasses.toPandas()
subclasses =  subclasses['subclass']\
   .tolist()
subclasses.append(tclass)
# - unique elements only
subclasses = list(set(subclasses))

# - find what is in classes by P31
# - initiate dump for items
WD_items = sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + \
   '"')
# - cache WD dump for items
WD_items.cache()
# - explode properties
WD_items = WD_items\
   .select('id', explode('claims').alias('claims'))\
   .select('id', 'claims.mainSnak')
WD_items = WD_items\
   .select('id', 'mainSnak.property', 'mainSnak.dataValue.value')
WD_items = WD_items\
   .select('id', 'property', \
   regexp_extract(col('value'), '(Q\d+)', 1)\
   .alias('value'))
# - keep items only and P31 or P279
WD_items = WD_items\
   .filter((WD_items["id"].rlike('Q\d+')) & \
   ((WD_items.property == 'P31') | \
   (WD_items.property == 'P279')))
# - filter by: subclasses list
WD_items = WD_items\
   .filter(WD_items['value'].isin(subclasses))
WD_items = WD_items\
   .select('id', 'value')


### --- Section 2. All selected items + targetProperty values

# - initiate dump for items
WD_dump = \
   sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
# - cache WD dump for items
WD_dump.cache()
# - explode properties
WD_dump = WD_dump\
   .select('id', explode('claims')\
   .alias('claims'))\
   .select('id', 'claims.mainSnak')
WD_dump = WD_dump\
   .select('id', 'mainSnak.property', \
   regexp_extract(col('mainSnak.dataValue.value'), '(Q\d+)', 1)\
   .alias('value'))
# - keep targetProperty only
WD_dump = WD_dump\
   .filter(WD_dump.property == targetProperty)
# - keep items only
WD_dump = WD_dump\
   .filter(WD_dump["id"].rlike('Q\d+'))
# - join to WD_items
WD_dump = WD_dump\
   .withColumnRenamed('value', 'propValue')
WD_items = WD_items\
      .select('id')\
      .join(WD_dump.select('id', 'propValue'), on=['id'], how='left')
WD_items = WD_items\
   .na.drop(subset=['propValue'])

### --- Section 3. All items belonging to referenceClass

# - read referenceClasses
referenceClassFile = hdfsDir +  'refClassSubclasses.csv'
referenceClasses = sqlContext.read.csv(referenceClassFile, \
      header=True)
referenceClasses = referenceClasses\
      .select('subclass')
# - referenceClasses to list + add tclass 
referenceClasses = referenceClasses\
      .toPandas()
referenceClasses =  referenceClasses['subclass']\
      .tolist()
referenceClasses\
      .append(rclass)
# - unique elements only
referenceClasses = list(set(referenceClasses))

# - find what is in classes by P31
# - initiate dump for items
WD_refitems = \
   sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
# - cache WD dump for items
WD_refitems.cache()
# - explode properties
WD_refitems = WD_refitems\
   .select('id', explode('claims').alias('claims'))\
   .select('id', 'claims.mainSnak')
WD_refitems = WD_refitems\
   .select('id', 'mainSnak.property', 'mainSnak.dataValue.value')
WD_refitems = WD_refitems\
   .select('id', 'property', regexp_extract(col('value'), '(Q\d+)', 1)\
   .alias('value'))
# - keep items only and P31 or P279
WD_refitems = WD_refitems\
   .filter((WD_refitems["id"].rlike('Q\d+')) & \
   ((WD_refitems.property == 'P31') | \
   (WD_refitems.property == 'P279')))
# - filter by: referenceClasses list
WD_refitems = WD_refitems\
   .filter(WD_refitems['value'].isin(referenceClasses))
WD_refitems = WD_refitems\
   .select('id', 'value')

### --- Section 4. All WD_items where propValue is not in WD_refitems
WD_refitems = WD_refitems\
   .withColumnRenamed('id', 'propValue')\
   .select('propValue')
WD_items  = WD_items\
   .join(WD_refitems, on=['propValue'], how='left_anti')

### --- Section 5. Store csv to local filesystem
filename = "result_M1_" + wikidataEntitySnapshot + ".csv"
# WD_items.coalesce(1).toPandas().to_csv(dataDir + filename, header=True, index=False)
WD_items\
   .repartition(10)\
   .write.format('csv')\
   .mode("overwrite")\
   .save(hdfsDir + filename)
