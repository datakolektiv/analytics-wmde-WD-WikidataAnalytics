
### ---------------------------------------------------------------------------
### --- wdcmModule_Statements_ETL.py
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- January 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- Pyspark ETL procedures for the WD JSON dumps in hdfs
### --- to extract the data sets for the WDCM Statements Dashboard
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
from pyspark.sql.functions import rank, col, \
   explode, regexp_extract, size
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
parsFile = \
   "/home/goransm/Analytics/WDCM/WDCM_Scripts/wdcmConfig.xml"
# - parse wdcmConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))
### --- dir structure and params
# - HDFS dir
hdfsDir = params['statements_hdfsDir']
etlDir = params['statements_etlDir']

### --- Init Spark
# - Spark Session
sc = SparkSession\
    .builder\
    .appName("Wikidata Concepts Monitor Statements ETL")\
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

### ---------------------------------------------------------------------------
### --- 1. Item usage in Wikidata statements
### ---------------------------------------------------------------------------

### --- Access WD dump
WD_dump = sqlContext.sql('SELECT claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
### --- Cache WD dump
WD_dump.cache()
### --- Explode mainSnak for properties
WD_dump = WD_dump\
   .select(explode('claims')\
   .alias('claims'))\
   .select('claims.mainSnak')
WD_dump = WD_dump.select('mainSnak.property', 'mainSnak.dataValue.value')
WD_dump = WD_dump.filter((WD_dump["value"].rlike('"entity-type":"item"')))
WD_dump = WD_dump\
   .select('property', regexp_extract(col('value'), '(Q\d+)', 1).alias('value'))
WD_dump = WD_dump\
   .select('value')\
   .groupBy('value')\
   .count()\
   .orderBy('count', ascending = False)
WD_dump = WD_dump.withColumnRenamed('value', 'item')
WD_dump.write.format('csv')\
   .mode("overwrite")\
   .save(hdfsDir + 'wd_statements_item_usage')

### ---------------------------------------------------------------------------
### --- 2. Property use in Wikidata
### ---------------------------------------------------------------------------

### --- Access WD dump
WD_dump = sqlContext.sql('SELECT claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
### --- Cache WD dump
WD_dump.cache()
### --- Explode mainSnak for properties
WD_dump = WD_dump.select(explode('claims').alias('claims'))\
   .select('claims.mainSnak')
WD_dump = WD_dump.select('mainSnak.property')
WD_dump = WD_dump.select('property')\
   .groupBy('property')\
   .count()\
   .orderBy('count', ascending = False)
WD_dump.coalesce(10)\
   .write.format('csv')\
   .mode("overwrite")\
   .save(hdfsDir + 'wd_statements_property_usage')

### ---------------------------------------------------------------------------
### --- 3. Number of references per  Wikidata Property
### ---------------------------------------------------------------------------

WD_dump = sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
WD_dump.cache()
WD_dump = WD_dump.select('id', explode('claims').alias('claims'))
WD_dump = WD_dump\
   .withColumn('numReferences', size(WD_dump.claims.references))
WD_dump = WD_dump.filter(WD_dump.numReferences > 0)
WD_dump = WD_dump.select('id', 'claims.mainSnak.property', 'numReferences')
WD_dump = WD_dump.select('property', 'numReferences')\
   .groupBy('property')\
   .sum('numReferences')
WD_dump = WD_dump\
   .withColumnRenamed('sum(numReferences)', 'numReferences')\
   .orderBy('numReferences', ascending = False)
WD_dump.coalesce(10).write.format('csv')\
   .mode("overwrite")\
   .save(hdfsDir + 'wd_statements_num_ref_per_property')

### ---------------------------------------------------------------------------
### --- 4. Properties used in references in  Wikidata
### ---------------------------------------------------------------------------

WD_dump = sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
WD_dump.cache()
WD_dump = WD_dump\
   .select('id', explode('claims').alias('claims'))
WD_dump = WD_dump\
   .select('id', 'claims.mainSnak.property', 'claims.references.snaks')
WD_dump = WD_dump\
   .select('id', 'property', explode('snaks').alias('snaks'))
WD_dump = WD_dump\
   .select('id', 'property', explode('snaks.property').alias('snakProperty'))
WD_dump = WD_dump\
   .select('snakProperty')\
   .withColumnRenamed('snakProperty', 'property')\
   .groupBy('property')\
   .count()\
   .orderBy('count', ascending = False)
WD_dump.coalesce(10)\
   .write.format('csv')\
   .mode("overwrite")\
   .save(hdfsDir + 'wd_statements_properties_used_in_references')

### ---------------------------------------------------------------------------
### --- 5. Properties used in qualifiers in  Wikidata
### ---------------------------------------------------------------------------

WD_dump = sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + \
   wikidataEntitySnapshot + '"')
WD_dump.cache()
WD_dump = WD_dump.select('id', explode('claims').alias('claims'))
WD_dump = WD_dump.select('id', 'claims.mainSnak.property', \
   explode('claims.qualifiers.property').alias('qualifier_propery'))
WD_dump = WD_dump.select('qualifier_propery')\
   .withColumnRenamed('qualifier_propery', 'property')\
   .groupBy('property')\
   .count()\
   .orderBy('count', ascending = False)
WD_dump.coalesce(10).write.format('csv')\
   .mode("overwrite")\
   .save(hdfsDir + 'wd_statements_properties_used_in_qualifiers')
