### ---------------------------------------------------------------------------
### --- wd_cluster_fetch_items_M3.py
### --- SINGLE VALUE CONSTRAINT VIOLATIONS
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE;
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- December 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Pyspark ETL procedures for the WMDE/QURATOR system
### --- M3 type anomaly solver
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
from pyspark.sql.functions import rank, col, round, explode, explode_outer, row_number, regexp_extract, lit
from pyspark.sql.functions import monotonically_increasing_id 
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

### --- dir structure and params
hdfsDir = 'hdfs:///tmp/wmde/analytics/qurator/'
dataDir = '/home/goransm/Analytics/Qurator/CuriousFacts/_data/'

### --- Init Spark
# - Spark Session
sc = SparkSession\
    .builder\
    .appName("WMDE Qurator ETL")\
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

# - read single value constraint properties
properties = hdfsDir +  'singleValueConstraintProperties.csv'
properties = sqlContext.read.csv(properties, header=True)
properties = properties.select('property')
# - subclasses to list + add tclass 
properties = properties.toPandas()
properties =  properties['property'].tolist()
# - unique elements only
properties = list(set(properties))

# - read separators
separators = hdfsDir +  'separators.csv'
separators = sqlContext.read.csv(separators, header=True)
separators = separators.select('separator')
# - subclasses to list + add tclass 
separators = separators.toPandas()
separators =  separators['separator'].tolist()
# - unique elements only
separators = list(set(separators))

### --- find items with > 1 value on single value constraint properties
WD_items = sqlContext.sql('SELECT id, claims FROM wmf.wikidata_entity WHERE snapshot="' + wikidataEntitySnapshot + '"')
# - cache WD dump for items
WD_items.cache()
# - explode properties
WD_items = WD_items.select('id', explode('claims').alias('claims'), ).select('id', 'claims.mainSnak', 'claims.qualifiers')
WD_items = WD_items.select('id', 'mainSnak.property', 'mainSnak.dataValue.value', 'qualifiers.property')
# - create cols_new so that seen columns will have a suffix 'qualifier'
cols_new = [] 
seen = set()
for c in WD_items.columns:
    cols_new.append('{}_qualifier'.format(c) if c in seen else c)
    seen.add(c)
WD_items = WD_items.toDF(*cols_new)
WD_items = WD_items.select('id', 'property', 'value', explode_outer('property_qualifier').alias('qualifier'))
# - filter by: properties list
WD_items = WD_items.filter(WD_items['property'].isin(properties))
# - filter out properties from 'id': keep only items
WD_items = WD_items.filter((WD_items["id"].rlike('Q\d+')))
# - introduce ids: ix
WD_items = WD_items.withColumn('ix', row_number().over(Window.orderBy(monotonically_increasing_id())))
# - filter by: no separator property P4155 is used to allow for multiple values
propsP4155 = WD_items.select('ix', 'property', 'qualifier')
propsP4155 = propsP4155.filter(propsP4155["qualifier"].isin(separators)).select('ix')
WD_items = WD_items.join(propsP4155, on = 'ix', how = 'left_anti')
# - drop qualifiers + de-duplicate rows
WD_items = WD_items.select('id', 'property', 'value').dropDuplicates()
# - groupby id and property -> derive counts
WD_items = WD_items.select('id', 'property').groupBy('id', 'property').count()
WD_items = WD_items.withColumnRenamed('id', 'item')

# - add English labels to id and property
### --- Access WD dump
WD_labels = sqlContext.sql('SELECT id, labels FROM wmf.wikidata_entity WHERE snapshot="' + wikidataEntitySnapshot + '"')
### --- Cache WD_labels
WD_labels.cache()
### --- Explode labels & select
WD_labels = WD_labels.select('id', explode('labels').alias("language", "label"))
WD_labels = WD_labels.filter("language == 'en'")
WD_labels = WD_labels.select('id', 'label')
# - join to WD_items: item
WD_items = WD_items.join(WD_labels, WD_items.item == WD_labels.id, how = 'left')
WD_items = WD_items.withColumnRenamed('label', 'itemLabel')
WD_items = WD_items.select('item', 'itemLabel', 'property', 'count')
# - join to WD_items: property
WD_items = WD_items.join(WD_labels, WD_items.property == WD_labels.id, how = 'left')
WD_items = WD_items.withColumnRenamed('label', 'propertyLabel')
WD_items = WD_items.select('item', 'itemLabel', 'property', 'propertyLabel', 'count')

# - store result
filename = "result_M3_" + wikidataEntitySnapshot + ".csv"
WD_items.coalesce(10).write.format('csv').mode("overwrite").save(hdfsDir + filename)
