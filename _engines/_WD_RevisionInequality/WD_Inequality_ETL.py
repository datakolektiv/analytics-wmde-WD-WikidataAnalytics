
### ---------------------------------------------------------------------------
### --- WD_Inequality, v 1.0.0
### --- script: WD_Inequality_ETL.py
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- January 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- ETL for the Wikidata Revision Inequality project.
### --- (WRI)
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Revisions Inequality project (WRI).
### ---
### --- WRI is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WRI is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WRI. If not, see <http://www.gnu.org/licenses/>.

### ---------------------------------------------------------------------------
### --- 0: Init
### ---------------------------------------------------------------------------

# - modules
import pyspark
from pyspark.sql import SparkSession, DataFrame, Window
from pyspark.sql.functions import rank, col, explode, regexp_extract, array_contains, when, sum, count, expr
import pandas as pd
import os
import subprocess
import gc
import re
import csv
import sys
import xml.etree.ElementTree as ET
    
### --- parse WRI parameters
parsFile = "/home/goransm/Analytics/Wikidata/WD_Inequality/wdiConfig.xml"
# - parse wdiConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))

### --- dir structure and params
hdfsPath = params['hdfsPath']
dataDir = params['dataDir']

# - Spark Session
sc = SparkSession\
    .builder\
    .appName("WD Inequality")\
    .enableHiveSupport()\
    .getOrCreate()

# - Spark Session Log Level: INFO
sc.sparkContext.setLogLevel("INFO")

# - SQL context
sqlContext = pyspark.SQLContext(sc)

### --- get wmf.mediawiki_history snapshot
snaps = sqlContext.sql('SHOW PARTITIONS wmf.mediawiki_history')
snaps = snaps.toPandas()
mwwikiSnapshot = snaps.tail(1)['partition'].to_string()
mwwikiSnapshot = mwwikiSnapshot[-7:]
currentMonth = mwwikiSnapshot
currentYear = mwwikiSnapshot[0:4]

### --- Edits distribution: since the beginning of time and until current snapshot
wdri = sqlContext.sql('SELECT event_user_id, event_user_is_bot_by FROM wmf.mediawiki_history WHERE event_entity="revision" AND event_type="create" AND wiki_db="wikidatawiki" AND page_namespace=0 AND snapshot="' + mwwikiSnapshot + '"')
wdri = wdri.withColumn("bot_name", array_contains(col("event_user_is_bot_by"), "name"))
wdri = wdri.withColumn("bot_group", array_contains(col("event_user_is_bot_by"), "group"))
wdri = wdri.select('event_user_id', 'bot_name', 'bot_group')
wdri = wdri.withColumn("human", when((col("bot_name") == True) | (col("bot_group") == True), 0).otherwise(1))
wdri = wdri.withColumn("bot", when((col("bot_name") == True) | (col("bot_group") == True), 1).otherwise(0))
wdri = wdri.filter((wdri["bot"] == 1)).select('event_user_id')
wdri = wdri.groupBy('event_user_id').count().orderBy('count', ascending = False)
wdri = wdri.select('count').withColumnRenamed('count', 'edits').groupBy('edits').count().orderBy('count', ascending = False)
fileName = "wdri_history_to_current_snapshot.csv"
wdri.cache().coalesce(1).toPandas().to_csv(dataDir + fileName, header=True, index=False)

### --- Edits distribution: current snapshot only: month
wdri = sqlContext.sql('SELECT event_user_id, event_timestamp, event_user_is_bot_by FROM wmf.mediawiki_history WHERE event_entity="revision" AND event_type="create" AND wiki_db="wikidatawiki" AND page_namespace=0 AND snapshot="' + mwwikiSnapshot + '"')
wdri = wdri.filter(wdri['event_timestamp'].rlike("^" + currentMonth))
wdri = wdri.withColumn("bot_name", array_contains(col("event_user_is_bot_by"), "name"))
wdri = wdri.withColumn("bot_group", array_contains(col("event_user_is_bot_by"), "group"))
wdri = wdri.select('event_user_id', 'bot_name', 'bot_group')
wdri = wdri.withColumn("human", when((col("bot_name") == True) | (col("bot_group") == True), 0).otherwise(1))
wdri = wdri.withColumn("bot", when((col("bot_name") == True) | (col("bot_group") == True), 1).otherwise(0))
wdri = wdri.filter((wdri["bot"] == 1)).select('event_user_id')
wdri = wdri.groupBy('event_user_id').count().orderBy('count', ascending = False)
wdri = wdri.select('count').withColumnRenamed('count', 'edits').groupBy('edits').count().orderBy('count', ascending = False)
fileName = "wdri_current_snapshot.csv"
wdri.cache().coalesce(1).toPandas().to_csv(dataDir + fileName, header=True, index=False)

### --- Edits distribution: current year
wdri = sqlContext.sql('SELECT event_user_id, event_timestamp, event_user_is_bot_by FROM wmf.mediawiki_history WHERE event_entity="revision" AND event_type="create" AND wiki_db="wikidatawiki" AND page_namespace=0 AND snapshot="' + mwwikiSnapshot + '"')
wdri = wdri.filter(wdri['event_timestamp'].rlike("^" + currentYear))
wdri = wdri.withColumn("bot_name", array_contains(col("event_user_is_bot_by"), "name"))
wdri = wdri.withColumn("bot_group", array_contains(col("event_user_is_bot_by"), "group"))
wdri = wdri.select('event_user_id', 'bot_name', 'bot_group')
wdri = wdri.withColumn("human", when((col("bot_name") == True) | (col("bot_group") == True), 0).otherwise(1))
wdri = wdri.withColumn("bot", when((col("bot_name") == True) | (col("bot_group") == True), 1).otherwise(0))
wdri = wdri.filter((wdri["bot"] == 1)).select('event_user_id')
wdri = wdri.groupBy('event_user_id').count().orderBy('count', ascending = False)
wdri = wdri.select('count').withColumnRenamed('count', 'edits').groupBy('edits').count().orderBy('count', ascending = False)
fileName = "wdri_current_year.csv"
wdri.cache().coalesce(1).toPandas().to_csv(dataDir + fileName, header=True, index=False)

