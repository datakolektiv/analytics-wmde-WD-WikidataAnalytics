
### ---------------------------------------------------------------------------
### --- WD_HumanEditsPerClass, v 0.0.1
### --- script: WD_HumanEditsPerClass_ETL.py
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- ETL for the Wikidata Human Edits Per Class Project
### --- (WHEPC)
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
    
### --- parse WDCM parameters
parsFile = "/home/goransm/Analytics/Wikidata/WD_HumanEdits/wdHumanEditsPerClass_Config.xml"
# - parse wdcmConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))

### --- dir structure and params
hdfsPath = params['hdfsPath']

# - Spark Session
sc = SparkSession\
    .builder\
    .appName("WD Human Edits per Class")\
    .enableHiveSupport()\
    .getOrCreate()

# - Spark Session Log Level: INFO
sc.sparkContext.setLogLevel("INFO")

# - SQL context
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
### --- 1: Produce human vs. bot Wikidata edit statistics
### ---------------------------------------------------------------------------

# - Process Wikidata JSON Dump: extract item, class by P31
WD_dump = sqlContext.sql('SELECT id, claims.mainSnak, labels FROM wmf.wikidata_entity WHERE snapshot="' + wikidataEntitySnapshot + '"')
WD_dump.cache()
WD_dump = WD_dump.withColumn('mainSnak', explode('mainSnak'))
WD_dump = WD_dump.select('id', col('mainSnak.property').alias('property'), col('mainSnak.datavalue.value').alias('value'), explode('labels').alias("language", "label"))
# produce labs (en)
labs = WD_dump.select('id', 'label', 'language').where(col('language').isin("en")).select('id', 'label').distinct()
labs = labs.filter(labs['id'].rlike("Q\d+"))
WD_dump = WD_dump.filter(WD_dump.property == 'P31')
WD_dump = WD_dump.select(col("id").alias("item"), col("value").alias("wd_class")).distinct()
WD_dump = WD_dump.withColumn('wd_class', regexp_extract(col('wd_class'), '(Q\d+)', 1))
WD_dump = WD_dump.filter(WD_dump['item'].rlike("Q\d+"))
WD_dump = WD_dump.orderBy('item')

# - Process wmf.mediawiki_history: extract item, numHumanEdits, numBotEdits
wmwh = sqlContext.sql('SELECT page_title, event_user_is_bot_by FROM wmf.mediawiki_history WHERE event_entity="revision" AND event_type="create" AND wiki_db="wikidatawiki" AND page_namespace=0 AND snapshot="' + mwwikiSnapshot + '"')
wmwh = wmwh.withColumn("bot_name", array_contains(col("event_user_is_bot_by"), "name"))
wmwh = wmwh.withColumn("bot_group", array_contains(col("event_user_is_bot_by"), "group"))
wmwh = wmwh.select('page_title', 'bot_name', 'bot_group')
wmwh = wmwh.withColumn("human", when((col("bot_name") == True) | (col("bot_group") == True), 0).otherwise(1))
wmwh = wmwh.withColumn("bot", when((col("bot_name") == True) | (col("bot_group") == True), 1).otherwise(0))
wmwh = wmwh.select(col('page_title').alias('itemhist'), 'human', 'bot')
wmwh = wmwh.groupBy('itemhist').agg(sum("human"),sum("bot"))
wmwh = wmwh.select(col('itemhist'), col("sum(human)").alias("human_edits"), col("sum(bot)").alias("bot_edits"))
wmwh = wmwh.orderBy('human_edits', ascending = False)

# - Left join: WD_dump on wmwh
wmwh = wmwh.join(WD_dump, wmwh.itemhist == WD_dump.item, how='left')
wmwh = wmwh.select('item', 'wd_class', 'human_edits', 'bot_edits')
wmwh = wmwh.orderBy('human_edits', ascending = False)
wmwh = wmwh.filter("item is not NULL")

# - Ratio of Human vs Bot Edits per Item
humanBotItems = wmwh.select('item', 'human_edits', 'bot_edits')
# de-duplicate humanBotItems
humanBotItems = humanBotItems.dropDuplicates()
# compute human/bot edits ratio and percent
humanBotItems = humanBotItems.withColumn("total_edits", col("human_edits")+col("bot_edits"))
humanBotItems = humanBotItems.withColumn("human_to_bot_ratio", col("human_edits")/col("bot_edits"))
humanBotItems = humanBotItems.withColumn("human_ratio", col("human_edits")/col("total_edits"))
humanBotItems = humanBotItems.withColumn("bot_ratio", col("bot_edits")/col("total_edits"))
humanBotItems = humanBotItems.withColumn("human_percent", col("human_ratio")*100)
humanBotItems = humanBotItems.withColumn("bot_percent", col("bot_ratio")*100)
# labels to humanBotItems
humanBotItems = humanBotItems.join(labs, humanBotItems.item == labs.id, how='left').drop('id').orderBy('human_ratio', ascending = False)
# save: humanBotItems
fileName = "humanBotItems.csv"
humanBotItems.repartition(30).write.option("quote", "\u0000").format('csv').mode("overwrite").save(hdfsPath + fileName)

# - Ratio of Human vs Bot Edits per Class
humanBotClasses = wmwh.select('wd_class', 'human_edits', 'bot_edits')
humanBotClasses = humanBotClasses.groupBy('wd_class').agg(sum("human_edits"), sum("bot_edits"))
humanBotClasses = humanBotClasses.select(col('wd_class'), col("sum(human_edits)").alias("human_edits"), col("sum(bot_edits)").alias("bot_edits"))
humanBotClasses = humanBotClasses.withColumn("total_edits", col("human_edits") + col("bot_edits"))
humanBotClasses = humanBotClasses.withColumn("human_to_bot_ratio", col("human_edits")/col("bot_edits"))
humanBotClasses = humanBotClasses.withColumn("human_ratio", col("human_edits")/col("total_edits"))
humanBotClasses = humanBotClasses.withColumn("bot_ratio", col("bot_edits")/col("total_edits"))
humanBotClasses = humanBotClasses.withColumn("human_percent", col("human_ratio")*100)
humanBotClasses = humanBotClasses.withColumn("bot_percent", col("bot_ratio")*100)
# add labels to humanBotClasses
humanBotClasses = humanBotClasses.join(labs, humanBotClasses.wd_class == labs.id, how='left').drop('id').orderBy('human_ratio', ascending = False)
# save: humanBotClasses
fileName = "humanBotClasses.csv"
humanBotClasses.repartition(30).write.option("quote", "\u0000").format('csv').mode("overwrite").save(hdfsPath + fileName)

# Number and percent of items edited by Humans, per Class
wmwh = wmwh.withColumn("human_edited", when((col("human_edits") > 0), 1).otherwise(0))
wmwh = wmwh.select("wd_class", "human_edited")
wmwh = wmwh.groupBy("wd_class").agg(sum("human_edited"), count("wd_class"))
wmwh = wmwh.select(col('wd_class'), col("sum(human_edited)").alias("human_edited_items"), col("count(wd_class)").alias("num_items"))
wmwh = wmwh.withColumn("proportion_items_touched", col("human_edited_items")/col("num_items"))
wmwh = wmwh.withColumn("percent_items_touched", (col("human_edited_items")/col("num_items"))*100)
# - join labels
wmwh = wmwh.join(labs, wmwh.wd_class == labs.id, how='left').drop('id').drop('item')
wmwh = wmwh.orderBy(['proportion_items_touched', 'num_items'], ascending = [0, 0])
# save: humanTouchClass (wmwh)
fileName = "humanTouchClass.csv"
wmwh.repartition(30).write.option("quote", "\u0000").format('csv').mode("overwrite").save(hdfsPath + fileName)

### ---------------------------------------------------------------------------
### --- 2: Produce median number of unique Wikidata human editors per class
### ---------------------------------------------------------------------------

# - The median number of unique human contributors per item by class
wmwh = sqlContext.sql('SELECT page_title, event_user_id, event_user_is_bot_by FROM wmf.mediawiki_history WHERE event_entity="revision" AND event_type="create" AND wiki_db="wikidatawiki" AND page_namespace=0 AND snapshot="' + mwwikiSnapshot + '"')
wmwh = wmwh.withColumn("bot_name", array_contains(col("event_user_is_bot_by"), "name"))
wmwh = wmwh.withColumn("bot_group", array_contains(col("event_user_is_bot_by"), "group"))
wmwh = wmwh.select('page_title', 'event_user_id', 'bot_name', 'bot_group')
wmwh = wmwh.withColumn("human", when((col("bot_name") == True) | (col("bot_group") == True), 0).otherwise(1))
wmwh = wmwh.filter(wmwh.human == 1)
wmwh = wmwh.select('page_title', 'event_user_id').distinct()
wmwh = wmwh.select('page_title').groupBy('page_title').count()
# - join WD_dump to wmwh
wmwh = wmwh.join(WD_dump, wmwh.page_title == WD_dump.item, how='left').drop('item')
wmwh = wmwh.filter("wd_class is not NULL")
# - median number of unique human contributors by class
wmwh_class = wmwh.select('wd_class', 'count').groupBy('wd_class').agg(expr('percentile(count, array(0.5))')[0].alias('median'), count('wd_class').alias('num_items')) 
wmwh_class = wmwh_class.join(labs, wmwh_class.wd_class == labs.id, how='left').drop('id').orderBy('median', ascending = False)
# save: wmwh_class
fileName = "classMedianEditors.csv"
wmwh_class.repartition(30).write.option("quote", "\u0000").format('csv').mode("overwrite").save(hdfsPath + fileName)
