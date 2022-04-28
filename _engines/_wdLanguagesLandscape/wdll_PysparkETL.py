
### ---------------------------------------------------------------------------
### --- wdll_PysparkETL.py
### --- Version 1.0.0
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- August 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Orchestrate WD Languages Landscape modules:
### --- 1. wdll_PysparkETL.py
### --- Pyspark ETL for WDLL
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- Script 1: wdll_PysparkETL.py
### ---------------------------------------------------------------------------

### --- Modules
import pyspark
from pyspark.sql import SparkSession, DataFrame
from pyspark.sql.functions import rank, col, \
   explode, regexp_extract
import csv
import pandas as pd

### --- Init Spark

# - Spark Session
sc = SparkSession\
    .builder\
    .appName("wd_processDump_Spark")\
    .enableHiveSupport()\
    .getOrCreate()
# - hdfs path
hdfsPath = \
   "hdfs:///tmp/wmde/analytics/Wikidata/LanguagesLandscape/"

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

### ------------------------------------------------------------------------
### --- Extract all entities w. labels
### ------------------------------------------------------------------------

### --- Access WD dump
WD_dump = sqlContext.sql('SELECT id, labels \
                        FROM wmf.wikidata_entity WHERE snapshot="' + \
                        wikidataEntitySnapshot + '"')

### --- Cache WD dump
WD_dump.cache()

### --- Explode labels & select
WD_dump = WD_dump.select('id', explode('labels').alias("language", "label"))
WD_dump = WD_dump.select('id', 'language')

# - repartition
WD_dump = WD_dump.orderBy(["id"])
WD_dump = WD_dump.repartition(10)

# - save to csv:
WD_dump.write.format('csv')\
   .mode("overwrite")\
   .save(hdfsPath + 'wd_dump_item_language')

# - clear
sc.catalog.clearCache()
