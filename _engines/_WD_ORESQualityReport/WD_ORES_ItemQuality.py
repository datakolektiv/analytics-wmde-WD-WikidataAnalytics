
### ---------------------------------------------------------------------------
### --- Wikidata Quality Report
### --- Script: WD_ORES_ItemQuality.py
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE, 2021/12.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### --- Modules
import pyspark
from pyspark.sql import SparkSession, DataFrame
from pyspark.sql.functions import rank, col, row_number, explode, regexp_extract
from pyspark.sql.window import Window
import csv

### --- directory structure
oresScoresPath = 'hdfs:///tmp/wmde/analytics/ORESPredictions'
oresOutputPath = 'hdfs:///tmp/wmde/analytics/ORESOutput/'

### --- Init Spark
# - Spark Session
sc = SparkSession\
    .builder\
    .appName("WD_ORES_Item_Quality")\
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

### --- Revision History

### --- Access wmf.mediawiki_history
mwHistory = sqlContext.sql('SELECT page_title, event_timestamp FROM wmf.mediawiki_history \
	                                WHERE snapshot = "' + mwwikiSnapshot + '" AND \
                                    wiki_db = "wikidatawiki" AND event_entity = "revision" \
                                    AND event_type = "create" AND page_namespace = 0 \
                                    ORDER BY page_title, event_timestamp DESC')

# - cache mwHistory
mwHistory.cache()

# - filter: the most recent revision only
window = Window.partitionBy(mwHistory['page_title']).orderBy(mwHistory['event_timestamp'].desc())
mwHistory = mwHistory.select(col('*'), row_number().over(window).alias('row_number')) \
    .where(col('row_number') == 1)

### --- WD JSON Dump
# - make sure to keep only the existing items: parse ID from dump
### --- Access WD dump
WD_dump = sqlContext.sql('SELECT id FROM wmf.wikidata_entity WHERE snapshot="' + wikidataEntitySnapshot + '"')
### --- Cache WD dump
WD_dump.cache()
# - join mwHistory to WD_dump
mwHistory = WD_dump.withColumnRenamed("id", "page_title").join(mwHistory, \
                                                              ["page_title"], \
                                                              how = 'left')

### --- Join WDCM re-use statistics to Revision History
# - join w. WDCM re-use statistics
# - from wdcm_clients_wb_entity_usage
wdcm_item = sqlContext.sql('SELECT eu_entity_id, wiki_db AS eu_project, COUNT(*) AS eu_count FROM \
                                        (SELECT DISTINCT eu_entity_id, eu_page_id, wiki_db \
                                            FROM goransm.wdcm_clients_wb_entity_usage \
                                            WHERE (eu_entity_id RLIKE "^Q" AND (eu_aspect like "S" or eu_aspect like "T" or eu_aspect like "O"))) \
                                            AS t GROUP BY wiki_db, eu_entity_id')

# - create View from WDCM_MainTableRaw
wdcm_item.createTempView("wdcmmain")
# - produce: wdcm_topItems
wdcm_item = sqlContext.sql('SELECT eu_entity_id, SUM(eu_count) AS eu_count FROM wdcmmain GROUP BY eu_entity_id')
#- join:
mwHistory = mwHistory.withColumnRenamed("page_title", "eu_entity_id").join(wdcm_item, \
                                                                           ["eu_entity_id"], \
                                                                           how = 'left')

### --- Join ORES scores to Revision History/Reuse data
# - load ORES scores
oresScores = sqlContext.read.format('csv')\
    .options(header='true', inferSchema='false')\
    .load(oresScoresPath)
oresScores.cache()
# - use only 'title' and 'prediction' from oresScores
oresScores = oresScores.select("title", "prediction")

# - join mwHistory to oresScores
mwHistory = mwHistory.join(oresScores.withColumnRenamed('title', 'eu_entity_id'), \
                          ["eu_entity_id"], \
                          how='left')
mwHistory = mwHistory.select('eu_entity_id', 'event_timestamp', 'eu_count', 'prediction')

# - repartition mwHistory; write
# - repartition
mwHistory = mwHistory.repartition(10)
# - save to csv:
mwHistory.write.format('csv').mode("overwrite").save(oresOutputPath + 'wdORESQuality_Reuse.csv')

# - clear
sc.catalog.clearCache()
