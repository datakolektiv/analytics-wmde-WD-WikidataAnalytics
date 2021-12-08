
### ---------------------------------------------------------------------------
### --- WD_PageviewsPerType_Engine.py
### --- v 1.0.0
### --- Authors: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- September 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- Pyspark ETL procedures for the Wikidata Pageviews Per Namespace System
### ---------------------------------------------------------------------------

### --- Modules
from datetime import datetime, timedelta
import pyspark
from pyspark.sql import SparkSession, DataFrame
import csv
import pandas as pd

### --- parameters
dataDir = \
   '/tmp/wmde/analytics/Wikidata/WD_PageviewsPerType/_data/'

### --- Init Spark
# - Spark Session
sc = SparkSession\
    .builder\
    .appName("wd_pageviewsPerType")\
    .enableHiveSupport()\
    .getOrCreate()

# - SQL Context
sqlContext = pyspark.SQLContext(sc)

### ------------------------------------------------
### --- Pageviews datasets
### ------------------------------------------------

### --- UTC Day:
d = datetime.today() - timedelta(days=1)

# - filenames:
filename = str(d.year) + '-' + \
   str(d.month) + '-' + str(d.day)
filename = 'wd_pageviewsPerType_' + filename
filename = filename + '.csv'
filename = dataDir + filename
# - pageviews dataset: Analytics/Data Lake/Traffic/Pageview hourly
pw = sqlContext.sql('SELECT namespace_id, \
                    access_method, \
                    agent_type, \
                    SUM(view_count) AS pageviews \
                    FROM wmf.pageview_hourly\
                    WHERE  year = ' + \
                    str(d.year) + ' AND month = ' + \
                    str(d.month) + ' AND day = ' + \
                    str(d.day) + \
                    ' AND project = "wikidata" \
                    AND (namespace_id = 0 OR namespace_id = 120 \
                    OR namespace_id = 146 OR namespace_id = 640) \
                    GROUP BY namespace_id, \
                    access_method, \
                    agent_type \
                    ORDER BY namespace_id, access_method, agent_type')
pw.cache().coalesce(1) \
        .write.format('csv')\
        .mode("overwrite")\
        .save(filename)
