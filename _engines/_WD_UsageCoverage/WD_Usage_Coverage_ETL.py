
### ---------------------------------------------------------------------------
### --- WD_percentUsage_ETL.py, v 0.0.1
### --- script: WD_percentUsage_ETL.py
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- July 2020.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- ETL for the Wikidata Usage and Coverage (WDUC) Project
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Usage and Coverage (WDUC)
### ---
### --- WDUC is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDUC is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDUC. If not, see <http://www.gnu.org/licenses/>.

### ---------------------------------------------------------------------------
### --- 0: Init
### ---------------------------------------------------------------------------

# - modules
import pyspark
from pyspark.sql import SparkSession, DataFrame
import csv
import sys
import xml.etree.ElementTree as ET

### --- parse WDCM parameters
parsFile = "/home/goransm/Analytics/Wikidata/WD_UsageCoverage/WD_Usage_Coverage_Config.xml"
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
    .appName("WD Usage and Coverage")\
    .enableHiveSupport()\
    .getOrCreate()

# - SQL context
sqlContext = pyspark.SQLContext(sc)

### ---------------------------------------------------------------------------
### --- 1: Produce Wikidata Usage (non-(S)itelinks) and Coverage ((S)itelinks)
### --- datasets.
### ---------------------------------------------------------------------------

# - USAGE. Process goransm.wdcm_clients_wb_entity_usage: non-(S)itelinks
WD_Usage = sqlContext.sql("SELECT DISTINCT eu_page_id, wiki_db from goransm.wdcm_clients_wb_entity_usage WHERE eu_aspect != 'S'")
WD_Usage.cache()
# save: wdUsagePerPage
fileName = "wdUsage"
WD_Usage.repartition(10).write.option("quote", "\u0000").format('csv').mode("overwrite").save(hdfsPath + fileName)

# - COVERAGE. Process goransm.wdcm_clients_wb_entity_usage: (S)itelinks
WD_Coverage = sqlContext.sql("SELECT DISTINCT eu_page_id, wiki_db from goransm.wdcm_clients_wb_entity_usage WHERE eu_aspect == 'S'")
WD_Coverage.cache()
# save: wdSitelinks
fileName = "wdSitelinks"
WD_Coverage.repartition(10).write.option("quote", "\u0000").format('csv').mode("overwrite").save(hdfsPath + fileName)
