#! /usr/bin/python3

### ---------------------------------------------------------------------------
### --- WD_Inequality, v 1.0.0
### --- script: WD_Inequality_Update.py
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- January 2021.
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Update script for the Wikidata Revision Inequality project.
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

### --- Modules
import os
import re
import xml.etree.ElementTree as ET
import pandas as pd

### --- Directory tree
# - where is the scrip run from?
fPath = os.path.dirname(os.path.realpath(__file__)) + "/"
# - dataDir
dataDir = fPath + "_data/"
# - analyticsDir
analyticsDir = fPath + "_analytics/"

### --- Hoover Index
def hooverIndex(x):
    x['IncomePerIndividual'] = x['edits']/x['count']
    x['RelativeDeviation'] = x['edits']/x['edits'].sum() - x['count']/x['count'].sum()    
    x['Hoover'] = x['RelativeDeviation'].abs()
    hoover = (x['Hoover'].sum())/2
    return hoover

### --- parse WRI parameters
parsFile = \
   "/home/goransm/Analytics/Wikidata/WD_Inequality/wdiConfig.xml"
# - parse wdiConfig.xml
tree = ET.parse(parsFile)
root = tree.getroot()
k = [elem.tag for elem in root.iter()]
v = [x.text for x in root.iter()]
params = dict(zip(k, v))
publicDir = params['publicDir']

### ---------------------------------------------------------------------------
### --- 1: Run update if necessary
### ---------------------------------------------------------------------------

# - Kerberos Init
command = \
   'sudo -u analytics-privatedata kerberos-run-command analytics-privatedata hdfs dfs -ls'
os.system(command)
# - List wmf.mediawiki_history partitions
query = 'SHOW PARTITIONS wmf.mediawiki_history;'
f = open(fPath + "snapshot_query.hql", "w")
f.write(query)
f.close()
commandPrefix = 'sudo -u analytics-privatedata kerberos-run-command ' + \
   'analytics-privatedata /usr/local/bin/beeline --incremental=true --silent -f'
command = fPath + "snapshot_query.hql"
commandPipe = fPath + "wdsnaps.csv"
os.system(commandPrefix + " " + \
   command + " > " + \
   commandPipe)
# - load wdsnaps.csv
snapsFrame = pd.read_csv(fPath + "wdsnaps.csv")
snapsFrame = pd.DataFrame.tail(snapsFrame, 1)
snaps = str(snapsFrame.iloc[0,0])
# - check if reference_snapshot.txt exists
check = os.path.exists(fPath + "reference_snapshot.csv")
if check:
    # - toReport
    print("The reference_snapshot.csv found: check if the update should be run.")
    # - load reference_snapshot.txt and compare to snaps
    referenceSnaps = pd.read_csv(fPath + "reference_snapshot.csv")
    referenceSnaps = str(referenceSnaps.iloc[0, 1])
    # - if snaps != referenceSnaps, RUN UPDATE 
    if snaps != referenceSnaps:
        # - toReport
        print("New data present: the update will be run.")
        # - snaps becomes new reference_snapshot.csv
        snapsFrame.to_csv(fPath + "reference_snapshot.csv")
        # - run regular update
        # - parse WRI parameters
        parsFile = fPath + "wdiConfig_Deploy.xml"
        # - parse wdiConfig.xml
        tree = ET.parse(parsFile)
        root = tree.getroot()
        k = [elem.tag for elem in root.iter()]
        v = [x.text for x in root.iter()]
        params = dict(zip(k, v))
        # - set Spark
        command = 'sudo -u analytics-privatedata spark2-submit ' + \
        params['master'] + " " + params['deploy_mode'] + " " + \
        params['num_executors'] + " " + params['driver_memory'] + \
        " " + params['executor_memory'] + " " + params['executor_cores'] + " " + \
        params['config'] + " " + fPath + "WD_Inequality_ETL.py"
        # - run Spark ETL
        os.system(command)
    else:
        # - toReport
        print("No new data - no update; exiting.")
        sys.exit()
else:
    # - toReport
    print("This is the FIRST UPDATE.")
    # - snaps becomes first reference_snapshot.csv
    snapsFrame.to_csv(fPath + "reference_snapshot.csv")
    # - run first update
    # - parse WRI parameters
    parsFile = fPath + "wdiConfig_Deploy.xml"
    # - parse wdiConfig.xml
    tree = ET.parse(parsFile)
    root = tree.getroot()
    k = [elem.tag for elem in root.iter()]
    v = [x.text for x in root.iter()]
    params = dict(zip(k, v))
    # - set Spark
    command = 'sudo -u analytics-privatedata spark2-submit ' + \
    params['master'] + " " + params['deploy_mode'] + " " + \
    params['num_executors'] + " " + \
    params['driver_memory'] + " " + params['executor_memory'] + \
    " " + params['executor_cores'] + " " + \
    params['config'] + " " + fPath + "WD_Inequality_ETL.py"
    # - run Spark ETL
    os.system(command)

### ---------------------------------------------------------------------------
### --- 2: Compute Hoover
### ---------------------------------------------------------------------------

# - check if there are any files in analyticsDir
files = os.listdir(analyticsDir)
if len(files) == 0:
    # - toReport
    print("Processing first production files in analyticsDir.")
    hoover = list()
    dataFiles = os.listdir(dataDir)
    for i in dataFiles:
        dataset = pd.read_csv(dataDir + i)
        hoover.append(hooverIndex(dataset))
    hoover = pd.DataFrame({'hoover':hoover})
    measurement = [str(x.split(".")[0]) for x in dataFiles]
    measurement = [re.sub("wdri_", "", x) for x in measurement]
    hoover['measurement'] = measurement
    snapshot = re.sub("snapshot=", "", snaps)
    hoover['snapshot'] = snapshot
    hoover.to_csv(analyticsDir + "HooverUpdate.csv", \
       header=True, index=False)
else:
    # - toReport
    print("Attaching new data to analyticsDir.")
    newhoover = list()
    dataFiles = os.listdir(dataDir)
    for i in dataFiles:
        dataset = pd.read_csv(dataDir + i)
        newhoover.append(hooverIndex(dataset))
    newhoover = pd.DataFrame({'hoover':newhoover})
    measurement = [str(x.split(".")[0]) for x in dataFiles]
    measurement = [re.sub("wdri_", "", x) for x in measurement]
    newhoover['measurement'] = measurement
    snapshot = re.sub("snapshot=", "", snaps)
    newhoover['snapshot'] = snapshot
    hoover = pd.read_csv(analyticsDir + "HooverUpdate.csv")
    hoover = hoover.append(newhoover)
    hoover.to_csv(analyticsDir + "HooverUpdate.csv", \
       header=True, index=False)
    
### ---------------------------------------------------------------------------
### --- 3: Publish
### ---------------------------------------------------------------------------
command = "cp " + analyticsDir + "HooverUpdate.csv " + publicDir
os.system(command)
