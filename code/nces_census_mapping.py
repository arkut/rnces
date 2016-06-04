import csv
import sys,os
import pandas as pd
import numpy as np
from urllib import urlopen
import json

infile="../input/sc132a.txt"

nces_data=pd.read_table(infile,sep='\t',low_memory=False)

# Extract Alexandria (51013) and Arlington data (51510)
alx_arl=nces_data[(nces_data['CONUM'] == '51013') | (nces_data['CONUM'] == '51510')]

# Only want these columns
subset=alx_arl[["SCHNAM","NCESSCH","MSTREE","MCITY","MSTATE","MZIP","MZIP4","LATCOD","LONCOD","TOTFRL","MEMBER","FRELCH","REDLCH"]]

# Store lats/lons in own lists
lats=subset['LATCOD'].tolist()
lons=subset['LONCOD'].tolist()

# Loop through each lat/lon and call FCC census mapping API
tracts=[]
blocks=[]
for i,lat in enumerate(lats):
    qstring='http://data.fcc.gov/api/block/find?format=json&latitude='+str(lat)+'&longitude='+str(lons[i])+'&showall=true'
    print qstring
    url = urlopen(qstring).read()
    data=json.loads(url)
    block_group=data['Block']['FIPS'][:12]
    tract_val=block_group[:11]
    blocks.append(block_group.encode('ascii','ignore'))
    tracts.append(tract_val.encode('ascii','ignore'))

# Add blocks/tracts to dataframe
subset['TRACT'] = pd.Series(tracts,index=subset.index)
subset['BLOCK'] = pd.Series(blocks,index=subset.index)

# Write to CSV file
subset.to_csv("./nces_alxarl_with_census.csv")