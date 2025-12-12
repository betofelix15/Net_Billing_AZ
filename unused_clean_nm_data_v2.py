# Title: 1_clean_nm_data_Python
# Start date: 8 Aug 2025
# Created to focus on utility differences: There are Investor-Owned Utilities (IOU) and 
# Non-investor owned utilities (NOIU). These were affected differently by the policy.
# The state level aggregation is therefore flawed in this sense. 

#### ----- 0. Libraries and directories ------

import numpy
import ee
import pandas as pd
from datetime import datetime, timedelta
import geopandas as gpd
import os 


# check directory 
dir = os.getcwd()


#### ----- 1. Load Utility Level data ------

# Load .xls files for 2011 to 2013
for i in range(2011, 2014):
    filename = f"nm_{i}"
    wd = os.path.join(dir['rawdata'], f"net_metering_{i}.xls")
    globals()[filename] = pd.read_excel(wd, sheet_name=1)  # sheet index is 0-based in Python

# Load 2014 and 2015 explicitly
nm_2014 = pd.read_excel(os.path.join(dir['rawdata'], "net_metering_2014.xls"), sheet_name=2)
nm_2015 = pd.read_excel(os.path.join(dir['rawdata'], "net_metering_2015.xls"), sheet_name=3)

# Load .xlsx files for 2016 to 2022
for i in range(2016, 2023):
    filename = f"nm_{i}"
    wd = os.path.join(dir['rawdata'], f"net_metering_{i}.xlsx")
    globals()[filename] = pd.read_excel(wd, sheet_name=3)
