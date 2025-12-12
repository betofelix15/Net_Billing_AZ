# -*- coding: utf-8 -*-
"""
Created on Sat Feb 15 07:48:18 2025

@author: jesus
"""

import numpy
import ee
import pandas as pd
from datetime import datetime, timedelta
import geopandas as gpd

# import state boundaries
ubound = gpd.re

# authenticate
ee.Authenticate()

# Initialize the Earth Engine module
ee.Initialize(
  user = j.felix@usu.edu
)

# Load the image collection
tcc_collection = ee.ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')
  .filter(ee.Filter.date('2013-01-01', '2020-01-01'))
  .select('tcdc')

# Make a loop for each utility and collect the daily cloud coverage
utility_daily_tcc = []
for u in u_bound['full_id'].tolist():  # select utility
    # Start time stamp
    start = datetime.now()

    # Filter the utility
    u_sf = u_bound[u_bound['full_id'] == u]

    year_tcc_df = []
    for y in range(2013, 2020):  # select year
        # Start time stamp
        start_year = datetime.now()

        y_sf = u_sf[u_sf['year'] == y]

        # Turn y_sf geometry into an ee object
        boundary = ee.Geometry.Polygon(y_sf['geometry'].values[0])  # Assuming geometry is in the first row

        # Generate a list of dates for that year
        dates = pd.date_range(start=f'{y}-01-01', end=f'{y}-12-31').strftime('%Y-%m-%d').tolist()

        daily_tcc = ee.FeatureCollection([])

        for d in dates:  # Loop through each date
            # Clip the daily image from tcc_collection
            daily_image = tcc_collection \
                .filterDate(d, ee.Date(d).advance(1, 'day')) \
                .mean() \
                .clip(boundary)

            # Reduce to mean over the boundary
            reduced_value = daily_image.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=boundary,
                scale=2500,  # Adjust scale based on dataset resolution
                maxPixels=1e13
            ).get('tcdc')

            # Return a feature with date and TCC value
            tcc_ee = ee.Feature(None, {'date': d, 'tcc': reduced_value})

            # Make a collection of these features
            daily_tcc = daily_tcc.merge(tcc_ee)

            # Tell us where we are at
            print(f"{d} done for {u}")

        # Turn feature into a dataframe in Python
        tcc_df = ee_to_dataframe(daily_tcc)  # Assuming a function to convert ee.FeatureCollection to DataFrame
        tcc_df = tcc_df[['date', 'tcc']]

        # Save the year tcc
        tcc_df['year'] = y
        tcc_df['month'] = pd.to_datetime(tcc_df['date']).dt.month
        year_tcc_df.append(tcc_df)

        # End time stamp
        end_year = datetime.now()

        # Time required for each step
        elapsed_time_year = end_year - start_year

        print(elapsed_time_year)

    # Save utility
    year_tcc_df = pd.concat(year_tcc_df, ignore_index=True)
    year_tcc_df['full_id'] = u
    utility_daily_tcc.append(year_tcc_df)

    print(f"{u} is done")

    # End time stamp
    end = datetime.now()

    # Time required for each step
    elapsed_time_utility = end - start

    print(elapsed_time_utility)

# Assuming utility_daily_tcc is a list of DataFrames, you can concatenate them if needed
utility_daily_tcc = pd.concat(utility_daily_tcc, ignore_index=True)
