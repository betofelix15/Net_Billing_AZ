## google earth engine 
# get total cloud coverage
# get surface temperature
# obtain any other GIS variables of interest
# made on feb 14, 2025 (valentines day!)
# mostly because shapefiles are too big for google collab to handle

## Pre-requesites ----------------------------------------------------------------
# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "tidygeocoder",
          "maptools","scales","mapview", "sf","readxl","sqldf","geojsonio",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi",
          "zoo","gmapsdistance","rmapshaper", "gridExtra","tigris","rgee","reticulate")

# This line says" check and see which of the packages you want is already
# installed.  Those note loadded are ones you "need".
need <- want[!(want %in% installed.packages()[,"Package"])]

# This line says, if a package is "needed", install it.
if (length(need)) install.packages(need)

# This line "requires" all the packages you want.  Meaning that in order 
# to use a function that is loaded, you need to "activate" it, by "requiring" it.
sapply(want, function(i) require(i, character.only = TRUE))

# This line removes the vectors "want" and "need".
rm(want, need)


### Working directories

# This line creates an empty list, which the next few lines populate with 
# a set of file paths.
dir <- list()

# This line uses getwd() to find where your .Rproj file is.
getwd()

# This line takes this location as the "root" directory for the project.
#dir$root <- dirname(getwd()) # root directory of the project 
# getwd() is actually the directory we need (dependent on where you open project)******discuss with Steve*****


dir$root <- getwd()

# Observe the output in the console:
dir$root 

# shapefiles
dir.create(paste0(dir$root, "/shapefiles/"))
dir$shp <- paste0(dir$root, "/shapefiles/")

# figures directory (these line create a new folder for you to export figures)
dir.create(paste0(dir$root, "/figures/"))
dir$fig <- paste0(dir$root, "/figures/")


# unprocessed data folder (this line creates a new folder for "raw" data)
dir.create(paste0(dir$root, "/data/raw data/"))
dir$rawdata <- paste0(dir$root, "/data/raw data/")

# clean data folder (this line creates a new folder for "clean" data)
dir.create(paste0(dir$root, "/data/clean data/"))
dir$cleandata <- paste0(dir$root, "/data/clean data/")

# create not in
`%not_in%` <- negate(`%in%`)

# remove scientific notation
options(scipen = 999)


##### Set up python and rgee ------------------------------------------------------------
# if you want to clean up house
# rgee::ee_clean_pyenv()
# reticulate::use_python(NULL, required = FALSE)
# Sys.unsetenv("RETICULATE_PYTHON")

# check whether python is installed (using reticulate package)
py_available()

# if false, install
# first remove any python environments and restart (ctr+shift+f10)
# ee_clean_pyenv()
# 
# # now discover where to save the environmnet
py_discover_config()
py_module_available("ee")
py_module_available("earthengine-api")
py_module_available("numpy")
# py_install() # if python is not installed
# 

# install python environment (using rgee package)
use_python("C:/Program Files/Python313/python.exe", required = TRUE)
ee_install(py_env = "rgee")
ee_install_set_pyenv(
  py_path = "C:/Users/Jesus/OneDrive/Documents/.virtualenvs/rgee/Scripts/python.exe",
  py_env = "rgee",
  Renviron = "local"
)


reticulate::py_install(c("earthengine-api", "numpy","ee"), envname = "rgee")

ee_install_upgrade() # this one has worked if you have issues later on (after installing environment, packages, etc.)


# since this has restarted r session, reload the pre-requisites and check that
# dependencies are installed.
ee_check()

# set up connection between r and google earth
ee_Initialize(
  user = "j.felix@usu.edu"
)
# if asked to generate toke, save it and apply

##### Load and transform  utility data  ------------------------------------------------

z_sf <- st_read(paste0(dir$shp,"utility_boundaries_all_years.shp"))
# 
# remove the unnecessary variables
u_bound <- z_sf[c("year","eiaid","utlty_n","full_id","geometry")]

# rename utility_n
u_bound <- rename(u_bound, "utility" = "utlty_n")

# test tep utility
tep_b <- u_bound[u_bound$full_id == "AZ_24211" & u_bound$year == 2015,]

plot(tep_b$geometry)

# what if we simplified?
simp_tep <- st_simplify(tep_b$geometry, dTolerance = 9000)
plot(simp_tep)

# what about a very complicated geometry
comp_b <- u_bound[u_bound$full_id == "CA_14328" & u_bound$year == 2013,]$geometry
# this has 8 megabytes; 8,116,976 bytes
plot(comp_b)

# simplifying goes to 21852 bytes, 27 kilobytes
simp_comp <- st_simplify(comp_b, dTolerance = 10000)
plot(simp_comp)

# simplify the geometry to reduce memory usage (ironicallly this code takes long)
# u_bound$geometry <- st_simplify(u_bound$geometry,
#                                 dTolerance = 10000)
# making a loop might be faster
u_bound$full_id_y <- paste0(u_bound$full_id,"_",u_bound$year)

simp_bound <- NULL
for(i in as.list(u_bound$full_id_y)){

  u_temp <- u_bound[u_bound$full_id_y == i,]

  u_temp$geometry <- st_simplify(u_temp$geometry,
                                 dTolerance = 10000)

  simp_bound <- rbind(simp_bound,u_temp)

  print(paste0(i," is done!"))

}

# simp_bound: 3,465,920 bytes, 3 megabytes

# export and save
st_write(simp_bound, paste0(dir$shp,"utility_boundaries_simplified_annual.shp"),
         append = F)

# load this new shapefile
u_bound <- st_read(paste0(dir$shp,"utility_boundaries_simplified_annual.shp"))

### Cloud coverage -----------------------------------------------------------------
# https://developers.google.com/earth-engine/datasets/catalog/NOAA_NCEP_DOE_RE2_total_cloud_coverage

# description: NCEP-DOE Reanalysis 2 project is using a state-of-the-art analysis/forecast system to perform data assimilation using past data from 1979 through the previous year.

# citation: NCEP-DOE AMIP-II Reanalysis (R-2): M. Kanamitsu, W. Ebisuzaki, J. Woollen, S-K Yang, J.J. Hnilo, M. Fiorino, and G. L. Potter. 1631-1643, Nov 2002, Bulletin of the American Meteorological Society..

# availability: 1979 to 2025

# provider: NOAA

# Cadence: 6 hrs
# Pixel size: 278300 meters
# bands: tcdc , units: %, min: 0*, max: 100*, description: Total Cloud Cover
# ee snippet: ee.ImageCollection("NOAA/NCEP_DOE_RE2/total_cloud_coverage") 


# bring the data from ee
tcc_collection<- ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$ # load the image
    filter(ee$Filter$date('2013-01-01','2019-12-31'))$ # filter the date
    select('tcdc') # select the band


# lets visualize this image set

# choose a boundary
boundary <- sf_as_ee(
  u_bound[u_bound$full_id_y == "CA_14328_2013",]$geometry,  
  proj = 'EPSG:4269')

# get collection from ee
tcc_collection <- ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
  filterDate('2013-04-01', '2013-05-01')$
  filterBounds(boundary)$
  select('tcdc')

# Reduce to a single mean image clipped to a boundary
mean_tcc <- tcc_collection$mean()$clip(boundary)


# define visual parameters
vis_params <- list(
  min = 0,        # Minimum value for scaling
  max = 100,      # Maximum value for scaling
  palette = c('blue', 'green', 'yellow', 'red') # Color scale
)

# display the map
Map$centerObject(mean_tcc, zoom = 3)
Map$addLayer(mean_tcc, vis_params, "Mean Cloud Coverage")


# Total Cloud Coverage  for all utilities ## ## ## ##

# lets use the bound for TEP (Tucson Electric Power) in 2014, full_id: AZ_24211

# Load image collection and filter by date
tcc_collection <- ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$select('tcdc')

# List of unique utility IDs
ulist <- as.list(unique(u_bound$full_id_y))
utility_monthly_tcc <- NULL

# Loop over utilities and year
for (u in ulist) {  # u <- "AZ_24211_2011"
  start_time <- Sys.time()
  
  u_sf <- u_bound %>% filter(full_id_y == u)
  
  # Skip if no geometry
  if (nrow(u_sf) == 0 | st_is_empty(u_sf) ) { next }
  
  # Convert to EE geometry
  boundary <- sf_as_ee(
      u_sf$geometry,  
      proj = 'EPSG:4269')
    
  # Generate monthly date list
  months <- seq(as.Date(paste0(u_sf$year, '-01-01')), as.Date(paste0(u_sf$year, '-12-31')), by = 'month')
  
  # select the dates to filter by   
  d1 <- as.Date(paste0(u_sf$year, '-01-01'))
  d2 <- as.Date(d1 %m+% months(12)) # select one year (12 months)
      
  monthly_image <- tcc_collection$
        filter(ee$Filter$date(as.character(d1),
                              as.character(d2)))
      
  # Function to extract daily mean TCC
  extract_tcc <- function(image) {
      # Get image date
      date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd HH:MM')
      
      # Reduce the region (mean tcc)
      mean_tcc <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = boundary,
        scale = 5000,   # Adjust scale for resolution
        maxPixels = 1e13
      )$get('tcdc')
      
      # Return as a feature (date, SST)
      ee$Feature(NULL, list(date = date, avg_tcc = mean_tcc))
    }
    
  # Apply function to each image
  tcc_features <- monthly_image$map(extract_tcc)
  
  # Convert to FeatureCollection and extract as table
  tcc_table <- ee$FeatureCollection(tcc_features)
  
  # Convert to R DataFrame
  tcc_df <- st_drop_geometry(ee_as_sf(tcc_table))

  # summarize by month, filter for noon
  f_tcc <- tcc_df %>%
    filter(hour(date) == 12)%>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarize(
      "cloudy_days" = length(which(avg_tcc >= 50)), # cloudy days are any-day where in noon 50% or more covered by clouds
    )
  
  # save the utility info
  f_tcc <- merge(st_drop_geometry(u_sf),f_tcc)
  
  # Save results
  utility_monthly_tcc <- rbind(utility_monthly_tcc, f_tcc)
  
  print(paste0(u, " completed"))
  print(Sys.time() - start_time)

}

  
write.csv(utility_monthly_tcc,paste0(dir$cleandata,"monthly_cloudy_days_utility.csv"),
          row.names = F)


# now there are utilities with no geometry, we need to figure those out
no_geom <- u_bound %>% filter(st_is_empty(geometry))


#### lets analyze this data #  ###################### #
u_tcc <- read.csv(paste0(dir$cleandata,"monthly_cloudy_days_utility.csv"))

#drop row names
u_tcc <- u_tcc[-c(1)]

# distribution of tcc
hist(u_tcc$cloudy_days)

summary(u_tcc$cloudy_days)

# load net metering data
u_nm <- read.csv(paste0(dir$cleandata,"utility_res_net_metering_12_feb_2025.csv"))

# combine the two
u_nm_tcc <- left_join(u_tcc,u_nm, by = c('year',"month","full_id"))


# check relationship between tcc and energy sold back
plot(u_nm_tcc$cloudy_days,u_nm_tcc$energy_sold_back_kwh)




##### surface air temperature ---------------------------------------------------------------
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_MONTHLY
# ERA5 Monthly Aggregates - Latest Climate Reanalysis Produced by ECMWF / Copernicus Climate Change Service
# in Kelvins (k)
# image collection: 'ECMWF/ERA5/MONTHLY'
# citation: Copernicus Climate Change Service (C3S) (2017): ERA5: Fifth generation of ECMWF atmospheric reanalyses of the global climate. Copernicus Climate Change Service Climate Data Store (CDS), (date of access), https://cds.climate.copernicus.eu/cdsapp#!/home
# terms of use:Please acknowledge the use of ERA5 as stated in the Copernicus C3S/CAMS License agreement:
#
# 5.1.1 Where the Licensee communicates or distributes Copernicus Products to the public, the Licensee shall inform the recipients of the source by using the following or any similar notice: "Generated using Copernicus Climate Change Service information (Year)".
# 5.1.2 Where the Licensee makes or contributes to a publication or distribution containing adapted or modified Copernicus Products, the Licensee shall provide the following or any similar notice: "Contains modified Copernicus Climate Change Service information (Year)".
# 5.1.3 Any such publication or distribution covered by clauses 5.1.1 and 5.1.2 shall state that neither the European Commission nor ECMWF is responsible for any use that may be made of the Copernicus information or Data it contains.

# band : mean_2m_air_temperature , measured in Kelvins. from 223.6 to 304 K


# lets use the bound for TEP (Tucson Electric Power) in 2014, full_id: AZ_24211

# choose the band
band <- "mean_2m_air_temperature"

# Load image collection
collection <- ee$ImageCollection('ECMWF/ERA5/MONTHLY')$select(band)


# List of unique utility IDs
ulist <- as.list(unique(u_bound$full_id_y))
utility_monthly_temp <- NULL

# Loop over utilities and year
for (u in ulist) {  # u <- "AZ_24211_2011"
  start_time <- Sys.time()
  
  u_sf <- u_bound %>% filter(full_id_y == u)
  
  # Skip if no geometry
  if (nrow(u_sf) == 0 | st_is_empty(u_sf) ) { next }
  
  # Convert to EE geometry
  boundary <- sf_as_ee(
    u_sf$geometry,  
    proj = 'EPSG:4269')
  
  # Generate monthly date list
  months <- seq(as.Date(paste0(u_sf$year, '-01-01')), as.Date(paste0(u_sf$year, '-12-31')), by = 'month')
  
  # select the dates to filter by   
  d1 <- as.Date(paste0(u_sf$year, '-01-01'))
  d2 <- as.Date(d1 %m+% months(12)) # select one year (12 months)
  
  annual_image <- collection$
    filter(ee$Filter$date(as.character(d1),
                          as.character(d2)))
  
  # Function to extract daily mean TCC
  extract <- function(image) {
    # Get image date
    date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
    
    # Reduce the region (mean)
    mean <- image$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = boundary,
      scale = 5000,   # Adjust scale for resolution
      maxPixels = 1e13
    )$get(band)
    
    # Reduce the region (median)
    median <- image$reduceRegion(
      reducer = ee$Reducer$median(),
      geometry = boundary,
      scale = 5000,   # Adjust scale for resolution
      maxPixels = 1e13
    )$get(band)
    
    
    # Return as a feature (date, SST)
    ee$Feature(NULL, list(date = date, avg_temp = mean, median_temp = median))
  }
  
  # Apply function to each image
  features <- annual_image$map(extract)
  
  # Convert to FeatureCollection and extract as table
  table <- ee$FeatureCollection(features)
  
  # Convert to R DataFrame
  df <- st_drop_geometry(ee_as_sf(table))
  
  # change k to celsius
  df$avg_temp <- df$avg_temp - 273.15
  df$median_temp <- df$median_temp - 273.15
  
  # save the month
  df$month <- month(df$date)

  # save the utility info
  df <- merge(st_drop_geometry(u_sf),df)
  
  # Save results
  utility_monthly_temp <- rbind(utility_monthly_temp, df)
  
  print(paste0(u, " completed"))
  print(Sys.time() - start_time)
  
}


write.csv(utility_monthly_temp,paste0(dir$cleandata,"monthly_temp_celcius_utility.csv"),
          row.names = F)


### analyze
summary(utility_monthly_temp$avg_temp)
hist(utility_monthly_temp$avg_temp)


#
#
#####
#####
#####
##### there were missing utilities -------------------------
#####
#####
#####


##### Load and transform  utility data  ------------------------------------------------

z_sf <- st_read(paste0(dir$shp,"utility_boundaries_all_years_missing.shp"))
# 
# remove the unnecessary variables
u_bound <- z_sf[c("year","eiaid","utlty_n","full_id","geometry")]

# rename utility_n
u_bound <- rename(u_bound, "utility" = "utlty_n")

# test tep utility
tep_b <- u_bound[u_bound$full_id == "AZ_24211" & u_bound$year == 2015,]

plot(tep_b$geometry)

# what if we simplified?
simp_tep <- st_simplify(tep_b$geometry, dTolerance = 9000)
plot(simp_tep)

# what about a very complicated geometry
comp_b <- u_bound[u_bound$full_id == "CA_14328" & u_bound$year == 2013,]$geometry
# this has 8 megabytes; 8,116,976 bytes
plot(comp_b)

# simplifying goes to 21852 bytes, 27 kilobytes
simp_comp <- st_simplify(comp_b, dTolerance = 10000)
plot(simp_comp)

# simplify the geometry to reduce memory usage (ironicallly this code takes long)
# u_bound$geometry <- st_simplify(u_bound$geometry,
#                                 dTolerance = 10000)
# making a loop might be faster
u_bound$full_id_y <- paste0(u_bound$full_id,"_",u_bound$year)

simp_bound <- NULL
for(i in as.list(u_bound$full_id_y)){
  
  u_temp <- u_bound[u_bound$full_id_y == i,]
  
  u_temp$geometry <- st_simplify(u_temp$geometry,
                                 dTolerance = 10000)
  
  simp_bound <- rbind(simp_bound,u_temp)
  
  print(paste0(i," is done!"))
  
}

# simp_bound: 3,465,920 bytes, 3 megabytes

# export and save
st_write(simp_bound, paste0(dir$shp,"utility_boundaries_simplified_annual_missing.shp"),
         append = F)

# load this new shapefile
u_bound2 <- st_read(paste0(dir$shp,"utility_boundaries_simplified_annual_missing.shp"))

# create all boundaries
all_bound <- rbind(u_bound, u_bound2)

# export the all
st_write(all_bound, paste0(dir$shp,"utility_boundaries_simplified_annual_all.shp"),
         append = F)

##### Cloud coverage -----------------------------------------------------------------
# https://developers.google.com/earth-engine/datasets/catalog/NOAA_NCEP_DOE_RE2_total_cloud_coverage

# description: NCEP-DOE Reanalysis 2 project is using a state-of-the-art analysis/forecast system to perform data assimilation using past data from 1979 through the previous year.

# citation: NCEP-DOE AMIP-II Reanalysis (R-2): M. Kanamitsu, W. Ebisuzaki, J. Woollen, S-K Yang, J.J. Hnilo, M. Fiorino, and G. L. Potter. 1631-1643, Nov 2002, Bulletin of the American Meteorological Society..

# availability: 1979 to 2025

# provider: NOAA

# Cadence: 6 hrs
# Pixel size: 278300 meters
# bands: tcdc , units: %, min: 0*, max: 100*, description: Total Cloud Cover
# ee snippet: ee.ImageCollection("NOAA/NCEP_DOE_RE2/total_cloud_coverage") 


# bring the data from ee
tcc_collection<- ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$ # load the image
  filter(ee$Filter$date('2013-01-01','2019-12-31'))$ # filter the date
  select('tcdc') # select the band


# lets visualize this image set

# choose a boundary
boundary <- sf_as_ee(
  u_bound[u_bound$full_id_y == "CA_14328_2013",]$geometry,  
  proj = 'EPSG:4269')

# get collection from ee
tcc_collection <- ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
  filterDate('2013-04-01', '2013-05-01')$
  filterBounds(boundary)$
  select('tcdc')

# Reduce to a single mean image clipped to a boundary
mean_tcc <- tcc_collection$mean()$clip(boundary)


# define visual parameters
vis_params <- list(
  min = 0,        # Minimum value for scaling
  max = 100,      # Maximum value for scaling
  palette = c('blue', 'green', 'yellow', 'red') # Color scale
)

# display the map
Map$centerObject(mean_tcc, zoom = 3)
Map$addLayer(mean_tcc, vis_params, "Mean Cloud Coverage")


# Total Cloud Coverage  for all utilities ## ## ## ##

# lets use the bound for TEP (Tucson Electric Power) in 2014, full_id: AZ_24211

# Load image collection and filter by date
tcc_collection <- ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$select('tcdc')

# List of unique utility IDs
ulist <- as.list(unique(all_bound$full_id_y))
utility_monthly_tcc <- NULL

# Loop over utilities and year
for (u in ulist) {  # u <- "AZ_24211_2011"
  start_time <- Sys.time()
  
  u_sf <- all_bound %>% filter(full_id_y == u)
  
  # Skip if no geometry
  if (nrow(u_sf) == 0 | st_is_empty(u_sf) ) { next }
  
  # Convert to EE geometry
  boundary <- sf_as_ee(
    u_sf$geometry,  
    proj = 'EPSG:4269')
  
  # Generate monthly date list
  months <- seq(as.Date(paste0(u_sf$year, '-01-01')), as.Date(paste0(u_sf$year, '-12-31')), by = 'month')
  
  # select the dates to filter by   
  d1 <- as.Date(paste0(u_sf$year, '-01-01'))
  d2 <- as.Date(d1 %m+% months(12)) # select one year (12 months)
  
  monthly_image <- tcc_collection$
    filter(ee$Filter$date(as.character(d1),
                          as.character(d2)))
  
  # Function to extract daily mean TCC
  extract_tcc <- function(image) {
    # Get image date
    date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd HH:MM')
    
    # Reduce the region (mean tcc)
    mean_tcc <- image$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = boundary,
      scale = 5000,   # Adjust scale for resolution
      maxPixels = 1e13
    )$get('tcdc')
    
    # Return as a feature (date, SST)
    ee$Feature(NULL, list(date = date, avg_tcc = mean_tcc))
  }
  
  # Apply function to each image
  tcc_features <- monthly_image$map(extract_tcc)
  
  # Convert to FeatureCollection and extract as table
  tcc_table <- ee$FeatureCollection(tcc_features)
  
  # Convert to R DataFrame
  tcc_df <- st_drop_geometry(ee_as_sf(tcc_table))
  
  # summarize by month, filter for noon
  f_tcc <- tcc_df %>%
    filter(hour(date) == 12)%>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarize(
      "cloudy_days" = length(which(avg_tcc >= 75)), # cloudy days are any-day where in noon 75% or more covered by clouds
    )
  
  # save the utility info
  f_tcc <- merge(st_drop_geometry(u_sf),f_tcc)
  
  # Save results
  utility_monthly_tcc <- rbind(utility_monthly_tcc, f_tcc)
  
  print(paste0(u, " completed"))
  print(Sys.time() - start_time)
  
}


write.csv(utility_monthly_tcc,paste0(dir$cleandata,"monthly_cloudy_days_utility_all_75pcnt_cov.csv"),
          row.names = F)


# now there are utilities with no geometry, we need to figure those out
no_geom <- u_bound %>% filter(st_is_empty(geometry))


#### lets analyze this data #  ###################### #
u_tcc <- read.csv(paste0(dir$cleandata,"monthly_cloudy_days_utility.csv"))

#drop row names
u_tcc <- u_tcc[-c(1)]

# distribution of tcc
hist(u_tcc$cloudy_days)

summary(u_tcc$cloudy_days)

# load net metering data
u_nm <- read.csv(paste0(dir$cleandata,"utility_res_net_metering_12_feb_2025.csv"))

# combine the two
u_nm_tcc <- left_join(u_tcc,u_nm, by = c('year',"month","full_id"))


# check relationship between tcc and energy sold back
plot(u_nm_tcc$cloudy_days,u_nm_tcc$energy_sold_back_kwh)




##### surface air temperature ---------------------------------------------------------------
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_MONTHLY
# ERA5 Monthly Aggregates - Latest Climate Reanalysis Produced by ECMWF / Copernicus Climate Change Service
# in Kelvins (k)
# image collection: 'ECMWF/ERA5/MONTHLY'
# citation: Copernicus Climate Change Service (C3S) (2017): ERA5: Fifth generation of ECMWF atmospheric reanalyses of the global climate. Copernicus Climate Change Service Climate Data Store (CDS), (date of access), https://cds.climate.copernicus.eu/cdsapp#!/home
# terms of use:Please acknowledge the use of ERA5 as stated in the Copernicus C3S/CAMS License agreement:
#
# 5.1.1 Where the Licensee communicates or distributes Copernicus Products to the public, the Licensee shall inform the recipients of the source by using the following or any similar notice: "Generated using Copernicus Climate Change Service information (Year)".
# 5.1.2 Where the Licensee makes or contributes to a publication or distribution containing adapted or modified Copernicus Products, the Licensee shall provide the following or any similar notice: "Contains modified Copernicus Climate Change Service information (Year)".
# 5.1.3 Any such publication or distribution covered by clauses 5.1.1 and 5.1.2 shall state that neither the European Commission nor ECMWF is responsible for any use that may be made of the Copernicus information or Data it contains.

# band : mean_2m_air_temperature , measured in Kelvins. from 223.6 to 304 K


# lets use the bound for TEP (Tucson Electric Power) in 2014, full_id: AZ_24211

# choose the band
band <- "mean_2m_air_temperature"

# Load image collection
collection <- ee$ImageCollection('ECMWF/ERA5/MONTHLY')$select(band)


# List of unique utility IDs
ulist <- as.list(unique(u_bound$full_id_y))
utility_monthly_temp <- NULL

# Loop over utilities and year
for (u in ulist) {  # u <- "AZ_24211_2011"
  start_time <- Sys.time()
  
  u_sf <- u_bound %>% filter(full_id_y == u)
  
  # Skip if no geometry
  if (nrow(u_sf) == 0 | st_is_empty(u_sf) ) { next }
  
  # Convert to EE geometry
  boundary <- sf_as_ee(
    u_sf$geometry,  
    proj = 'EPSG:4269')
  
  # Generate monthly date list
  months <- seq(as.Date(paste0(u_sf$year, '-01-01')), as.Date(paste0(u_sf$year, '-12-31')), by = 'month')
  
  # select the dates to filter by   
  d1 <- as.Date(paste0(u_sf$year, '-01-01'))
  d2 <- as.Date(d1 %m+% months(12)) # select one year (12 months)
  
  annual_image <- collection$
    filter(ee$Filter$date(as.character(d1),
                          as.character(d2)))
  
  # Function to extract daily mean TCC
  extract <- function(image) {
    # Get image date
    date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
    
    # Reduce the region (mean)
    mean <- image$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = boundary,
      scale = 5000,   # Adjust scale for resolution
      maxPixels = 1e13
    )$get(band)
    
    # Reduce the region (median)
    median <- image$reduceRegion(
      reducer = ee$Reducer$median(),
      geometry = boundary,
      scale = 5000,   # Adjust scale for resolution
      maxPixels = 1e13
    )$get(band)
    
    
    # Return as a feature (date, SST)
    ee$Feature(NULL, list(date = date, avg_temp = mean, median_temp = median))
  }
  
  # Apply function to each image
  features <- annual_image$map(extract)
  
  # Convert to FeatureCollection and extract as table
  table <- ee$FeatureCollection(features)
  
  # Convert to R DataFrame
  df <- st_drop_geometry(ee_as_sf(table))
  
  # change k to celsius
  df$avg_temp <- df$avg_temp - 273.15
  df$median_temp <- df$median_temp - 273.15
  
  # save the month
  df$month <- month(df$date)
  
  # save the utility info
  df <- merge(st_drop_geometry(u_sf),df)
  
  # Save results
  utility_monthly_temp <- rbind(utility_monthly_temp, df)
  
  print(paste0(u, " completed"))
  print(Sys.time() - start_time)
  
}


write.csv(utility_monthly_temp,paste0(dir$cleandata,"monthly_temp_celcius_utility_missing.csv"),
          row.names = F)


### analyze
summary(utility_monthly_temp$avg_temp)
hist(utility_monthly_temp$avg_temp)









