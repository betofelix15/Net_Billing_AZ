# Mapping utility locations
# 5 Aug, 2025
# Author: Jesus Felix

#### 0.  Pre-requesites ----------------------------------------------------------------
# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "tidygeocoder",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi",
          "zoo","gmapsdistance","rmapshaper", "gridExtra","tigris","digest")

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

#### 1. Load data ------------------------------------------------------------------------------

# load data
nm <- read.csv(paste0(dir$cleandata,"clean_res_solar_wind_nm_utility_aug_4_2025.cvs"))

# load IOU zipcode data
iou_zip <- NULL
for(i in c(2011,2013:2023)){
  zf <- read.csv(paste0(dir$rawdata,"iouzipcodes",i,".csv"))
  zf$year <- i
  
  iou_zip <- rbind(zf,iou_zip)
  
}

# load IOU zipcode data
niou_zip <- NULL
for(i in c(2011,2013:2023)){
  zf <- read.csv(paste0(dir$rawdata,"non_iou_zipcodes_",i,".csv"))
  zf$year <- i
  
  niou_zip <- rbind(zf,niou_zip)
  
}



#### 2. Prep data --------------------------------------------------------------------------------

# rbind iou and noiu utilities zip
u_zip <- rbind(iou_zip,niou_zip)

# check change in price
#plot(u_zip[u_zip$eiaid == 24211,]$year, u_zip[u_zip$eiaid == 24211 ,]$res_rate )

# 2012 is missing so fill it in with 2013
z_12 <- u_zip[u_zip$year == 2013,] # filter 2013 out
z_12$year <- 2012 # change the year

u_zip <- rbind(u_zip,z_12) # bind to full set

# there are similar utility ID's in different states, so we need a full id to 
# distinguish these
u_zip$full_id <- paste(u_zip$state,u_zip$eiaid, sep = "_")

nm$full_id <- paste(nm$state,nm$utility_id, sep = "_")

# utitilities may have different boundaries each year, distinguish
u_zip$full_id_y <- paste(u_zip$full_id,u_zip$year, sep = "_")

nm$full_id_y <- paste(nm$full_id,nm$year, sep = "_")

# lets get sf file with all the polygons for each zip code in u.s.
# find all zip_codes for the state 
options(tigris_use_cache = TRUE)
zip_sf_10 <- zctas(year = 2010) #using 2010 for years before 2015
zip_sf_20 <- zctas(year = 2020) # using 2020 for year after 2015

# make the zipcode numeric
zip_sf_10$GEOID10 <- as.numeric(zip_sf_10$GEOID10)
zip_sf_20$GEOID20 <- as.numeric(zip_sf_20$GEOID20)

#### 3. Make boundaries for utilities using zipcodes -----------------------------------------

# Create list of unique full_id_y values before 2024
u_list <- as.list(unique(nm[nm$year < 2024, ]$full_id_y))

# Initialize output object
u_bound <- NULL

# Loop through each utility-year
for(i in u_list) { 
  
  d1 <- u_zip[u_zip$full_id_y == i, ] # find utility in zipcode list
  
  # if utility is not found, skip
  if(nrow(d1) == 0){next}
  
  # Pick correct ZIP shapefile depending on year
  if(d1$year[1] < 2015){
    d_zip <- zip_sf_10[zip_sf_10$GEOID10 %in% d1$zip, ]
  } else {
    d_zip <- zip_sf_20[zip_sf_20$GEOID20 %in% d1$zip, ]
  }
  
  # Union of geometries
  geo <- st_union(d_zip$geometry)
  
  # Keep selected columns only once per group
  d2 <- unique(d1[c(2:6, 10:12)])
  
  # Add average rates in cents per kWh
  d2$annual_res_cents_kwh <- mean(d1$res_rate, na.rm = TRUE) * 100
  d2$annual_comm_cents_kwh <- mean(d1$comm_rate, na.rm = TRUE) * 100
  d2$annual_ind_cents_kwh <- mean(d1$ind_rate, na.rm = TRUE) * 100
  
  # Simplify geometry
  d2$geometry <- st_simplify(geo, dTolerance = 0.01, preserveTopology = TRUE)
  # Append to output
  u_bound <- rbind(u_bound, d2)
  
  # Progress message
  print(paste0(i, " is done"))
}

# utility boundaries as csv
write.csv(st_drop_geometry(u_bound),
          paste0(dir$clean,"utility_boundaries_prices_df.csv"),
          row.names = F)


# make into a sf object
u_bound_sf <- st_as_sf(u_bound)

# check how many geometries are empty
nrow(u_bound_sf[st_is_empty(u_bound_sf$geometry),])

empty_geom <- u_bound_sf[st_is_empty(u_bound_sf$geometry),]

length(unique(empty_geom$full_id_y))

# find out if they had geometries before and add them
sf_geom <- u_bound_sf[u_bound_sf$full_id %in% empty_geom$full_id &
                        !st_is_empty(u_bound_sf$geometry),]

sf_geom <- unique(sf_geom[c("full_id","geometry","service_type")]) # only unique values for this 

empty_geom <- st_drop_geometry(empty_geom)

new_geom <- left_join(empty_geom, sf_geom, by = c("full_id","service_type"))

# Keep the last observation of each group
new_geom <- new_geom %>%
  group_by(full_id_y, service_type) %>%
  slice_tail(n = 1) # Retains the last row of each group

# export for the income stuff (so you don't do it all over again)

st_write(new_geom,
         paste0(dir$shp,"missing_geom.shp"))

# add to the rest

u_bound_sf2 <- u_bound_sf[u_bound_sf$full_id_y %not_in% new_geom$full_id_y,]

new_geom <- st_as_sf(new_geom)  # make the new_geom an sf object

u_bound_sf2 <- rbind(u_bound_sf2,new_geom)

# how many empty now?
nrow(u_bound_sf2[is_empty(u_bound_sf2$geometry),])

# export
st_write(u_bound_sf2,
         paste0(dir$shp,"utility_boundaries_prices.shp"),
         append = F)

# read in
u_sf <-  st_read(paste0(dir$shp,"utility_boundaries_prices.shp"))

names <- c("utility_id","utility","state", "service_type","ownership","year","full_id","full_id_y","annual_res_cents_kwh","annual_comm_cents_kwh","annual_ind_cents_kwh","geometry")

colnames(u_sf) <- names


# simplify further for use in earth engine and only using the latest geometries for the utility
simp_sf <- u_sf %>%
  select(year,full_id,full_id_y,geometry)


# Number of features per batch
batch_size <- 1000  

# Split into a list of sf objects
sf_list <- split(simp_sf, (seq_len(nrow(simp_sf)) - 1) %/% batch_size)

# Simplify each batch
sf_list_simplified <- lapply(sf_list, function(batch) {
  st_simplify(batch, dTolerance = 20, preserveTopology = TRUE)
})

# Combine back into one sf object
simp_sf_simplified <- do.call(rbind, sf_list_simplified)

plot(simp_sf_simplified[simp_sf_simplified$year == 2020 &
                          simp_sf_simplified$full_id == "AZ",]$geometry)

simp_sf <- st_simplify(simp_sf, dTolerance = 10)

p_bound <- st_read(paste0(dir$shp,"utility_boundaries_simplified_annual_all.shp"))

nrow(p_bound[st_is_empty(p_bound$geometry),])
nrow(simp_sf[st_is_empty(simp_sf$geometry),])

# export
st_write(simp_sf,
         paste0(dir$shp,"utility_boundaries_annual_v2.shp"))
