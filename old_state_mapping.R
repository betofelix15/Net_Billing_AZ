# Title: state mapping
# Author: Jesus Felix
# Start date: 25 NOV 2024
################################################################################
# This version does municipio level data first then summarizes for the states

#### R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi",
          "zoo","gmapsdistance","rmapshaper", "gridExtra")

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

# not in
`%not_in%` <- negate(`%in%`)

##### import shapefile  --------------------------------------------------
# resolution differs between 500k, 5m, and 20m, lets go with 5m
states_sf <- st_read(paste0(dir$shp,"cb_2018_us_state_5m.shp"))

# what is the crs? units of measurement?
crs(states_sf) # ESPG 4269, meters

# visualize the shapefile
plot(states_sf$geometry) # kinda small innit?

# remove dc, and territories
states_sf <- states_sf %>% filter(STUSPS %not_in% c("DC","PR","AS","GU","MP","VI"))
  
 
# AGAIN, visualize the shapefile but without Alakska or Hawaii, looks bigger right?
plot(states_sf[states_sf$STUSPS %not_in% c("HI","AK"),]$geometry)


# Alaska is the problem child, so lets clip off that last bit way in the east
plot(states_sf[states_sf$STUSPS== "AK",]$geometry)
ak <- states_sf[states_sf$STUSPS== "AK",]
ak_clipped <- ms_clip(ak,bbox = c(-180,50,-130,80))
plot(ak_clipped$geometry)

# replace geometry in states_sf
states_sf[states_sf$STUSPS== "AK",]$geometry <- ak_clipped$geometry

# now plot the entire US of A Baby!!!
plot(states_sf$geometry) # now that looks better

# remove scientific notation
options(scipen =999)

# export the state_sf
st_write(states_sf,paste0(dir$shp,"us_states_sf.shp"))

#### (do this after fixing some ) combine with the net metering (residential,state-level) --------------------------------------------
nm_all_res <- read.csv(paste0(dir$cleandata,"res_net_metering_wcovariates.csv"))
nm_all_res$date <- as.Date(nm_all_res$date)

# join states_sf with nm_all_res
nm_mapped <- left_join(nm_all_res,states_sf[c(5:6,8:10)], join_by("state" == "STUSPS"))

# export as dataframe, takes a minute
#write.csv(nm_mapped,paste0(dir$cleandata,"net_metering_mapped.csv"), row.names = F)

# make this an sf object
nm_mapped <- st_sf(nm_mapped, crs = crs(states_sf))


# export for tableau or for later, tableau kinda sucks tbh
st_write(nm_mapped, paste0(dir$shp,"net_metering_mapped_2.shp"),append = F )

# locate the centroids and export
nm_mapped <- st_centroid(nm_mapped)
df <- st_coordinates(nm_mapped)
colnames(df) <- c("cent_longitude","cent_latitude")
nm_mapped <- cbind(nm_mapped,df)
nm_wcoords <- st_drop_geometry(nm_mapped)

# lowercase the columns
colnames(nm_wcoords) <- tolower(colnames(nm_wcoords))

# rearrange
nm_wcoords <- arrange(nm_wcoords,year,state_id)

# export
write.csv(nm_wcoords, paste0(dir$cleandata,"res_nm_wcov_coords.csv"),
          row.names = F)

g_names <- colnames(nm_mapped)
print(g_names)
#### Net metering per state map -----------------------------
# g_names <- c("year","month" , "date","state","state_id", "nm_per_cust",            
#              "res_customers", "energy_sold_back_kwh","avg_temp_f",          
#              "anomaly","per_capita_inc","annual_gdp" ,         
#              "cent_kwh" , "NAME" , "ALAND","AWATER","geometry" )

# resolution differs between 500k, 5m, and 20m, lets go with 5m
states_sf <- st_read(paste0(dir$shp,"cb_2018_us_state_5m.shp"))

# what is the crs? units of measurement?
crs(states_sf) # ESPG 4269, meters

# visualize the shapefile
plot(states_sf$geometry) # kinda small innit?

# remove dc, and territories
states_sf <- states_sf %>% filter(STUSPS %not_in% c("DC","PR","AS","GU","MP","VI"))


# AGAIN, visualize the shapefile but without Alakska or Hawaii, looks bigger right?
plot(states_sf[states_sf$STUSPS %not_in% c("HI","AK"),]$geometry)


# Alaska is the problem child, so lets clip off that last bit way in the east
plot(states_sf[states_sf$STUSPS== "AK",]$geometry)
ak <- states_sf[states_sf$STUSPS== "AK",]
ak_clipped <- ms_clip(ak,bbox = c(-180,50,-130,80))
plot(ak_clipped$geometry)

# shift it down to present with the rest of the data, same with hawaii
ak_clipped$geometry <- st_geometry(ak_clipped) + c(5, -20)  # Adjust the coordinates as needed
hi <- states_sf[states_sf$STUSPS == "HI",]
hi$geometry <- st_geometry(hi) + c(15,10) # adjust right and up
# replace geometry in states_sf
states_sf[states_sf$STUSPS== "AK",]$geometry <- ak_clipped$geometry
states_sf[states_sf$STUSPS== "HI",]$geometry <- hi$geometry

# maybe make alaska smaller?
# p1 <- st_buffer(ak_clipped$geometry, dist = -0.1, joinStyle  = "MITRE", mitreLimit = 2)
# plot(p1)


# now plot the entire US of A Baby!!!
plot(states_sf$geometry) # now that looks better

# write st for future use
st_write(states_sf,paste0(dir$shp,"shifted_us_states.shp"), append = F)

nm_all_res <- read.csv(paste0(dir$cleandata,"res_net_metering_wcov_2dec2024.csv"))
nm_all_res$date <- as.Date(nm_all_res$date)

# remove the already added coordinates (from doing this before)
nm_all_res <- nm_all_res[-c(14:18)]

# join states_sf with nm_all_res
nm_mapped <- left_join(nm_all_res,states_sf[c(5:6,8:10)], join_by("state" == "STUSPS"))



# visualize the average net metering by customer state by year
for(y in 2011:2022){ # y <- 2011

    d <- nm_mapped %>% filter(year == y) %>%
      group_by(state, geometry) %>% 
      summarise(avg_nm_pc = mean(nm_per_cust))
    d <- st_as_sf(d, crs = crs(states_sf))
    d <- as_Spatial(d)
    
    png(paste0(dir$fig,"energy sold back by state/",
               "energy_sold_back_",y,".png"))
    print(spplot(d,"avg_nm_pc", 
                 main = paste("Avg. energy sold back per customer:",y),
                 xlab = "kWh/Residents", colorkey = list(space = "bottom")))
    dev.off()
  print(paste(y," is done!"))
}



##### map for rmarkdown -------------------

# ```{r mapping_lib, message=FALSE, warning=FALSE, include=FALSE}
# library(sf)
# library(spatial)
# library(sp)
# 
# # resolution differs between 500k, 5m, and 20m, lets go with 5m
# states_sf <- st_read(paste0(dir$shp,"shifted_us_states.shp"))
# ```


# ```{r map, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Net Metering by State", fig.height= 5}
# nm_all_res <- read.csv(paste0(dir$cleandata,"res_net_metering_wcov_2dec2024.csv"))
# nm_all_res$date <- as.Date(nm_all_res$date)
# 
# # remove the already added coordinates (from doing this before)
# nm_mapped <- nm_all_res[-c(7:18)]
# 
# # join states_sf with nm_all_res
# nm_mapped <- left_join(nm_mapped,states_sf[c(5:6,8:10)], join_by("state" == "STUSPS"))
# 
# # visualize the average net metering by customer state by year
# y <- 2016
# 
# d <- nm_mapped %>% filter(year == y) %>%
#   group_by(state, geometry) %>%
#   summarise(avg_nm_pc = mean(nm_per_cust))
# d <- st_as_sf(d, crs = 4269)
# d <- as_Spatial(d)
# spplot(d,"avg_nm_pc",
#        main = paste("Annual Avg. NMPC:",y),
#        xlab = "kWh/Residents",
#        colorkey = list(space = "bottom"))
# 
# ```


# if you dont want to use a chunk
# ![NMPC by State.](C:/Users/jesus/Box/Energy/net_metering/figures/map_2016.png)
