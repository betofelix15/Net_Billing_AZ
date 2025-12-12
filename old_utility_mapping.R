# Mapping utility locations
# Feb 9, 2025
# Author: Jesus Felix

## Pre-requesites ----------------------------------------------------------------
# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "tidygeocoder",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi",
          "zoo","gmapsdistance","rmapshaper", "gridExtra","tigris")

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

## 1. Data prep (last updated feb 11 2025) -----------------------------------------

# load data
utility_nm <- read.csv(paste0(dir$cleandata,"net_metering_utility_residential.csv"))

# replace ut_id with a new character (full_id)
utility_nm$full_id <- paste(utility_nm$state,utility_nm$utility_id, sep = "_")
  
# list the unique utilities
utility_list <- unique(utility_nm[c("state","utility_id","utility","full_id","year")])
# keep the year: some utilities change name, its best to keep the latest name

# remove NA's
utility_list <- drop_na(utility_list)

# remove duplicates, sometimes utilities change name (i.e., inc to LLC)
dup <- utility_list[duplicated(utility_list$full_id),]

# get all duplicates
dup1 <- utility_list[utility_list$full_id %in% dup$full_id,]

# number them (1 - original,2 - 2nd copy, etc.)
dup2 <- dup1 %>%
  group_by(full_id) %>%
  mutate(num_sim_obs = n(),
         dup_id = row_number()) %>%
  ungroup() %>%
  mutate(is_duplicate = dup_id > 1)

# keep the last duplicate
dup3 <- dup2 %>%
  group_by(full_id) %>%
  filter(dup_id == max(dup_id))


# for all the names in the utility duplicates, only keep the last one
# do this for full nm data
dup3 <- dup3[c("full_id", "utility")]

utility_nm2 <- left_join(utility_nm,dup3, by = "full_id")

# utility.x is original name, we want utility.y, unless it is NA
utility_nm2$utility_name <- ifelse(is.na(utility_nm2$utility.y),utility_nm2$utility.x,
                              utility_nm2$utility.y)

# remove utility.x and utility.y
utility_nm2 <- utility_nm2 %>% select(-c("utility.x","utility.y"))

# make nm_list 
nm_list <- unique(utility_nm2[c("state","utility_id","utility_name","full_id")])

# export these
 write.csv(nm_list, paste0(dir$cleandata,"utility_list_12_feb_2025.csv"), row.names = F)
# 
 write.csv(utility_nm2, paste0(dir$cleandata,"utility_res_net_metering_12_feb_2025.csv"), row.names = F)

# read in these files
nm_list <- read.csv(paste0(dir$cleandata,"utility_list_12_feb_2025.csv"))
u_nm <- read.csv(paste0(dir$cleandata,"utility_res_net_metering_12_feb_2025.csv"))
## 2. Find Coordinates for cities ----------------------------------------------------

# not all utilities have coordinates. Also, utilities cover area, not points...
# hey, I have a list of utilities and zipcodes they align to by year. 
# maybe we can loop through each utility, by year and see the boundaries...

# load the data
utlty_zip <- NULL
for(i in c(2011,2013:2023)){
  zf <- read.csv(paste0(dir$rawdata,"iouzipcodes",i,".csv"))
  zf$year <- i
  
  utlty_zip <- rbind(zf,utlty_zip)
  
}

# 2012 is missing so fill it in with 2013
z_12 <- utlty_zip[utlty_zip$year == 2013,] # filter 2013 out
z_12$year <- 2012 # change the year

utlty_zip <- rbind(utlty_zip,z_12) # bind to full set

# there are similar utility ID's in different states, so we need a full id to 
# distinguish these
utlty_zip$full_id <- paste(utlty_zip$state,utlty_zip$eiaid, sep = "_")

# do the same thing with duplicates here that we did to nm data
zip_list <- unique(utlty_zip[c("state","eiaid","utility_name","full_id")])
# remove duplicates, sometimes utilities change name (i.e., inc to LLC)
dup <- zip_list[duplicated(zip_list$full_id),]

# get all duplicates
dup1 <- zip_list[zip_list$full_id %in% dup$full_id,]

# number them (1 - original,2 - 2nd copy, etc.)
dup2 <- dup1 %>%
  group_by(full_id) %>%
  mutate(num_sim_obs = n(),
         dup_id = row_number()) %>%
  ungroup() %>%
  mutate(is_duplicate = dup_id > 1)

# keep the last duplicate
dup3 <- dup2 %>%
  group_by(full_id) %>%
  filter(dup_id == max(dup_id))


# for all the names in the utility duplicates, only keep the last one
# do this for full zip data
dup3 <- dup3[c("full_id", "utility_name")]

utlty_zip2 <- left_join(utlty_zip,dup3, by = "full_id")

# utility.x is original name, we want utility.y, unless it is NA
utlty_zip2$utility_name <- ifelse(is.na(utlty_zip2$utility_name.y),utlty_zip2$utility_name.x,
                                  utlty_zip2$utility_name.y)

# remove utility.x and utility.y
utlty_zip2 <- utlty_zip2 %>% select(-c("utility_name.x","utility_name.y"))
# make the utlty_zip2 into u_zip for easier
u_zip <- utlty_zip2

# we can get rid of earlier sets
rm(list = c("dup","dup1","dup2","dup3","utlty_zip","zf","utlty_zip2"))

# lets see how nm data and zip match up
zip_list <- unique(u_zip[c("state","eiaid","utility_name","full_id")]) %>%
  arrange(state)
# do the same for 

# make a list of the utilities found in utlty_zip_list and utility_list
# match by name and utility id 
match_list_id <- nm_list[nm_list$full_id %in% zip_list$full_id,]
match_list_name <- nm_list[nm_list$utility_name %in% zip_list$utility_name,]

# which are not found together?
unmatch_list <- nm_list[nm_list$full_id %not_in% 
                          match_list_id$full_id,]

# We can use match_list_id, it has the most observations

# lets get sf file with all the polygons for each zip code in u.s.
# find all zip_codes for the state 
options(tigris_use_cache = TRUE)
zip_sf <- zctas(year = 2022)
zip_sf$GEOID20 <- as.numeric(zip_sf$GEOID20)
# in each year, give me all the zip codes the utility covers and make a polygon accordingly
# also save the other variables (ownership, res_price, etc.)

# join the Zip sf file with the zip list for utilities
z_join <-  left_join(u_zip,zip_sf, by = c("zip" = "GEOID20")) # 351,669 obs

# drop the NA's, these are probably zip codes that only existed prior to 2022
z_join <- drop_na(z_join) # 294,717

# filter by the utilities found in match_list
z_join <- z_join[z_join$full_id %in% match_list_id$full_id,] # 289,327 obs

# # see if it works with this 
# AR_814 gives us a hard time because it covers so many zipcodes
temp_z <- z_join[z_join$full_id == "AR_814" &
                   z_join$year == 2015,]

temp_geom <- st_union(temp_z$geometry) # only took 1 minute


# summarize by year, because this can take a really long time,
# save each year so you can start again in the year you left off

for(y in 2011:2023){ # y <- 2015 , took approx. 30 min for 1 year
  sum_u <- NULL
    # 2013 already done
  for(u in as.list(match_list_id$full_id)){ # u <- "AR_814"
    
      temp_z <- z_join[z_join$full_id == u & z_join$year == y,]
      
      # some may not appear in the selected year, so make an if statement
      if(length(temp_z$eiaid) == 0){ # if temp_z is empty, move on to next
        next
      }
      
      
      u_z <- unique(temp_z[c("year","state","eiaid","utility_name",
                             "full_id")])
      annual_comm_rate <- mean(temp_z$comm_rate) # rates should be similar if not same
      annual_ind_rate <- mean(temp_z$ind_rate)
      annual_res_rate <- mean(temp_z$res_rate)
      land_sqm <- sum(temp_z$ALAND20) # land and water coverage in squared meters(sqm)
      water_sqm <- sum(temp_z$AWATER20)
      boundary <- st_union(temp_z$geometry)
      
      u_z <- cbind(u_z,annual_comm_rate,annual_ind_rate,annual_res_rate,
                   land_sqm,water_sqm,boundary)
      sum_u <- rbind(sum_u,u_z)
      
      print(paste0(u," for ",y," is done!"))
    }

  sf_sum <- st_as_sf(sum_u)

  st_write(sf_sum, paste0(dir$shp,y,"_utility_boundaries_annual.shp"),
           append = F)
  
  print(paste0(y," is done!"))
}


z_sf <-NULL
# now append them
for(y in 2011:2023){ # y <- 2013
  
  sf <- st_read(paste0(dir$shp,y,"_utility_boundaries_annual.shp"))
  
  z_sf <- rbind(z_sf,sf)
}

# st_write(z_sf,paste0(dir$shp,"utility_boundaries_all_years.shp"), append = F)

# read in this file
z_sf <- st_read(paste0(dir$shp,"utility_boundaries_all_years.shp"))

# combine with the nm data
nm_utility <- read.csv(paste0(dir$cleandata,"utility_res_net_metering_12_feb_2025.csv"))


z_sf$full_id_y <- paste0(z_sf$full_id, "_",z_sf$year)

z_join <- left_join(nm_utility,z_sf[6:12], by = "full_id_y")

###### find the utilities with missing geometries ----------------------------------------------

options(tigris_use_cache = TRUE)
zip_sf <- zctas(year = 2022)
zip_sf$GEOID20 <- as.numeric(zip_sf$GEOID20)

z_miss <- z_join[st_is_empty(z_join$geometry),]

unique(z_miss$full_id) # 145 utilities with missing geographies

z_miss <- st_drop_geometry(z_miss[c(1,3,4,10:12)])
z_miss <- unique(z_miss)
z_miss <- z_miss[z_miss$year >= 2014 & 
                   z_miss$year <= 2020,]

# These are from non-investor owned utilities

# load the data
n_zip <- NULL
for(i in c(2014:2020)){
  zf <- read.csv(paste0(dir$rawdata,"non_iou_zipcodes_",i,".csv"))
  zf$year <- i
  
  n_zip <- rbind(zf,n_zip)
  
}

# to make things quick, lets just stick to 2014 to 2020
miss_list <- as.list(z_miss$full_id_y)



# there are similar utility ID's in different states, so we need a full id to 
# distinguish these
n_zip$full_id <- paste(n_zip$state,n_zip$eiaid, sep = "_")
n_zip$full_id_y <- paste(n_zip$state,n_zip$eiaid,n_zip$year,sep = "_")

# join with z_miss
z_join_m <- left_join(z_miss,n_zip[-c(3)], by = c("full_id_y", "full_id", "year","state"))

# join with zipcode geometry
z_join_m <- left_join(z_join_m, zip_sf, by = c("zip" = "GEOID20"))
z_join_m <- st_as_sf(z_join_m)

# now consolidate for each utility
zip_miss_geom <- NULL
for(u in miss_list){ # u <- "AZ_19189_2014"
    
    temp_z <- z_join_m[z_join_m$full_id_y == u,]
    
    # some may not appear in the selected year, so make an if statement
    if(length(temp_z$eiaid) == 0){ # if temp_z is empty, move on to next
      next
    }
    
    u_z <- unique(st_drop_geometry(temp_z[c("year","state","eiaid","utility_name",
                           "full_id","full_id_y")]))
    annual_comm_rate <- mean(temp_z$comm_rate, na.rm = T) # rates should be similar if not same
    annual_ind_rate <- mean(temp_z$ind_rate, na.rm = T)
    annual_res_rate <- mean(temp_z$res_rate, na.rm = T)
    land_sqm <- sum(temp_z$ALAND20, na.rm = T) # land and water coverage in squared meters(sqm)
    water_sqm <- sum(temp_z$AWATER20, na.rm= T)
    boundary <- st_union(temp_z$geometry)
    
    u_z <- cbind(u_z,annual_comm_rate,annual_ind_rate,annual_res_rate,
                 land_sqm,water_sqm,boundary)
    zip_miss_geom <- rbind(zip_miss_geom,u_z)
    
    print(paste0(u," is done!"))

}

st_write(zip_miss_geom,paste0(dir$shp,"utility_boundaries_all_years_missing.shp"), append = F)



# can we vizuualize the data?

library(sp)


















