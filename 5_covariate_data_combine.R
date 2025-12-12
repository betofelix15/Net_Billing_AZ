# Title: Covariates for net metering 
# Author: Jesus Felix
# Start date: 11 Aug 2025
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
          "zoo","tigris")

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


##### 1. Residential Load data ----------------------------------------------------------------------------------
# load data
nm <- read.csv(paste0(dir$cleandata,"clean_res_solar_wind_nm_utility_aug_4_2025.cvs"))

# load shapefile for utility mapping
u_sf <-  st_read(paste0(dir$shp,"utility_boundaries_prices.shp"))

names <- c("utility_id","utility","state", "service_type","ownership","year","full_id","full_id_y","annual_res_cents_kwh","annual_comm_cents_kwh","annual_ind_cents_kwh","geometry")

colnames(u_sf) <- names

# wwhich utitlies are found in nm but not in u_sf?
list_nm <- unique(nm$utility_id)
list_sf <- unique(u_sf$utility_id)

list_nm[list_nm %not_in% list_sf]

##### Residential monthly prices -------------------------------------------------------------------

# load data
#load the sales and revenue data (residential)
u_sales <- NULL
for(y in 2011:2024){ 
  
  if(y < 2013){ # y <- 2011
    
    df <- read_excel(paste0(dir$rawdata,"revenue_utility_data/utility_sales_rev_",y,".xls"))
    df <- df[c(1:8)]
  }
  if(y == 2013){# y <- 2013
    df <- read_excel(paste0(dir$rawdata,"revenue_utility_data/utility_sales_rev_",y,".xls"), sheet = 1, skip = 2)
    df <- df %>% select(`Utility Number`,`Utility Name`,State,Year,Month,`Thousands Dollars...7`,Megawatthours...8,Count...9)
  }
  if(y > 2013 & y < 2017){# y <- 2013
    df <- read_excel(paste0(dir$rawdata,"revenue_utility_data/utility_sales_rev_",y,".xls"), sheet = 1, skip = 2)
    df <- df %>% select(`Utility Number`,`Utility Name`,State,Year,Month,`Thousands Dollars...8`,Megawatthours...9,Count...10)
  }
  if(y > 2016 ){ # y <- 2017
    df <- read_xlsx(paste0(dir$rawdata,"revenue_utility_data/utility_sales_rev_",y,".xlsx"), sheet = 1, skip = 2)
    df <- df %>% select(`Utility Number`,`Utility Name`,State,Year,Month,`Thousands Dollars...8`,Megawatthours...9,Count...10)
  }
  
  colnames(df) <- c("utility_id","utility","state","year","month","revenue_1k_usd","sales_mwh","customers") 
  df[c(6:8)] <- lapply(df[c(6:8)],as.numeric) 
  
  u_sales <- rbind(u_sales,df)
  
}


# remove the adjustments and totals
u_sales <- u_sales[u_sales$utility_id != 0,]
u_sales <- u_sales[u_sales$utility_id != 88888,]

# drop the NA's
u_sales <- u_sales[!is.na(u_sales$utility_id),]

# create unit price, average price and average consumption columns
u_sales$avg_price_cents_per_kwh <- u_sales$revenue_1k_usd/u_sales$sales_mwh*100

u_sales$avg_rev_usd_pc <- (u_sales$revenue_1k_usd*1000)/u_sales$customers

u_sales$avg_consumption_kwh_pc <- (u_sales$sales_mwh*1000)/u_sales$customers

# clear the NA's
u_sales$avg_price_cents_per_kwh <- ifelse(is.na(u_sales$avg_price_cents_per_kwh),0,u_sales$avg_price_cents_per_kwh)
u_sales$avg_rev_usd_pc <- ifelse(is.na(u_sales$avg_rev_usd_pc),0,u_sales$avg_rev_usd_pc)
u_sales$avg_consumption_kwh_pc <- ifelse(is.na(u_sales$avg_consumption_kwh_pc),0,u_sales$avg_consumption_kwh_pc)

u_sales$revenue_1k_usd <- ifelse(is.na(u_sales$revenue_1k_usd),0,u_sales$revenue_1k_usd)
u_sales$sales_mwh <- ifelse(is.na(u_sales$sales_mwh),0,u_sales$sales_mwh)
u_sales$customers <- ifelse(is.na(u_sales$customers),0,u_sales$customers)

# create an identifier for utility and state and another for utility, state, and year
u_sales$full_id <- paste0(u_sales$utility_id,"_",u_sales$state) # this is wrong
u_sales$full_id_y <- paste(u_sales$utility_id,u_sales$state,u_sales$year, sep = "_")

# export this set
write.csv(u_sales,
          paste0(dir$cleandata,"unit_prices_residential_utilities.csv"),
          row.names = F)


##### 3. Income information ----------------------------------------------------------------------------


# get the geographic boundaries for the zipcodes 
# find all zip_codes for the state 
options(tigris_use_cache = TRUE)

zip_sf <- zctas(year = 2022)

zip_sf <- zip_sf[c("GEOID20","geometry")]

zip_sf <- rename(zip_sf, "zipcode" = 1)

# we want the centroids of the zipcodes
zip_sf$geometry <- st_centroid(zip_sf$geometry)

# you can skip this go straing to loading data (read in)
# ## now load the median income data
# z_income <- NULL
# for(y in 2011:2023){ # y <- 2017
#   
#   # load data for year y
#   df <- read.csv(paste0(dir$rawdata,"income_data_zipcode/ACSST5Y",y,".S1903-Data.csv"))
#   
#   if(y < 2017){
#     # remove 1st row,  keep only the median income column and identifier (needs fixing!!!)
#     df <- df[-1,c("NAME","S1903_C02_001E")]
#     
#   }
#   else{ 
#     # this group does not have a specific median income for all 
#     df <- df[-1,c("NAME","S1903_C03_015E","S1903_C03_034E")]
#     df[c(2:3)] <- lapply(df[c(2:3)],as.numeric) 
#     
#     # we have to use the mean of family and non-family groups
#     df$med_income <- (df$S1903_C03_015E+df$S1903_C03_034E)/2
#     
#     # remove the other two 
#     df <- df[,-c(2,3)]
#   }
#   # add year
#   df$year <- y
#   
#   # rename columns
#   colnames(df) <- c("zipcode","med_income","year")
#   
#   # append together
#   z_income <- rbind(z_income,df)
#   
# }
# 
# 
# # distinguish the zipcode by splitting the first column
# z_income <- z_income %>% separate(zipcode, c("ztas","zipcode")) %>%
#   select(-ztas)
# 
# # make the med income variable numerical
# z_income$med_income <- as.numeric(z_income$med_income)

# export
#write.csv(z_income, paste0(dir$cleandata,"med_income_zipcode.csv"),
#          row.names = F)

# read in
z_income <- read.csv( paste0(dir$cleandata,"med_income_zipcode.csv"))
# add leading zeros to z_income and change to character
z_income$zipcode <- str_pad(z_income$zipcode,width = 5, pad = 0, side = "left")
z_income$zipcode <- as.character(z_income$zipcode)

# combine shapefile and income data
income_sf <- left_join(z_income, zip_sf, by = "zipcode")

income_sf <- st_as_sf(income_sf)
# remove the empty polygons
income_sf <- income_sf %>% filter(!st_is_empty(.))

## Now, using the shapefile with the utility polygons, find the zipcodes within those
# polygons and provide the mean of the median income within those boundaries

id_y <- as.list(u_sf$full_id_y)

u_income <- NULL  
for(i in id_y){ # i <- "MA_11804_2011"
  
  i_sf <- u_sf[u_sf$full_id_y == i,]
  
  inc_y <- income_sf[income_sf$year == unique(i_sf$year),]
  
  int <- st_intersects(i_sf$geometry,inc_y$geometry)
  
  zip_int <- inc_y[int[[1]],]
  
  df <- st_drop_geometry(i_sf)
  
  df$med_inc <- median(zip_int$med_income, na.rm = T)
  df$avg_inc <- mean(zip_int$med_income, na.rm = T)
  df$sd_dev_med_inc <- sd(zip_int$med_income, na.rm = T)
  
  # save the values
  u_income <- rbind(u_income, df)
  
  print(paste0(i," complete"))
}  


# export this set
write.csv(u_income[c(1:8,12:14)],
          paste0(dir$cleandata,"annual_median_income_within_utility_bounds.csv"),
          row.names = F)

### utility dynamic pricing -----------------------------------------------------

dy_price <- NULL
for(y in 2013:2023){

  if(y < 2015){
    
    dyprice_y <- read_xls(paste0(dir$rawdata,"form_861_annual/",y,"/Dynamic_Pricing",y,".xls"))
    
    dyprice_y <- dyprice_y[-c(1:2),c(1:5,10)]
    colnames(dyprice_y) <- c("year","utility_id","utility_name","state","customers_enrolled","TOU")
    dyprice_y$customers_enrolled <- as.numeric(dyprice_y$customers_enrolled)
  
    }
  
  if(y > 2014 & y < 2019){
    
    dyprice_y <- read_xlsx(paste0(dir$rawdata,"form_861_annual/",y,"/Dynamic_Pricing_",y,".xlsx"))
    dyprice_y <- dyprice_y[-c(1:2),c(1:4,6,11)]
    colnames(dyprice_y) <- c("year","utility_id","utility_name","state","customers_enrolled","TOU")
    dyprice_y$customers_enrolled <- as.numeric(dyprice_y$customers_enrolled)
    
  }

  
  if(y > 2018){
  
    dyprice_y <- read_xlsx(paste0(dir$rawdata,"form_861_annual/",y,"/Dynamic_Pricing_",y,".xlsx"))
    dyprice_y <- dyprice_y[-c(1:2),c(1:3,5,7,12)]
    colnames(dyprice_y) <- c("year","utility_id","utility_name","state","customers_enrolled","TOU")
    dyprice_y$customers_enrolled <- as.numeric(dyprice_y$customers_enrolled)
    
  }
  
  # save 
  dy_price <- rbind(dy_price,dyprice_y)

}

#NA's in number enrolled

dy_price$customers_enrolled <- ifelse(is.na(dy_price$customers_enrolled),0,dy_price$customers_enrolled)

# make TOU a numeric dummy
dy_price$TOU <- ifelse(dy_price$TOU == "Y",1,0)
dy_price$TOU <- ifelse(is.na(dy_price$TOU),0,dy_price$TOU)

# make full_id and full_id_y
dy_price$full_id <- paste0(dy_price$state,"_",dy_price$utility_id)
dy_price$full_id_y <- paste0(dy_price$full_id,"_",dy_price$year)

# remove withheld
dy_price <- dy_price %>%  filter(utility_name != "Withheld")


# export
write.csv(dy_price,
          paste0(dir$cleandata, "dynamic_pricing_utilities_2013_2023.csv"),
          row.names = F)

#### Combine the covariates (including EE covariates)---------------------------------------------
monthly_cloud <- read.csv(paste0(dir$cleandata, "monthly_cloudy_days_utility_all_75pcnt_cov.csv"))

# air temp missing needs to be aggregated to month and combined with other air temp

monthly_air_temp <- read.csv(paste0(dir$cleandata, "monthly_air_temp_utilities_kelvin.csv"))
missing_air_temp <- read.csv(paste0(dir$cleandata, "missing_monthly_air_temp_utilities_kelvin.csv"))
# there is an outrageously high values that need removing (im talking 500000000000000 kelvins)
missing_air_temp <- missing_air_temp[missing_air_temp$avg_temp < 500,]

missing_sum <- missing_air_temp %>%
  group_by(full_id_y, year, month) %>%
  summarize( median_temp = median(median_temp, na.rm = T),
             avg_temp = mean(avg_temp, na.rm = T))
monthly_air_temp <- rbind(monthly_air_temp[c(3:7)],missing_sum)


unit_prices <- read.csv(paste0(dir$cleandata, "unit_prices_residential_utilities.csv"))
unit_prices$full_id_y <- paste(unit_prices$state,unit_prices$utility_id,unit_prices$year, 
                               sep = "_")
# remove option of bundled and delivery (causes duplicates)
median_income <- read.csv(paste0(dir$cleandata, "annual_median_income_within_utility_bounds.csv"))
median_income <- median_income %>%
  select(year,full_id_y,med_inc,avg_inc,sd_dev_med_inc,ownership) %>%
  group_by(full_id_y) %>%
  slice(1) %>%
  ungroup()
median_income$IOU <- ifelse(median_income$ownership == "Investor Owned",1,0) # make dummy for investor owned utilities

dy_price <- read.csv(paste0(dir$cleandata, "dynamic_pricing_utilities_2013_2023.csv"))

# create the full_id_y
nm$full_id <- paste0(nm$state,"_",nm$utility_id)
nm$full_id_y <- paste0(nm$full_id,"_",nm$year)

# join them

utility_join <- left_join(nm, monthly_cloud, by = c("full_id","full_id_y","year","month"))

utility_join <- left_join(utility_join,monthly_air_temp, by = c("full_id_y","year","month"))

utility_join <- left_join(utility_join,median_income, by = c("full_id_y","year"))

utility_join <- left_join(utility_join,unit_prices[c(4:11,13)], by = c("full_id_y","year","month"))

utility_join <- left_join(utility_join,dy_price[c(1,5:8)], by = c("full_id_y","full_id","year"))


# export this
write.csv(utility_join,
          paste0(dir$cleandata, "utility_nm_full_covariates_18aug2025.csv"),
          row.names = F)

