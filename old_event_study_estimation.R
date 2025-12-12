# Event study estimation (Robustness)



#### R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "png",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi",
          "zoo","Synth","flextable")

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

###### estimation data ---------------------------------------------------------------
# bring in state level data
nm_all_res <- read.csv(paste0(dir$cleandata,'res_net_metering_wcov_2024_12_05_zeros_edited.csv'))

# aggregate using utility data
utility_all <- read.csv(paste0(dir$cleandata,"utility_nm_full_covariates_10apr2025.csv"))

# add the cloud coverage with 75% coverage
tcc_75 <- read.csv(paste0(dir$cleandata, "monthly_cloudy_days_utility_all_75pcnt_cov.csv"))

# join
utility_all <- left_join(utility_all,tcc_75, by = c("year","month","full_id","full_id_y"))

utility_all <- rename(utility_all, "cloudy_days_50" = "cloudy_days.x", "cloudy_days_75" = "cloudy_days.y")

# drop the state_id 
utility_all <- utility_all %>% select(-state_id)

state_agg <- utility_all %>%
  group_by(year,month,state) %>%
  summarise(
    total_kwh_sold_back = sum(energy_sold_back_kwh, na.rm = T),
    esbpc_kwh_pc = sum(energy_sold_back_kwh, na.rm = T)/sum(res_customers, na.rm = T), #ESBPC = energy sold back per customer (in kwh per customer)
    pcnt_solar_cust = sum(res_customers)/sum(customers),
    avg_cloudy_days_50 = mean(cloudy_days_50, na.rm = T),
    avg_cloudy_days_75 = mean(cloudy_days_75, na.rm = T),
    avg_tcc = mean(avg_tcc, na.rm = T),
    avg_consumption_kwh = mean(avg_consumption_kwh, na.rm = T),
    avg_temp = mean(avg_temp, na.rm = T),
    avg_med_inc = mean(med_inc, na.rm = T),
    avg_eng_price = mean(usd_per_kwh, na.rm = T),
    n_utilities = length(unique(utility_id))
  )

# some things should combine (temperature, annual_gdp, annual_med_inc_per_cap)
state_agg <- left_join(state_agg, nm_all_res[c("year","month","state","avg_temp_f","precipitation_in","per_capita_inc","annual_gdp")], by = c("year","month","state"))

# if avg_temp is NA, use avg_temp_f
state_agg$avg_temp <- ifelse(is.na(state_agg$avg_temp), state_agg$avg_temp_f, state_agg$avg_temp)

# if per_captia_inc is NA, use avg_med_inc
state_agg$per_capita_inc <- ifelse(is.na(state_agg$per_capita_inc), state_agg$avg_med_inc, state_agg$per_capita_inc)


#drop the state_id in nm_all_res
nm_all_res <- nm_all_res[-c(5)]

# drop the water
nm_all_res <- nm_all_res[-c(9,11,13,16)]

# turn cents into dollars
nm_all_res$cent_kwh <- nm_all_res$cent_kwh/100
nm_all_res <- rename(nm_all_res, "usd_per_kwh" = "cent_kwh")

# add dc
nm_all_res <- rbind(nm_all_res, dc)


# reduce the dataset
nm_all_res <- nm_all_res[nm_all_res$year >2013 & nm_all_res$year < 2020,]

# drop AK, HI, CA, GA, MI, MO, NE, ND, TN, SD, AL, TX, ID, WI, NV, KS
s_drop <- c("AK", "HI","AL", "CA", "GA", "MI", "MO", "NE", "ND", "TN", "SD", "AL", "TX", "ID", "WI", "NV", "KS")

nm_states <- nm_all_res[nm_all_res$state %not_in% s_drop,]

# set the periods 
t0 <- length(unique(nm_states$state)) -1
tn <- length(unique(nm_states$date))

# assign the new ID's
nm_states <- arrange(nm_states,date, state)

az <- nm_states[nm_states$state == "AZ",]
az$state_id <- 1

nm_states2 <- nm_states[nm_states$state != "AZ",]
state_list <- unique(nm_states2$state)
state_id <- c(2:35)
state_id <- as.data.frame(cbind(state_list,state_id))
nm_states2 <- left_join(nm_states2,state_id, by = c("state" ="state_list"))

nm_states <- rbind(az,nm_states2)
nm_states$state_id <- as.numeric(nm_states$state_id)
nm_states <- arrange(nm_states, date, state_id)

# make a date numeric value
nm_states$date <- date(nm_states$date)

u_date <- as.data.frame(unique(nm_states$date))
u_date$date_numeric <- 1:length(u_date$`unique(nm_states$date)`)
colnames(u_date) <- c("date","date_numeric")
nm_states <- left_join(nm_states,u_date, by = "date")