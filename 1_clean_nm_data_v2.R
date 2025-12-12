# Title: clean NM Data_version 2
# Author: Jesus Felix
# Start date: 8 Aug 2025
# Created to focus on utility differences: There are Investor-Owned Utilities (IOU) and 
# Non-investor owned utilities (NOIU). These were affected differently by the policy.
# The state level aggregation is therefore flawed in this sense. \
### Important acronyms:
# IOU: Investor-Owned Utilities 
# NIOU: non-Investor-Owned Utilities 
# ESB: Energy Sold Back (to utility company)
# MWH: mega-watt hours (measure of energy)
# KWH: Kilo-watt hours (measure of energy, commonly used in bills)
# INMC: installed net metering capacity
# TPO: Third party owners
##################################~~~~~~~############################################~
##################################~~~~~~~############################################~
##################################~~~~~~~############################################~
#### 0.R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi")

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

`%not_in%` <- negate(`%in%`)

#### 1. Load all data (utility level) --------------------------------------------------------------------

for(i in 2011:2013){ # i = 2013
  filename <- paste0("nm_",i)
  wd <- paste0(dir$rawdata,"net_metering_",i,".xls")
  assign(filename, read_excel(wd, sheet = 1))
} 

nm_2014 <- read_excel(paste0(dir$rawdata,"net_metering_2014.xls"), sheet = 1)
nm_2015 <- read_excel(paste0(dir$rawdata,"net_metering_2015.xls"), sheet = 1)


# from 2016 to 2022, they used xlxs files
for(i in 2016:2024){ # i = 2016
  filename <- paste0("nm_",i)
  wd <- paste0(dir$rawdata,"net_metering_",i,".xlsx")
  assign(filename, read_xlsx(wd, sheet = 1))
} 



#### 2. Residential (Solar and Wind)  --------------------------------------------------------------------


# from 2011 to 2012 same setup: select residential net metering (energy sold back) and customers

# 2011
df_11 <- nm_2011 %>%
  select(YEAR,MONTH,STATE,`UTILITY CODE`,`UTILITY NAME`,`RESIDENTIAL  PHOTOVOLTAIC ELECTRIC ENERGY SOLD BACK (MWh)`,
         `RESIDENTIAL PHOTOVOLTAIC INSTALLED NET METERING CAPACITY (MW)`,`RESIDENTIAL PHOTOVOLTAIC NET METERING CUSTOMER COUNT`,
         `RESIDENTIAL  WIND ELECTRIC ENERGY SOLD BACK (MWh)`,`RESIDENTIAL WIND INSTALLED NET METERING CAPACITY (MW)`,
         `RESIDENTIAL WIND NET METERING CUSTOMER COUNT`, `RESIDENTIAL  TOTAL ENERGY SOLD BACK TO THE UTILITY (MWh)`,
         `RESIDENTIAL TOTAL  INSTALLED NET METERING CAPACITY (MW)`,`RESIDENTIAL TOTAL  NET METERING CUSTOMER COUNT`) 

# 2012
df_12 <- nm_2012 %>%
  select(YEAR,MONTH,STATE,`UTILITY CODE`,`UTILITY NAME`,`RESIDENTIAL  PHOTOVOLTAIC ELECTRIC ENERGY SOLD BACK (MWh)`,
         `RESIDENTIAL PHOTOVOLTAIC INSTALLED NET METERING CAPACITY (MW)`,`RESIDENTIAL PHOTOVOLTAIC NET METERING CUSTOMER COUNT`,
         `RESIDENTIAL  WIND ELECTRIC ENERGY SOLD BACK (MWh)`,`RESIDENTIAL WIND INSTALLED NET METERING CAPACITY (MW)`,
         `RESIDENTIAL WIND NET METERING CUSTOMER COUNT`, `RESIDENTIAL  TOTAL ENERGY SOLD BACK TO THE UTILITY (MWh)`,
         `RESIDENTIAL INSTALLED NET METERING CAPACITY FOR ALL STATES SERVED(MW)`,`RESIDENTIAL TOTAL  NET METERING CUSTOMER COUNT`)


# 2013 to 2016 same setup
colunms <- c(1:5,17,7,12,32,22,27,62,52,57) # double check

df_13 <- nm_2013[c(4:nrow(nm_2013)),colunms]
df_14 <- nm_2014[c(4:nrow(nm_2014)),colunms]
df_15 <- nm_2015[c(4:nrow(nm_2015)),colunms] 
df_16 <- nm_2016[c(4:nrow(nm_2016)),colunms] 

# 2017  introduces batter storage and virtual net metering (columns : ,18,23,28,33)
colunms <- c(1:5,38,8,13,53,43,48,83,73,78) # double check

df_17 <- nm_2017[c(4:nrow(nm_2017)),colunms] 
df_18 <- nm_2018[c(4:nrow(nm_2018)),colunms] 
df_19 <- nm_2019[c(4:nrow(nm_2019)),colunms] 
df_20 <- nm_2020[c(4:nrow(nm_2020)),colunms] 
df_21 <- nm_2021[c(4:nrow(nm_2021)),colunms] 
df_22 <- nm_2022[c(4:nrow(nm_2022)),colunms] 
df_23 <- nm_2023[c(4:nrow(nm_2023)),colunms] 

# 2024 IS DIFFERENT
colunms <- c(1:5,18,8,13,83,73,78,113,103,108)
df_24 <- nm_2024[c(4:nrow(nm_2024)),colunms] 



# change their names
names <- c("year","month","state","utility_id","utility","solar_esb_mwh","solar_inmc_mw","solar_nm_customers",
           "wind_esb_mwh","wind_inmc_mw","wind_nm_customers","total_esb_mwh","total_inmc_mw","total_nm_customers")
for (i in 11:24) {
  df_name <- paste0("df_", i)
  df <- get(df_name)  # Get the data frame by name
  colnames(df) <- names  # Set column names
  assign(df_name, df)  # Save it back to the environment
}

# put them all together
nm_all <- rbind(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22,df_23,df_24)

# delete if you want
# rm(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22,df_23,df_24)

# remove adjustments or notes at the end (month would be NA)
nm_all <- nm_all[nm_all$utility_id != "99999",]
nm_all <- nm_all[!is.na(nm_all$month),]

# make numeric the appropriate columns
nm_all[c(1,2,4,6:14)] <- lapply(nm_all[c(1,2,4,6:14)], function(x) as.numeric(as.character(x)))

# there is an issue to be aware of: utility 17543 (month 3 2020) and 9273 (month 7 2020) have extremely high numbers that do not add up given their capacity
# we can impute a value that is the average of the year before and after based on their capacity, but we have to take note of this

imp_df <- nm_all[nm_all$utility_id %in% c(17543,9273) & nm_all$year %in% c(2019:2021),]
# definitely its a data entry error: decimal should move three paces back

nm_all[nm_all$utility_id == 17543 & nm_all$year == 2020 & nm_all$month == 3,]$solar_esb_mwh <- 328.192000
nm_all[nm_all$utility_id == 9273 & nm_all$year == 2020 & nm_all$month == 7,]$solar_esb_mwh <- 142.820000

# export
write.csv(nm_all,
          paste0(dir$cleandata,"clean_res_solar_wind_nm_utility_aug_4_2025.cvs"),
          row.names = F)

#### 3. Commercial (Solar and Wind)   --------------------------------------------------------------------


# from 2011 to 2012 same setup: select COMMERCIAL net metering (energy sold back) and customers

# 2011
df_11 <- nm_2011 %>%
  select(YEAR,MONTH,STATE,`UTILITY CODE`,`UTILITY NAME`,`COMMERCIAL  PHOTOVOLTAIC ELECTRIC ENERGY SOLD BACK (MWh)`,
         `COMMERCIAL PHOTOVOLTAIC INSTALLED NET METERING CAPACITY (MW)`,`COMMERCIAL PHOTOVOLTAIC NET METERING CUSTOMER COUNT`,
         `COMMERCIAL  WIND ELECTRIC ENERGY SOLD BACK (MWh)`,`COMMERCIAL WIND INSTALLED NET METERING CAPACITY (MW)`,
         `COMMERCIAL WIND NET METERING CUSTOMER COUNT`, `COMMERCIAL  TOTAL  ELECTRIC ENERGY SOLD BACK (MWh)`,
         `COMMERCIAL TOTAL  INSTALLED NET METERING CAPACITY (MW)`,`COMMERCIAL TOTAL  NET METERING CUSTOMER COUNT`) 

# 2012
df_12 <- nm_2012 %>%
  select(YEAR,MONTH,STATE,`UTILITY CODE`,`UTILITY NAME`,`COMMERCIAL  PHOTOVOLTAIC ELECTRIC ENERGY SOLD BACK (MWh)`,
         `COMMERCIAL PHOTOVOLTAIC INSTALLED NET METERING CAPACITY (MW)`,`COMMERCIAL PHOTOVOLTAIC NET METERING CUSTOMER COUNT`,
         `COMMERCIAL  WIND ELECTRIC ENERGY SOLD BACK (MWh)`,`COMMERCIAL WIND INSTALLED NET METERING CAPACITY (MW)`,
         `COMMERCIAL WIND NET METERING CUSTOMER COUNT`, `COMMERCIAL  TOTAL  ELECTRIC ENERGY SOLD BACK (MWh)`,
         `COMMERCIAL TOTAL  INSTALLED NET METERING CAPACITY (MW)`,`COMMERCIAL TOTAL  NET METERING CUSTOMER COUNT`) 

# 2013 to 2016 same setup
colunms <- c(1:5,18,8,13,33,23,28,63,53,58) # double check

df_13 <- nm_2013[c(4:nrow(nm_2013)),colunms]
df_14 <- nm_2014[c(4:nrow(nm_2014)),colunms]
df_15 <- nm_2015[c(4:nrow(nm_2015)),colunms] 
df_16 <- nm_2016[c(4:nrow(nm_2016)),colunms] 

# 2017  introduces batter storage and virtual net metering (columns : ,18,23,28,33)
colunms <- c(1:5,39,9,14,54,44,49,84,74,79) # double check

df_17 <- nm_2017[c(4:nrow(nm_2017)),colunms] 
df_18 <- nm_2018[c(4:nrow(nm_2018)),colunms] 
df_19 <- nm_2019[c(4:nrow(nm_2019)),colunms] 
df_20 <- nm_2020[c(4:nrow(nm_2020)),colunms] 
df_21 <- nm_2021[c(4:nrow(nm_2021)),colunms] 
df_22 <- nm_2022[c(4:nrow(nm_2022)),colunms] 
df_23 <- nm_2023[c(4:nrow(nm_2023)),colunms] 

# 2024 IS DIFFERENT
colunms <- c(1:5,19,9,14,84,74,79,114,104,109)
df_24 <- nm_2024[c(4:nrow(nm_2024)),colunms] 




# change their names
names <- c("year","month","state","utility_id","utility","solar_esb_mwh","solar_inmc_mw","solar_nm_customers",
           "wind_esb_mwh","wind_inmc_mw","wind_nm_customers","total_esb_mwh","total_inmc_mw","total_nm_customers")
for (i in 11:24) {
  df_name <- paste0("df_", i)
  df <- get(df_name)  # Get the data frame by name
  colnames(df) <- names  # Set column names
  assign(df_name, df)  # Save it back to the environment
}

# put them all together
nm_all <- rbind(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22,df_23,df_24)

# delete if you want
# rm(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22,df_23,df_24)

# remove adjustments or notes at the end (month would be NA)
nm_all <- nm_all[nm_all$utility_id != "99999",]
nm_all <- nm_all[!is.na(nm_all$month),]

# make numeric the appropriate columns
nm_all[c(1,2,4,6:14)] <- lapply(nm_all[c(1,2,4,6:14)], function(x) as.numeric(as.character(x)))

# export
write.csv(nm_all,
          paste0(dir$cleandata,"clean_commercial_solar_wind_nm_utility_aug_4_2025.cvs"),
          row.names = F)



#### 4. Industrial (solar and Wind)   --------------------------------------------------------------------


# from 2011 to 2012 same setup: select INDUSTRIAL net metering (energy sold back) and customers

# 2011
df_11 <- nm_2011 %>%
  select(YEAR,MONTH,STATE,`UTILITY CODE`,`UTILITY NAME`,`INDUSTRIAL  PHOTOVOLTAIC ELECTRIC ENERGY SOLD BACK (MWh)`,
         `INDUSTRIAL  PHOTOVOLTAIC INSTALLED NET METERING CAPACITY (MW)`,`INDUSTRIAL PHOTOVOLTAIC NET METERING CUSTOMER COUNT`,
         `INDUSTRIAL  WIND ELECTRIC ENERGY SOLD BACK (MWh)`,`INDUSTRIAL  WIND INSTALLED NET METERING CAPACITY (MW)`,
         `INDUSTRIAL WIND NET METERING CUSTOMER COUNT`, `INDUSTRIAL  TOTAL  ELECTRIC ENERGY SOLD BACK (MWh)`,
         `INDUSTRIAL  TOTAL  INSTALLED NET METERING CAPACITY (MW)`,`INDUSTRIAL TOTAL  NET METERING CUSTOMER COUNT`) 

# 2012
df_12 <- nm_2012 %>%
  select(YEAR,MONTH,STATE,`UTILITY CODE`,`UTILITY NAME`,`INDUSTRIAL  PHOTOVOLTAIC ELECTRIC ENERGY SOLD BACK (MWh)`,
         `INDUSTRIAL  PHOTOVOLTAIC INSTALLED NET METERING CAPACITY (MW)`,`INDUSTRIAL PHOTOVOLTAIC NET METERING CUSTOMER COUNT`,
         `INDUSTRIAL  WIND ELECTRIC ENERGY SOLD BACK (MWh)`,`INDUSTRIAL  WIND INSTALLED NET METERING CAPACITY (MW)`,
         `INDUSTRIAL WIND NET METERING CUSTOMER COUNT`, `INDUSTRIAL  TOTAL  ELECTRIC ENERGY SOLD BACK (MWh)`,
         `INDUSTRIAL  TOTAL  INSTALLED NET METERING CAPACITY (MW)`,`INDUSTRIAL TOTAL  NET METERING CUSTOMER COUNT`) 


# 2013 to 2016 same setup
colunms <- c(1:5,19,9,14,34,24,29,64,54,59) # double check

df_13 <- nm_2013[c(4:nrow(nm_2013)),colunms]
df_14 <- nm_2014[c(4:nrow(nm_2014)),colunms]
df_15 <- nm_2015[c(4:nrow(nm_2015)),colunms] 
df_16 <- nm_2016[c(4:nrow(nm_2016)),colunms] 

# 2017  introduces batter storage and virtual net metering (columns : ,18,23,28,33)
colunms <- c(1:5,40,10,15,55,45,50,85,75,80) # double check

df_17 <- nm_2017[c(4:nrow(nm_2017)),colunms] 
df_18 <- nm_2018[c(4:nrow(nm_2018)),colunms] 
df_19 <- nm_2019[c(4:nrow(nm_2019)),colunms] 
df_20 <- nm_2020[c(4:nrow(nm_2020)),colunms] 
df_21 <- nm_2021[c(4:nrow(nm_2021)),colunms] 
df_22 <- nm_2022[c(4:nrow(nm_2022)),colunms] 
df_23 <- nm_2023[c(4:nrow(nm_2023)),colunms] 

# 2024 IS DIFFERENT
colunms <- c(1:5,20,10,15,85,75,80,115,105,110)
df_24 <- nm_2024[c(4:nrow(nm_2024)),colunms] 



# change their names
names <- c("year","month","state","utility_id","utility","solar_esb_mwh","solar_inmc_mw","solar_nm_customers",
           "wind_esb_mwh","wind_inmc_mw","wind_nm_customers","total_esb_mwh","total_inmc_mw","total_nm_customers")
for (i in 11:24) {
  df_name <- paste0("df_", i)
  df <- get(df_name)  # Get the data frame by name
  colnames(df) <- names  # Set column names
  assign(df_name, df)  # Save it back to the environment
}

# put them all together
nm_all <- rbind(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22,df_23,df_24)

# delete if you want
# rm(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22,df_23,df_24)

# remove adjustments or notes at the end (month would be NA)
nm_all <- nm_all[nm_all$utility_id != "99999",]
nm_all <- nm_all[!is.na(nm_all$month),]

# make numeric the appropriate columns
nm_all[c(1,2,4,6:14)] <- lapply(nm_all[c(1,2,4,6:14)], function(x) as.numeric(as.character(x)))


# export
write.csv(nm_all,
          paste0(dir$cleandata,"clean_industrial_solar_wind_nm_utility_aug_4_2025.cvs"),
          row.names = F)



#### 5.  --------------------------------------------------------------------