# Title: Clean Net Metering Data
# Author: Jesus Felix
# Start date: 31 OCT 2024 (HALLOWEEEEEEEEEN!!!!!)
################################################################################
# This version does municipio level data first then summarizes for the states

#### R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


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

##### import datasets ---------------------------------------------------------

for(i in 2011:2013){ # i = 2013
  filename <- paste0("nm_",i)
  wd <- paste0(dir$rawdata,"net_metering_",i,".xls")
  assign(filename, read_excel(wd, sheet = 2))
} 

nm_2014 <- read_excel(paste0(dir$rawdata,"net_metering_2014.xls"), sheet = 3)
nm_2015 <- read_excel(paste0(dir$rawdata,"net_metering_2015.xls"), sheet = 4)


# from 2016 to 2022, they used xlxs files
for(i in 2016:2022){ # i = 2016
  filename <- paste0("nm_",i)
  wd <- paste0(dir$rawdata,"net_metering_",i,".xlsx")
  assign(filename, read_xlsx(wd, sheet = 4))
} 

#### clean  for residential state level  ---------------------------------------------------------------

# Photovoltaic only
# from 2011 to 2012 same setup: select residential net metering (energy sold back) and customers
df_11 <- nm_2011[c(1:4,14)]
colnames(df_11) <- c("year","month","state","energy_sold_back_mwh","res_customers")
df_12 <- nm_2012[c(1:4,14)] # notice the differences in length
colnames(df_12) <- c("year","month","state","energy_sold_back_mwh","res_customers")

# 2013 to 2016 same setup
df_13 <- nm_2013[c(4:length(nm_2013$...2)),c(1:3,10,15)]
colnames(df_13) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_14 <- nm_2014[c(4:length(nm_2014$...2)),c(1:3,10,15)]
colnames(df_14) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_15 <- nm_2015[c(4:length(nm_2015$...2)),c(1:3,10,15)]
colnames(df_15) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_16 <- nm_2016[c(4:length(nm_2016$...2)),c(1:3,10,15)]
colnames(df_16) <- c("year","month","state","res_customers","energy_sold_back_mwh")

# 2017 to 2022 introduces storage
df_17 <- nm_2017[c(4:length(nm_2017$...2)),c(1:3,10,35)]
colnames(df_17) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_18 <- nm_2018[c(4:length(nm_2018$...2)),c(1:3,10,35)]
colnames(df_18) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_19 <- nm_2019[c(4:length(nm_2019$...2)),c(1:3,10,35)]
colnames(df_19) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_20 <- nm_2020[c(4:length(nm_2020$...2)),c(1:3,10,35)]
colnames(df_20) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_21 <- nm_2021[c(4:length(nm_2021$...2)),c(1:3,10,35)]
colnames(df_21) <- c("year","month","state","res_customers","energy_sold_back_mwh")
df_22 <- nm_2022[c(4:length(nm_2022$...2)),c(1:3,10,35)]
colnames(df_22) <- c("year","month","state","res_customers","energy_sold_back_mwh")


# put together
nm_all <- rbind(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22)

# remove months that are NA (these have a year that was a description in excel)
# remove american samoa and dc

nm_all <- nm_all %>%
  filter(!is.na(month) & state %not_in% c("AS","DC")) %>%
  mutate(res_customers = case_when(
    res_customers == "." ~ "0",
    is.na(res_customers) ~ "0",
    TRUE ~ res_customers
  ),
  energy_sold_back_mwh = case_when(
    energy_sold_back_mwh == "." ~ "0",
    is.na(energy_sold_back_mwh) ~ "0",
    TRUE ~ energy_sold_back_mwh
  ))
length(unique(nm_all$state)) #48 states

count <- nm_all %>%
  group_by(state) %>%
  summarise(n_years = length(unique(year)),
            n_obs = length(unique(paste(year,month))))
bad <- count[count$n_obs != 144,]

df <- nm_all %>% filter(state %in% bad$state) # starts april 2012

# panel data is unbalanced, alabama is missing 2011, 
year <- c(rep(2011, 12),rep(2012,3), # AL
          2011,                      # KS
          rep(2011, 12),rep(2012,3), # MS
          2011)                      #TN 

month <- c(c(1:12),c(1:3),          #AL
           3,                       # KS
           c(1:12),c(1:3),          #MS
           1)                       # TN

state <- c(rep("AL",15),"KS",rep("MS",15),"TN")

res_customers <- rep(0,32)
                   
energy_sold_back_mwh <- rep(0,32)

a <- as.data.frame(cbind(year,month,state,res_customers,energy_sold_back_mwh))

df <- rbind(nm_all,a)

count <- df %>%
  group_by(state) %>%
  summarise(n_years = length(unique(year)),
            n_obs = length(unique(paste(year,month))))

nm_all <- df

# should be 12 years for all 48 states: 12yrs*12mo=144, 144*48 = 6912, checks out!

# make the res and energy sold back columns numeric
nm_all$energy_sold_back_mwh <- as.numeric(nm_all$energy_sold_back_mwh)
nm_all$res_customers <- as.numeric(nm_all$res_customers)

nm_all$year <- as.numeric(nm_all$year)
nm_all$month <- as.numeric(nm_all$month)

states <- unique(nm_all$state)
states

# give all states an id
nm_all <- nm_all %>%
  group_by(state) %>%
  mutate(state_id = cur_group_id())


# identify Arizona and give it a 1 as the state ID, the rest by alphatecical order
nm_az <- nm_all[nm_all$state == "AZ",]
nm_az$state_id <- 1

# give whoever was 1 whatever AZ used to be, seperate AZ, and add the new AZ
nm_all[nm_all$state_id == 1,]
nm_other <- nm_all %>%
  filter(state != "AZ") %>%
  mutate(state_id = case_when(
    state == "AL" ~ 3,
    TRUE ~ state_id
  ))

nm_all <- rbind(nm_az,nm_other)

# make mwh to kwh (1 mwh = 1000 kwh)
nm_all$energy_sold_back_kwh <- nm_all$energy_sold_back_mwh*1000

# get rid of mwh variable
nm_all <- nm_all[-c(4)]

# make the net metering by customer
nm_all$nm_per_cust <- nm_all$energy_sold_back_kwh/nm_all$res_customers

# if the value is NA -> make zero
nm_all[is.na(nm_all$nm_per_cust),7] <- 0


# format for date
nm_all$date <- as.Date(paste(nm_all$year,nm_all$month,"01",sep = "-"))



# rearrange the datat
nm_all <- arrange(nm_all,year,month,state_id)


# export this df
write.csv(nm_all, paste0(dir$cleandata,"net_metering_all_states_residential.csv"),
          row.names = F)


##### clean for all customers (residential, commercial, industrial) ------------------------------

# Photovoltaic only
# from 2011 to 2012 same setup: select residential net metering (energy sold back) and customers
df_11 <- nm_2011[c(1:3,8,18)]
colnames(df_11) <- c("year","month","state","energy_sold_back_mwh","customers")
df_12 <- nm_2012[c(1:3,8,18)] # notice the differences in length
colnames(df_12) <- c("year","month","state","energy_sold_back_mwh","customers")

# 2013 to 2016 same setup
df_13 <- nm_2013[c(4:length(nm_2013$...2)),c(1:3,14,19)]
colnames(df_13) <- c("year","month","state","customers","energy_sold_back_mwh")
df_14 <- nm_2014[c(4:length(nm_2014$...2)),c(1:3,14,19)]
colnames(df_14) <- c("year","month","state","customers","energy_sold_back_mwh")
df_15 <- nm_2015[c(4:length(nm_2015$...2)),c(1:3,14,19)]
colnames(df_15) <- c("year","month","state","customers","energy_sold_back_mwh")
df_16 <- nm_2016[c(4:length(nm_2016$...2)),c(1:3,14,19)]
colnames(df_16) <- c("year","month","state","customers","energy_sold_back_mwh")

# 2017 to 2022 introduces storage
df_17 <- nm_2017[c(4:length(nm_2017$...2)),c(1:3,14,39)]
colnames(df_17) <- c("year","month","state","customers","energy_sold_back_mwh")
df_18 <- nm_2018[c(4:length(nm_2018$...2)),c(1:3,14,39)]
colnames(df_18) <- c("year","month","state","customers","energy_sold_back_mwh")
df_19 <- nm_2019[c(4:length(nm_2019$...2)),c(1:3,14,39)]
colnames(df_19) <- c("year","month","state","customers","energy_sold_back_mwh")
df_20 <- nm_2020[c(4:length(nm_2020$...2)),c(1:3,14,39)]
colnames(df_20) <- c("year","month","state","customers","energy_sold_back_mwh")
df_21 <- nm_2021[c(4:length(nm_2021$...2)),c(1:3,14,39)]
colnames(df_21) <- c("year","month","state","customers","energy_sold_back_mwh")
df_22 <- nm_2022[c(4:length(nm_2022$...2)),c(1:3,14,39)]
colnames(df_22) <- c("year","month","state","customers","energy_sold_back_mwh")


# put together
nm_all <- rbind(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22)

# remove months that are NA (these have a year that was a description in excel)
# remove american samoa, hawaii, alaska, and dc
`%not_in%` <- negate(`%in%`)

nm_all <- nm_all %>%
  filter(!is.na(month) & state %not_in% c("AS","DC")) %>%
  mutate(customers = case_when(
    customers == "." ~ "0",
    is.na(customers) ~ "0",
    TRUE ~ customers
  ),
  energy_sold_back_mwh = case_when(
    energy_sold_back_mwh == "." ~ "0",
    is.na(energy_sold_back_mwh) ~ "0",
    TRUE ~ energy_sold_back_mwh
  ))

# panel data is unbalanced, 
count <- nm_all %>%
  group_by(state) %>%
  summarise(n_years = length(unique(year)),
            n_obs = length(unique(paste(year,month))))
bad <- count[count$n_obs != 144,]

df <- nm_all %>% filter(state %in% bad$state) # starts april 2012

# panel data is unbalanced, alabama is missing 2011, 
year <- c(rep(2011, 12),rep(2012,3), # AL
          2011,                      # KS
          rep(2011, 12),rep(2012,3), # MS
          2011)                      #TN 

month <- c(c(1:12),c(1:3),          #AL
           3,                       # KS
           c(1:12),c(1:3),          #MS
           1)                       # TN

state <- c(rep("AL",15),"KS",rep("MS",15),"TN")

customers <- rep(0,32)

energy_sold_back_mwh <- rep(0,32)

a <- as.data.frame(cbind(year,month,state,customers,energy_sold_back_mwh))

df <- rbind(nm_all,a)

count <- df %>%
  group_by(state) %>%
  summarise(n_years = length(unique(year)),
            n_obs = length(unique(paste(year,month))))

nm_all <- df

# make the res and energy sold back columns numeric
nm_all$energy_sold_back_mwh <- as.numeric(nm_all$energy_sold_back_mwh)
nm_all$customers <- as.numeric(nm_all$customers)

nm_all$year <- as.numeric(nm_all$year)
nm_all$month <- as.numeric(nm_all$month)

states <- unique(nm_all$state)
states



# give all states an id
nm_all <- nm_all %>%
  group_by(state) %>%
  mutate(state_id = cur_group_id())


# identify Arizona and give it a 1 as the state ID, the rest by alphatecical order
nm_az <- nm_all[nm_all$state == "AZ",]
nm_az$state_id <- 1

# give whoever was 1 whatever AZ used to be, seperate AZ, and add the new AZ
nm_all[nm_all$state_id == 1,]
nm_other <- nm_all %>%
  filter(state != "AZ") %>%
  mutate(state_id = case_when(
    state == "AL" ~ 3,
    TRUE ~ state_id
  ))

nm_all <- rbind(nm_az,nm_other)

# make mwh to kwh (1 mwh = 1000 kwh)
nm_all$energy_sold_back_kwh <- nm_all$energy_sold_back_mwh*1000

# get rid of mwh variable
nm_all <- nm_all[-c(4)]

# make the net metering by customer
nm_all$nm_per_cust <- nm_all$energy_sold_back_kwh/nm_all$customers

# if the value is NA -> make zero
nm_all[is.na(nm_all$nm_per_cust),7] <- 0

# rearrange the datat
nm_all <- arrange(nm_all,year,month,state_id)

# I guess this is the best format for date
nm_all$date <- as.Date(paste(nm_all$year,nm_all$month,"01",sep = "-"))

options(scipen = 999)
# export this df
write.csv(nm_all, paste0(dir$cleandata,"net_metering_all_states_all_cust.csv"),
          row.names = F)

#----
#----
#----
##### Individual Utility level --------------------------------------------------

for(i in 2011:2013){ # i = 2013
  filename <- paste0("nm_",i)
  wd <- paste0(dir$rawdata,"net_metering_",i,".xls")
  assign(filename, read_excel(wd, sheet = 1))
} 

nm_2014 <- read_excel(paste0(dir$rawdata,"net_metering_2014.xls"), sheet = 1)
nm_2015 <- read_excel(paste0(dir$rawdata,"net_metering_2015.xls"), sheet = 1)


# from 2016 to 2022, they used xlxs files
for(i in 2016:2022){ # i = 2016
  filename <- paste0("nm_",i)
  wd <- paste0(dir$rawdata,"net_metering_",i,".xlsx")
  assign(filename, read_xlsx(wd, sheet = 1))
} 

#### Residential at Utility level (keep working) ---------------------------------------------------------------

# Photovoltaic only
# from 2011 to 2012 same setup: select residential net metering (energy sold back) and customers
df_11 <- nm_2011[c(1:6,16)]
colnames(df_11) <- c("year","month","state","utility_id","utility","energy_sold_back_mwh","res_customers")
df_12 <- nm_2012[c(1:6,16)] # notice the differences in length
colnames(df_12) <- c("year","month","state","utility_id","utility","energy_sold_back_mwh","res_customers")



# 2013 to 2016 same setup
df_13 <- nm_2013[c(4:length(nm_2013$...2)),c(1:5,12,17)]
colnames(df_13) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_14 <- nm_2014[c(4:length(nm_2014$...2)),c(1:5,12,17)]
colnames(df_14) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_15 <- nm_2015[c(4:length(nm_2015$...2)),c(1:5,12,17)]
colnames(df_15) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_16 <- nm_2016[c(4:length(nm_2016$...2)),c(1:5,12,17)]
colnames(df_16) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")

# 2017 to 2022 introduces storage
df_17 <- nm_2017[c(4:length(nm_2017$...2)),c(1:5,13,38)]
colnames(df_17) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_18 <- nm_2018[c(4:length(nm_2018$...2)),c(1:5,13,38)]
colnames(df_18) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_19 <- nm_2019[c(4:length(nm_2019$...2)),c(1:5,13,38)]
colnames(df_19) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_20 <- nm_2020[c(4:length(nm_2020$...2)),c(1:5,13,38)]
colnames(df_20) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_21 <- nm_2021[c(4:length(nm_2021$...2)),c(1:5,13,38)]
colnames(df_21) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")
df_22 <- nm_2022[c(4:length(nm_2022$...2)),c(1:5,13,38)]
colnames(df_22) <- c("year","month","state","utility_id","utility","res_customers","energy_sold_back_mwh")


# put together
nm_all <- rbind(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22)

# remove months that are NA (these have a year that was a description in excel)
# remove american samoa and dc, alaska, hawaii, remove adjustment utility id
`%not_in%` <- negate(`%in%`)

nm_all <- nm_all %>%
  filter(!is.na(month) & state %not_in% c("AS","AK","HI") &
           utility_id != "99999") %>%
  mutate(res_customers = case_when(
    res_customers == "." ~ "0",
    is.na(res_customers) ~ "0",
    TRUE ~ res_customers
  ),
  energy_sold_back_mwh = case_when(
    energy_sold_back_mwh == "." ~ "0",
    is.na(energy_sold_back_mwh) ~ "0",
    TRUE ~ energy_sold_back_mwh
  ))



# make the res and energy sold back columns numeric
nm_all$energy_sold_back_mwh <- as.numeric(nm_all$energy_sold_back_mwh)
nm_all$res_customers <- as.numeric(nm_all$res_customers)

nm_all$year <- as.numeric(nm_all$year)
nm_all$month <- as.numeric(nm_all$month)



# give all states an id
nm_all <- nm_all %>%
  group_by(state) %>%
  mutate(state_id = cur_group_id())


# identify Arizona and give it a 1 as the state ID, the rest by alphatecical order
nm_az <- nm_all[nm_all$state == "AZ",]
nm_az$state_id <- 1

# give whoever was 1 whatever AZ used to be, seperate AZ, and add the new AZ
nm_all[nm_all$state_id == 1,]
nm_other <- nm_all %>%
  filter(state != "AZ") %>%
  mutate(state_id = case_when(
    state == "AL" ~ 3,
    TRUE ~ state_id
  ))

nm_all <- rbind(nm_az,nm_other)

nm_all[nm_all$state_id == 1,]

# make mwh to kwh (1 mwh = 1000 kwh)
nm_all$energy_sold_back_kwh <- nm_all$energy_sold_back_mwh*1000

# get rid of mwh variable
nm_all <- nm_all[-c(6)]

# make the net metering by customer
nm_all$nm_per_cust <- nm_all$energy_sold_back_kwh/nm_all$res_customers

# if the value is NA -> make zero
nm_all[is.na(nm_all$nm_per_cust),7] <- 0

nm_all$nm_per_cust <- ifelse(nm_all$res_customers == 0, 0,nm_all$nm_per_cust)


# rearrange the datat
nm_all <- arrange(nm_all,year,month,state_id)

# format for date
nm_all$date <- as.Date(paste(nm_all$year,nm_all$month,"01",sep = "-"))

# remove all NA observations
nm_all <- nm_all[!is.na(nm_all$utility_id),]

# remove the state id == 0
nm_all <- nm_all[nm_all$state_id != 0,]

# change the utility id to reflect all the utilities and states (some added later)
states <- unique(nm_all$state)
states
# do this for the estimation at the end
# nm_all_utid <- NA
# for(st in as.list(states)){ # st <- "AR"
# 
#   nm_st <- nm_all[nm_all$state == st,]
# 
#   n_ut <- unique(nm_st[c(4,7)])
# 
#   n_ut$ut_id <- as.numeric(paste0(n_ut$state_id,row.names(n_ut)))
#   
#   # combine
#   nm_st <- as.data.frame(left_join(nm_st,n_ut, by = c("state_id","utility_id")))
#   
#   # save
#   nm_all_utid <- rbind(nm_all_utid,nm_st)
# }
# # remove first obs
# nm_all_utid <- nm_all_utid[-1,]




# export this df
write.csv(nm_all, paste0(dir$cleandata,"net_metering_utility_residential.csv"),
          row.names = F)

# from here move on to "2_covariate_data.R" into step U1

#### All customers at Utility level ---------------------------------------------------------------

# Photovoltaic only
# from 2011 to 2012 same setup: select residential net metering (energy sold back) and customers
df_11 <- nm_2011[c(1:5,10,20)]
colnames(df_11) <- c("year","month","state","utility_id","utility","energy_sold_back_mwh","customers")
df_12 <- nm_2012[c(1:5,10,20)] # notice the differences in length
colnames(df_12) <- c("year","month","state","utility_id","utility","energy_sold_back_mwh","customers")


# 2013 to 2016 same setup
df_13 <- nm_2013[c(4:length(nm_2013$...2)),c(1:5,16,21)]
colnames(df_13) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_14 <- nm_2014[c(4:length(nm_2014$...2)),c(1:5,16,21)]
colnames(df_14) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_15 <- nm_2015[c(4:length(nm_2015$...2)),c(1:5,16,21)]
colnames(df_15) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_16 <- nm_2016[c(4:length(nm_2016$...2)),c(1:5,16,21)]
colnames(df_16) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")

# 2017 to 2022 introduces storage
df_17 <- nm_2017[c(4:length(nm_2017$...2)),c(1:5,17,42)]
colnames(df_17) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_18 <- nm_2018[c(4:length(nm_2018$...2)),c(1:5,17,42)]
colnames(df_18) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_19 <- nm_2019[c(4:length(nm_2019$...2)),c(1:5,17,42)]
colnames(df_19) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_20 <- nm_2020[c(4:length(nm_2020$...2)),c(1:5,17,42)]
colnames(df_20) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_21 <- nm_2021[c(4:length(nm_2021$...2)),c(1:5,17,42)]
colnames(df_21) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")
df_22 <- nm_2022[c(4:length(nm_2022$...2)),c(1:5,17,42)]
colnames(df_22) <- c("year","month","state","utility_id","utility","customers","energy_sold_back_mwh")


# put together
nm_all <- rbind(df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22)

# remove months that are NA (these have a year that was a description in excel)
# remove american samoa, hawaii, alaska, and dc, remove adjustment utility id
`%not_in%` <- negate(`%in%`)

nm_all <- nm_all %>% # consider keeping DC (well, we went pretty far without it)
  filter(!is.na(month) & state %not_in% c("AS","DC") &
           utility_id != "99999") %>%
  mutate(customers = case_when(
    customers == "." ~ "0",
    is.na(customers) ~ "0",
    TRUE ~ customers
  ),
  energy_sold_back_mwh = case_when(
    energy_sold_back_mwh == "." ~ "0",
    is.na(energy_sold_back_mwh) ~ "0",
    TRUE ~ energy_sold_back_mwh
  ))



# make the res and energy sold back columns numeric
nm_all$energy_sold_back_mwh <- as.numeric(nm_all$energy_sold_back_mwh)
nm_all$customers <- as.numeric(nm_all$customers)

nm_all$year <- as.numeric(nm_all$year)
nm_all$month <- as.numeric(nm_all$month)

states <- unique(nm_all$state)
states

# give all states an id
nm_all <- nm_all %>%
  group_by(state) %>%
  mutate(state_id = cur_group_id())


# identify Arizona and give it a 1 as the state ID, the rest by alphatecical order
nm_az <- nm_all[nm_all$state == "AZ",]
nm_az$state_id <- 1

# give whoever was 1 whatever AZ used to be, seperate AZ, and add the new AZ
nm_all[nm_all$state_id == 1,]
nm_other <- nm_all %>%
  filter(state != "AZ") %>%
  mutate(state_id = case_when(
    state == "AL" ~ 3,
    TRUE ~ state_id
  ))

nm_all <- rbind(nm_az,nm_other)


# make mwh to kwh (1 mwh = 1000 kwh)
nm_all$energy_sold_back_kwh <- nm_all$energy_sold_back_mwh*1000

# get rid of mwh variable
nm_all <- nm_all[-c(6)]

# make the net metering by customer
nm_all$nm_per_cust <- nm_all$energy_sold_back_kwh/nm_all$customers

# if the value is NA -> make zero
nm_all[is.na(nm_all$nm_per_cust),7] <- 0

nm_all$nm_per_cust <- ifelse(nm_all$customers == 0, 0,nm_all$nm_per_cust)


# rearrange the datat
nm_all <- arrange(nm_all,year,month,state_id)

# format for date
nm_all$date <- as.Date(paste(nm_all$year,nm_all$month,"01",sep = "-"))

# remove all NA observations
nm_all <- nm_all[!is.na(nm_all$utility_id),]


# change the utility id to reflect all the utilities (some added later)
# work later
nm_az <- nm_all[nm_all$state == "AZ",]

ut_id <- unique(nm_az$utility_id)


# export this df
write.csv(nm_all, paste0(dir$cleandata,"net_metering_utility_all_customers.csv"),
          row.names = F)


# ## additional cleaning required (1 Dec 2024, with covariates) ----------------------------------
nm_all_res <- read.csv(paste0(dir$cleandata,"res_nm_wcov_coords.csv"))

# in 2020 there are large peaks in march and july for some reason, lets check it out
nm_2020_mar <- nm_all_res[nm_all_res$year == 2020 & 
                          nm_all_res$month == 3,]

nm_2020_jul <- nm_all_res[nm_all_res$year == 2020 & 
                            nm_all_res$month == 7,]
# find the sums
sum(nm_2020_mar$nm_per_cust)
sum(nm_2020_jul$nm_per_cust)

# find the max
nm_2020_mar[which(nm_2020_mar$nm_per_cust == max(nm_2020_mar$nm_per_cust)),]$state
nm_2020_jul[which(nm_2020_jul$nm_per_cust == max(nm_2020_jul$nm_per_cust)),]$state


# how does it compare to 2021?
nm_2021_mar <- nm_all_res[nm_all_res$year == 2021 & 
                            nm_all_res$month == 3,]

nm_2021_jul <- nm_all_res[nm_all_res$year == 2021 & 
                            nm_all_res$month == 7,]
# find the sums
sum(nm_2021_mar$nm_per_cust)
sum(nm_2021_jul$nm_per_cust)

# find the max
nm_2021_jul[which(nm_2021_jul$nm_per_cust == max(nm_2021_jul$nm_per_cust)),]$state
nm_2021_mar[which(nm_2021_mar$nm_per_cust == max(nm_2021_mar$nm_per_cust)),]$state

# South Carolina is the problem in 2020 march and Indiana is the problem in July 2020
# we can set their prices to equal the average from 2019 and 2020
# also Georgia (GA) in April 2012 is out of order
nm_2019_mar <- nm_all_res[nm_all_res$year == 2019 & 
                            nm_all_res$month == 3,]

nm_2019_jul <- nm_all_res[nm_all_res$year == 2019 & 
                            nm_all_res$month == 7,]
avg_in <- (nm_2019_jul[nm_2019_jul$state == 'IN',]$nm_per_cust + 
           nm_2021_jul[nm_2021_jul$state == 'IN',]$nm_per_cust)/2
avg_sc <- (nm_2019_mar[nm_2019_mar$state == 'SC',]$nm_per_cust + 
           nm_2021_mar[nm_2021_mar$state == 'SC',]$nm_per_cust)/2
avg_ga <- (nm_all_res[nm_all_res$year == 2011 & nm_all_res$month == 4 & nm_all_res$state == "GA",]$nm_per_cust +
             nm_all_res[nm_all_res$year == 2013 & nm_all_res$month == 4 & nm_all_res$state == "GA",]$nm_per_cust)/2

nm_all_res[nm_all_res$state == "IN" & 
           nm_all_res$year == 2020 &
           nm_all_res$month == 7,]$nm_per_cust <- avg_in

nm_all_res[nm_all_res$state == "SC" & 
             nm_all_res$year == 2020 &
             nm_all_res$month == 3,]$nm_per_cust <- avg_sc

nm_all_res[nm_all_res$year == 2012 & nm_all_res$month == 4 & nm_all_res$state == "GA",]$nm_per_cust <- avg_ga

sum_all_other <- nm_all_res %>%
  group_by(date) %>%
  summarize(avg_nm_kwh = mean(nm_per_cust))

plot(sum_all_other$avg_nm_kwh,
     type = "l")


# because we need an actual amount in the net metering per cust, we have to add a value
# really close to zero, 0.00001. We can discuss the zero rates later (one idea is to use change in customers instead net metering)

nm_all_res <- read.csv(paste0(dir$cleandata,"res_net_metering_wcov_2024_12_03.csv"))
nm_all_res$nm_per_cust <- ifelse(nm_all_res$nm_per_cust == 0,0.000001,nm_all_res$nm_per_cust)
nm_all_res_zero <- nm_all_res


# export this df (edit the date each time) (last time edited)
write.csv(nm_all_res, paste0(dir$cleandata,"res_net_metering_wcov_2024_12_03.csv"),
          row.names = F)

# export this df (edit the date each time) (last time edited)
write.csv(nm_all_res_zero, paste0(dir$cleandata,"res_net_metering_wcov_2024_12_05_zeros_edited.csv"),
          row.names = F)

####
nm_all_res <- read.csv(paste0(dir$cleandata,"res_net_metering_wcov_2024_12_05_zeros_edited.csv"))
nm_u <- read.csv(paste0(dir$cleandata,"net_metering_utility_residential.csv"))
