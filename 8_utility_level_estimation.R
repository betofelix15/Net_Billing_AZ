# Utility level estimation
# 11 Aug, 2025
# Author: Jesus Felix

#### 0.  Pre-requesites ----------------------------------------------------------------
# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "tidygeocoder",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi",
          "zoo","gmapsdistance","rmapshaper", "gridExtra","tigris","Synth")

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

## IMPORTANT ACRONYMS!
# ESB -- ENERGY SOLD BACK
# inmc = installed net metering capacity
#
##
###### data ----------------------------------------------

# load the data with covariates
nm_full <- read.csv(paste0(dir$cleandata,"utility_nm_full_covariates_18aug2025.csv"))

# create the esb/install in in hrs: mwh/mw = h. remember : 1 MWh is equivalent to the energy produced or consumed by a power source of 1 megawatt (MW) running for one hour
nm_full$solar_esb_inmc_hr <- nm_full$solar_esb_mwh/nm_full$solar_inmc_mw

# make NA's into zero
nm_full$solar_esb_inmc_hr <- ifelse(is.na(nm_full$solar_esb_inmc_hr),0,nm_full$solar_esb_inmc_hr )

# create the esb/customer in mwh (PC = per customer)
nm_full$solar_esb_kwh_pc <- (nm_full$solar_esb_mwh*1000)/nm_full$solar_nm_customers

# make NA's into zero
nm_full$solar_esb_kwh_pc <- ifelse(is.na(nm_full$solar_esb_kwh_pc),0,nm_full$solar_esb_kwh_pc )

# create a date
nm_full$date <- as.Date(paste0(nm_full$year,"-",nm_full$month,"-","28"))

# create a installed capacity per customer: mw_pc. This is to measure change in capacity rates
nm_full$solar_inmc_pc_kw <- 1000*nm_full$solar_inmc_mw/nm_full$solar_nm_customers

# make NA's into zero
nm_full$solar_inmc_pc_kw <- ifelse(is.na(nm_full$solar_inmc_pc_kw),0,nm_full$solar_inmc_pc_kw )


#### ----TEP synt control -----------------------------------------------------






