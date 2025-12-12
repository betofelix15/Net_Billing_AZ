# state level estimation
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


##### AZ Synt. Control ---------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~AZ: data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# identify utilities in AZ that are not investor owned
u_drop <- unique(nm_full[nm_full$state == "AZ" & nm_full$IOU == 0,]$full_id)

# identify states that cannot be included for similar treatment or different treatment
# drop AK, HI, CA, GA, MI, MO, NE, ND, TN, SD, AL, TX, ID, WI, NV, KS
s_drop <- c("AK", "HI","AL", "CA", "GA", "MI", "MO", "NE", "ND", "TN", "SD", "AL", "TX", "ID", "WI", "NV", "KS")


# summarize by state, filter dates, states, utilities
nm_sc <- nm_full %>%
  filter(year > 2012 & year < 2020  & state %not_in% s_drop & full_id %not_in% u_drop) %>%
  group_by(year,month,date,state) %>%
  summarize(
    # outcome variables
    solar_esb_inmc_hr = sum(solar_esb_inmc_hr, na.rm = T), 
    solar_esb_kwh_pc = sum(solar_esb_kwh_pc, na.rm = T),
    solar_inmc_pc_kw = sum(solar_inmc_pc_kw, na.rm = T),
    # covariates
    avg_temp = mean(avg_temp, na.rm = T),
    avg_tcc = mean(avg_tcc, na.rm = T),
    cloudy_days = mean(cloudy_days, na.rm = T),
    med_inc = mean(med_inc, na.rm = T),
    avg_price_cents_per_kwh = mean(avg_price_cents_per_kwh, na.rm = T),
    avg_rev_usd_pc = mean(avg_rev_usd_pc, na.rm = T),
    avg_consumption_kwh_pc = mean(avg_consumption_kwh_pc, na.rm = T)
  ) 

# add a numeric date
date_df <- unique(nm_sc[c("year","month")])
date_df$date_numeric <- 1:nrow(date_df)
nm_sc <- left_join(nm_sc,date_df, by = c("year","month"))

# give the states an ID other than 1 and give AZ 1
state <- unique(nm_sc[nm_sc$state != "AZ",]$state)
state_id <- c(2:(length(state)+1))
states_df <- as.data.frame(cbind(state,state_id))
nm_sc <- left_join(nm_sc, states_df, by = "state")

nm_sc$state_id <- ifelse(nm_sc$state == 'AZ',1, nm_sc$state_id)

# make this column numeric
nm_sc$state_id <- as.numeric(nm_sc$state_id)

# arrange
nm_sc <- arrange(nm_sc, state_id, date_numeric)

# is the data balanced?
35*84
nrow(nm_sc)

# turn nm_sc into a dataframe
nm_sc <- as.data.frame(nm_sc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#~~~~~~~~~~~ AZ: synthetic control package/estimation/ solar_esb_inmc_hr ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this estimation is for esb per installed capacity

# select t0 as date before treatment as a numeric index (treatment was Dec 2016)
t0 <- date_df[date_df$year == 2016 & date_df$month == 11,]$date_numeric

# select tn as final date in numeric index
tn <- date_df[date_df$year == 2019 & date_df$month == 12,]$date_numeric

# select total number of ids for controls
id_n <- max(nm_sc$state_id)


dataprep.out <- dataprep(
  foo = nm_sc,
  predictors=c("avg_temp",
               "avg_tcc",
               "med_inc",
               "avg_price_cents_per_kwh",
               "avg_rev_usd_pc",
               "avg_consumption_kwh_pc"),
  predictors.op="mean",
  time.variable="date_numeric", # must be a numeric value
  time.predictors.prior=1:t0, # pre-treatment period
  dependent="solar_esb_inmc_hr",
  unit.variable = "state_id",
  unit.names.variable= "state",
  treatment.identifier=1,
  controls.identifier=2:id_n,
  time.optimize.ssr=1:t0, # pre-treatment period
  time.plot=1:tn) # full time period

dataprep.out$X1 # treatment predictors
dataprep.out$X0 # control predictors

dataprep.out$Z0 # treatment outcome variable
dataprep.out$Z1 # control outcome variable

# Optimization (minimizing MSPE over W and V)
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS",
                   genoud = F)
str(synth.out)
# W MATRIX
synth.out$solution.w

# ANNUAL GAP BETWEEN OUTCOME VARIABLE OF TREATMENT AND SYNTHETIC CONTROL- TREATMENT EFFECT
gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot%*%synth.out$solution.w)

# summary of gap in the pre
summary(gap[1:t0,1])
# summary of gap in the post
summary(gap[(t0+1):tn,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred

# export covariate values for synth, treated, sample
df_export <- as.data.frame(synth.tables$tab.pred)
df_export$covariates <- row.names(df_export)
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_AZ_esb_inmc_cov_values.csv"),
          row.names = F)

# TABLE 2: V MATRIX
synth.tables$tab.v
# export covariate weights
df_export <- as.data.frame(synth.tables$tab.v)
df_export$covariates <- row.names(df_export)
df_export <- df_export %>%
  mutate(across(where(is.list), ~ sapply(., toString)))
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_AZ_esb_inmc_cov_weights.csv"),
          row.names = F)

# TABLE 3: W MATRIX
synth.tables$tab.w

# export state weights
df_export <- as.data.frame(synth.tables$tab.w)

write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_AZ_esb_inmc_cov_values.csv"),
          row.names = F)

# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "AZ: State Level Estimation",
          Ylab= "MwH ESB Per installed Capacity",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")



# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(nm_sc$date)),
                    ncol=length(unique(nm_sc$state_id)))
storage_gap[,1]<-gap
print(storage_gap)

# RMSPE FOR PRE INTERVENTION
rmspe_pre <- sqrt(sum(gap[1:t0]^2)/t0) #MSPE: manual calc (Jan 2013- Nov 2016) (if index did not start from 1, then it would be t0-(initial)+1)
rmspe_pre

#MSPE: reported by synth(), used for verification
synth.out$loss.v
sqrt(synth.out$loss.v) # using the square root of this should give you pre-RMSPE

# RMSPE FOR THE POST INTERVENTION
rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0+1)) # manual calc
rmspe_post

# POST RMSPE/PRE RMSPE
rmspe_storage <- c(rep(NA,length(unique(nm_sc$state_id)))) # make an empty set
rmspe_storage[1] <- rmspe_post/rmspe_pre # add the ratio for index 1
rmspe_data <- data.frame(state_i=c(unique(nm_sc$state_id)), # make into a dataframe
                       ratio_rmspe=rmspe_storage)

# falsification test
for(i in 2:length(unique(nm_sc$state_id))){ # i <- 6 
  
  # DATA PREPERATION
  
  dataprep.out <- dataprep(
    foo = nm_sc,
    predictors=c("avg_temp",
                 "avg_tcc",
                 "med_inc",
                 "avg_price_cents_per_kwh",
                 "avg_rev_usd_pc",
                 "avg_consumption_kwh_pc"),
    predictors.op="mean",
    time.variable="date_numeric", # must be a numeric value
    time.predictors.prior=1:t0, # pre-treatment period
    dependent="solar_esb_inmc_hr",
    unit.variable = "state_id",
    unit.names.variable= "state",
    treatment.identifier=i,
    controls.identifier=unique(
      nm_sc[nm_sc$state_id != i,]$state_id),
    time.optimize.ssr=1:t0, # pre-treatment period
    time.plot=1:tn) # full time period
  
  # OPTIMIZATION
  synth.out <- synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   genoud=FALSE)
  
  gap <- dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT - 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(synth.out$loss.v) < 2*rmspe_pre){ # what if we just did smaller than instead of smaller than twice?
    # GAP (TREATMENT EFFECT)
    storage_gap[,i] <- gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i] <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-48+1))/sqrt(sum(gap[1:t0]^2)/t0)
    
  }
  print(paste0(i," is finished!"))
}
system("say Simulation is complete!")

# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- nm_sc[nm_sc$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# plot
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-140,170),
     col= "grey",
     ylab="Difference in ESB per INMC (Hr)",
     xlab="Date",
     main="All States")
for(i in 5:length(storage_gap1)){
  lines(storage_gap1$date,storage_gap1[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap1$date,storage_gap1$V1, lwd=2,)
lines(storage_gap1$date,storage_gap1$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[t0,]$date), col=1, lty=4, lwd= 2)
legend('bottomleft', c("AZ","Control States"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)

# select only those that pre-rmspe < pre_rmspe(AZ)
le_rsmpe <- as.data.frame(t(storage_gap))
le_rsmpe$index <- rownames(le_rsmpe)
le_rsmpe$pre_rsmpe <- sqrt(rowSums(le_rsmpe[,c(1:t0)]^2)/t0)
le_rsmpe <- na.omit(le_rsmpe)
le_rsmpe <- le_rsmpe[le_rsmpe$pre_rsmpe <= rmspe_pre,c("index","pre_rsmpe")]
le_rsmpe$col_index <- paste0("V",le_rsmpe$index)

# plot with less than 1*pre-rsmpe
storage_gap2 <- as.data.frame(storage_gap) %>% 
  select(le_rsmpe$col_index)
storage_gap2$date <- storage_gap1$date
storage_gap2$synthetic <- 0

plot(storage_gap2$date,
     storage_gap2$V2,
     type = "l",
     ylim = c(-140,170),
     col= "grey",
     ylab="Difference in ESB per INMC (Hr)",
     xlab="Date",
     main="All States")
for(i in 5:length(storage_gap2)){
  lines(storage_gap2$date,storage_gap2[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap2$date,storage_gap2$V1, lwd=2,)
lines(storage_gap2$date,storage_gap2$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[t0,]$date), col=1, lty=4, lwd= 2)
legend('bottomleft', c("AZ","Control States"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)


# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# make storage_gap1 a dataframe
storage_gap1 <- as.data.frame(storage_gap1)

# save all the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"state_estimation_AZ_esb_inmc_ordered_rmspe_data.csv"), row.names = F)
write.csv(storage_gap1, paste0(dir$cleandata,"state_estimation_AZ_esb_inmc_storage_gap.csv"), row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#~~~~~~~~~~~ AZ: synthetic control package/estimation/ solar_inmc_pc_mw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this estimation is for installed capacity per customer
# if this affects newer customers, then new installations may be smaller. Less marginal benefit in a larger installation

dataprep.out <- dataprep(
  foo = nm_sc,
  predictors=c("avg_temp",
               "avg_tcc",
               "med_inc",
               "avg_price_cents_per_kwh",
               "avg_rev_usd_pc",
               "avg_consumption_kwh_pc"),
  predictors.op="mean",
  time.variable="date_numeric", # must be a numeric value
  time.predictors.prior=1:t0, # pre-treatment period
  dependent="solar_inmc_pc_kw",
  unit.variable = "state_id",
  unit.names.variable= "state",
  treatment.identifier=1,
  controls.identifier=2:id_n,
  time.optimize.ssr=1:t0, # pre-treatment period
  time.plot=1:tn) # full time period

dataprep.out$X1 # treatment predictors
dataprep.out$X0 # control predictors

dataprep.out$Z0 # treatment outcome variable
dataprep.out$Z1 # control outcome variable

# Optimization (minimizing MSPE over W and V)
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS",
                   genoud = F)
str(synth.out)
# W MATRIX
synth.out$solution.w

# ANNUAL GAP BETWEEN OUTCOME VARIABLE OF TREATMENT AND SYNTHETIC CONTROL- TREATMENT EFFECT
gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot%*%synth.out$solution.w)

# summary of gap in the pre
summary(gap[1:t0,1])
# summary of gap in the post
summary(gap[(t0+1):tn,1])

# TABLES OF RESULTS
synth.tables <- synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred

# export covariate values for synth, treated, sample
df_export <- as.data.frame(synth.tables$tab.pred)
df_export$covariates <- row.names(df_export)
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_AZ_inmc_pc_mw_cov_values.csv"),
          row.names = F)

# TABLE 2: V MATRIX
synth.tables$tab.v
# export covariate weights
df_export <- as.data.frame(synth.tables$tab.v)
df_export$covariates <- row.names(df_export)
df_export <- df_export %>%
  mutate(across(where(is.list), ~ sapply(., toString)))
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_AZ_inmc_pc_mw_weights.csv"),
          row.names = F)

# TABLE 3: W MATRIX
synth.tables$tab.w

# export state weights
df_export <- as.data.frame(synth.tables$tab.w)

write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_AZ_inmc_pc_mw_cov_values.csv"),
          row.names = F)

# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "AZ: State Level Estimation",
          Ylab= "MwH ESB Per installed Capacity",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")



# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(nm_sc$date)),
                    ncol=length(unique(nm_sc$state_id)))
storage_gap[,1]<-gap
print(storage_gap)

# RMSPE FOR PRE INTERVENTION
rmspe_pre <- sqrt(sum(gap[1:t0]^2)/t0) #MSPE: manual calc (Jan 2013- Nov 2016) (if index did not start from 1, then it would be t0-(initial)+1)
rmspe_pre

#MSPE: reported by synth(), used for verification
synth.out$loss.v
sqrt(synth.out$loss.v) # using the square root of this should give you pre-RMSPE

# RMSPE FOR THE POST INTERVENTION
rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0+1)) # manual calc
rmspe_post

# POST RMSPE/PRE RMSPE
rmspe_storage <- c(rep(NA,length(unique(nm_sc$state_id)))) # make an empty set
rmspe_storage[1] <- rmspe_post/rmspe_pre # add the ratio for index 1
rmspe_data <- data.frame(state_i=c(unique(nm_sc$state_id)), # make into a dataframe
                       ratio_rmspe=rmspe_storage)

# falsification test
for(i in 2:length(unique(nm_sc$state_id))){ # i <- 6 
  
  # DATA PREPERATION
  
  dataprep.out <- dataprep(
    foo = nm_sc,
    predictors=c("avg_temp",
                 "avg_tcc",
                 "med_inc",
                 "avg_price_cents_per_kwh",
                 "avg_rev_usd_pc",
                 "avg_consumption_kwh_pc"),
    predictors.op="mean",
    time.variable="date_numeric", # must be a numeric value
    time.predictors.prior=1:t0, # pre-treatment period
    dependent="solar_inmc_pc_kw",
    unit.variable = "state_id",
    unit.names.variable= "state",
    treatment.identifier=i,
    controls.identifier=unique(
      nm_sc[nm_sc$state_id != i,]$state_id),
    time.optimize.ssr=1:t0, # pre-treatment period
    time.plot=1:tn) # full time period
  
  # OPTIMIZATION
  synth.out <- synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   genoud=FALSE)
  
  gap <- dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT - 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(synth.out$loss.v) < 2*rmspe_pre){ # what if we just did smaller than instead of smaller than twice?
    # GAP (TREATMENT EFFECT)
    storage_gap[,i] <- gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i] <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-48+1))/sqrt(sum(gap[1:t0]^2)/t0)
    
  }
  print(paste0(i," is finished!"))
}
system("say Simulation is complete!")

# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- nm_sc[nm_sc$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# plot
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-20,20),
     col= "grey",
     ylab="Difference in Capacity (MW)",
     xlab="Date",
     main="All States")
for(i in 5:length(storage_gap1)){
  lines(storage_gap1$date,storage_gap1[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap1$date,storage_gap1$V1, lwd=2,)
lines(storage_gap1$date,storage_gap1$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[t0,]$date), col=1, lty=4, lwd= 2)
legend('bottomleft', c("AZ","Control States"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)


# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data <- na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# make storage_gap1 a dataframe
storage_gap1 <- as.data.frame(storage_gap1)

# save all the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"state_estimation_AZ_inmc_pc_mw_ordered_rmspe_data.csv"), row.names = F)
write.csv(storage_gap1, paste0(dir$cleandata,"state_estimation_AZ_inmc_pc_mw_storage_gap.csv"), row.names = F)

##### CA Synt. Control ---------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~CA: data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# identify utilities in AZ that are not investor owned
u_drop <- unique(nm_full[nm_full$state == "CA" & nm_full$IOU == 0,]$full_id)

# identify states that cannot be included for similar treatment or different treatment
# drop AK, HI, CA, GA, MI, MO, NE, ND, TN, SD, AL, TX, ID, WI, NV, KS
s_drop <- c("AZ","AK", "HI","AL", "GA", "MI", "MO", "NE", "ND", "TN", "SD", "AL", "TX", "ID", "WI", "NV", "KS")


# summarize by state, filter dates, states, utilities
nm_sc <- nm_full %>%
  filter(year > 2012 & year < 2020  & state %not_in% s_drop & full_id %not_in% u_drop) %>%
  group_by(year,month,date,state) %>%
  summarize(
    # outcome variables
    solar_esb_inmc_hr = sum(solar_esb_inmc_hr, na.rm = T), 
    solar_esb_kwh_pc = sum(solar_esb_kwh_pc, na.rm = T),
    solar_inmc_pc_kw = sum(solar_inmc_pc_kw, na.rm = T),
    # covariates
    avg_temp = mean(avg_temp, na.rm = T),
    avg_tcc = mean(avg_tcc, na.rm = T),
    cloudy_days = mean(cloudy_days, na.rm = T),
    med_inc = mean(med_inc, na.rm = T),
    avg_price_cents_per_kwh = mean(avg_price_cents_per_kwh, na.rm = T),
    avg_rev_usd_pc = mean(avg_rev_usd_pc, na.rm = T),
    avg_consumption_kwh_pc = mean(avg_consumption_kwh_pc, na.rm = T)
  ) 

# add a numeric date
date_df <- unique(nm_sc[c("year","month")])
date_df$date_numeric <- 1:nrow(date_df)
nm_sc <- left_join(nm_sc,date_df, by = c("year","month"))

# give the states an ID other than 1 and give CA 1
state <- unique(nm_sc[nm_sc$state != "CA",]$state)
state_id <- c(2:(length(state)+1))
states_df <- as.data.frame(cbind(state,state_id))
nm_sc <- left_join(nm_sc, states_df, by = "state")

nm_sc$state_id <- ifelse(nm_sc$state == 'CA',1, nm_sc$state_id)

# make this column numeric
nm_sc$state_id <- as.numeric(nm_sc$state_id)

# arrange
nm_sc <- arrange(nm_sc, state_id, date_numeric)

# is the data balanced?
35*84
nrow(nm_sc)

# turn nm_sc into a dataframe
nm_sc <- as.data.frame(nm_sc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#~~~~~~~~~~~ CA: synthetic control package/estimation/ solar_esb_inmc_hr ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this estimation is for esb per installed capacity

# all IOU utilities in CA took on successor Tariff on July 1, 2017
# select t0 as date before treatment as a numeric index 
t0 <- date_df[date_df$year == 2017 & date_df$month == 6,]$date_numeric

# select tn as final date in numeric index
tn <- date_df[date_df$year == 2019 & date_df$month == 12,]$date_numeric

# select total number of ids for controls
id_n <- max(nm_sc$state_id)


dataprep.out <- dataprep(
  foo = nm_sc,
  predictors=c("avg_temp",
               "avg_tcc",
               "med_inc",
               "avg_price_cents_per_kwh",
               "avg_rev_usd_pc",
               "avg_consumption_kwh_pc"),
  predictors.op="mean",
  time.variable="date_numeric", # must be a numeric value
  time.predictors.prior=1:t0, # pre-treatment period
  dependent="solar_esb_inmc_hr",
  unit.variable = "state_id",
  unit.names.variable= "state",
  treatment.identifier=1,
  controls.identifier=2:id_n,
  time.optimize.ssr=1:t0, # pre-treatment period
  time.plot=1:tn) # full time period

dataprep.out$X1 # treatment predictors
dataprep.out$X0 # control predictors

dataprep.out$Z0 # treatment outcome variable
dataprep.out$Z1 # control outcome variable

# Optimization (minimizing MSPE over W and V)
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS",
                   genoud = F)
str(synth.out)
# W MATRIX
synth.out$solution.w

# ANNUAL GAP BETWEEN OUTCOME VARIABLE OF TREATMENT AND SYNTHETIC CONTROL- TREATMENT EFFECT
gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot%*%synth.out$solution.w)

# summary of gap in the pre
summary(gap[1:t0,1])
# summary of gap in the post
summary(gap[(t0+1):tn,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred

# export covariate values for synth, treated, sample
df_export <- as.data.frame(synth.tables$tab.pred)
df_export$covariates <- row.names(df_export)
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_CA_esb_inmc_cov_values.csv"),
          row.names = F)

# TABLE 2: V MATRIX
synth.tables$tab.v
# export covariate weights
df_export <- as.data.frame(synth.tables$tab.v)
df_export$covariates <- row.names(df_export)
df_export <- df_export %>%
  mutate(across(where(is.list), ~ sapply(., toString)))
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_CA_esb_inmc_cov_weights.csv"),
          row.names = F)

# TABLE 3: W MATRIX
synth.tables$tab.w

# export state weights
df_export <- as.data.frame(synth.tables$tab.w)

write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_CA_esb_inmc_cov_values.csv"),
          row.names = F)

# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "CA: State Level Estimation",
          Ylab= "MwH ESB Per installed Capacity",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")



# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(nm_sc$date)),
                    ncol=length(unique(nm_sc$state_id)))
storage_gap[,1]<-gap
print(storage_gap)

# RMSPE FOR PRE INTERVENTION
rmspe_pre <- sqrt(sum(gap[1:t0]^2)/t0) #MSPE: manual calc (Jan 2013- Nov 2016) (if index did not start from 1, then it would be t0-(initial)+1)
rmspe_pre

#MSPE: reported by synth(), used for verification
synth.out$loss.v
sqrt(synth.out$loss.v) # using the square root of this should give you pre-RMSPE

# RMSPE FOR THE POST INTERVENTION
rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0+1)) # manual calc
rmspe_post

# POST RMSPE/PRE RMSPE
rmspe_storage <- c(rep(NA,length(unique(nm_sc$state_id)))) # make an empty set
rmspe_storage[1] <- rmspe_post/rmspe_pre # add the ratio for index 1
rmspe_data <- data.frame(state_i=c(unique(nm_sc$state_id)), # make into a dataframe
                         ratio_rmspe=rmspe_storage)

# falsification test
for(i in 2:length(unique(nm_sc$state_id))){ # i <- 6 
  
  # DATA PREPERATION
  
  dataprep.out <- dataprep(
    foo = nm_sc,
    predictors=c("avg_temp",
                 "avg_tcc",
                 "med_inc",
                 "avg_price_cents_per_kwh",
                 "avg_rev_usd_pc",
                 "avg_consumption_kwh_pc"),
    predictors.op="mean",
    time.variable="date_numeric", # must be a numeric value
    time.predictors.prior=1:t0, # pre-treatment period
    dependent="solar_esb_inmc_hr",
    unit.variable = "state_id",
    unit.names.variable= "state",
    treatment.identifier=i,
    controls.identifier=unique(
      nm_sc[nm_sc$state_id != i,]$state_id),
    time.optimize.ssr=1:t0, # pre-treatment period
    time.plot=1:tn) # full time period
  
  # OPTIMIZATION
  synth.out <- synth(data.prep.obj=dataprep.out,
                     method="BFGS",
                     genoud=FALSE)
  
  gap <- dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT - 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(synth.out$loss.v) < 2*rmspe_pre){ # what if we just did smaller than instead of smaller than twice?
    # GAP (TREATMENT EFFECT)
    storage_gap[,i] <- gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i] <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-48+1))/sqrt(sum(gap[1:t0]^2)/t0)
    
  }
  print(paste0(i," is finished!"))
}
system("say Simulation is complete!")

# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- nm_sc[nm_sc$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# plot
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-140,170),
     col= "grey",
     ylab="Difference in ESB per INMC (Hr)",
     xlab="Date",
     main="All States")
for(i in 5:length(storage_gap1)){
  lines(storage_gap1$date,storage_gap1[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap1$date,storage_gap1$V1, lwd=2,)
lines(storage_gap1$date,storage_gap1$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[t0,]$date), col=1, lty=4, lwd= 2)
legend('bottomleft', c("CA","Control States"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)

# select only those that pre-rmspe < pre_rmspe(CA)
le_rsmpe <- as.data.frame(t(storage_gap))
le_rsmpe$index <- rownames(le_rsmpe)
le_rsmpe$pre_rsmpe <- sqrt(rowSums(le_rsmpe[,c(1:t0)]^2)/t0)
le_rsmpe <- na.omit(le_rsmpe)
le_rsmpe <- le_rsmpe[le_rsmpe$pre_rsmpe <= rmspe_pre,c("index","pre_rsmpe")]
le_rsmpe$col_index <- paste0("V",le_rsmpe$index)

# plot with less than 1*pre-rsmpe
storage_gap2 <- as.data.frame(storage_gap) %>% 
  select(le_rsmpe$col_index)
storage_gap2$date <- storage_gap1$date
storage_gap2$synthetic <- 0

plot(storage_gap2$date,
     storage_gap2$V2,
     type = "l",
     ylim = c(-140,170),
     col= "grey",
     ylab="Difference in ESB per INMC (Hr)",
     xlab="Date",
     main="All States")
for(i in 5:length(storage_gap2)){
  lines(storage_gap2$date,storage_gap2[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap2$date,storage_gap2$V1, lwd=2,)
lines(storage_gap2$date,storage_gap2$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[t0,]$date), col=1, lty=4, lwd= 2)
legend('bottomleft', c("CA","Control States"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)


# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# make storage_gap1 a dataframe
storage_gap1 <- as.data.frame(storage_gap1)

# save all the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"state_estimation_CA_esb_inmc_ordered_rmspe_data.csv"), row.names = F)
write.csv(storage_gap1, paste0(dir$cleandata,"state_estimation_CA_esb_inmc_storage_gap.csv"), row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#~~~~~~~~~~~ CA: synthetic control package/estimation/ solar_inmc_pc_mw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this estimation is for installed capacity per customer
# if this affects newer customers, then new installations may be smaller. Less marginal benefit in a larger installation

dataprep.out <- dataprep(
  foo = nm_sc,
  predictors=c("avg_temp",
               "avg_tcc",
               "med_inc",
               "avg_price_cents_per_kwh",
               "avg_rev_usd_pc",
               "avg_consumption_kwh_pc"),
  predictors.op="mean",
  time.variable="date_numeric", # must be a numeric value
  time.predictors.prior=1:t0, # pre-treatment period
  dependent="solar_inmc_pc_kw",
  unit.variable = "state_id",
  unit.names.variable= "state",
  treatment.identifier=1,
  controls.identifier=2:id_n,
  time.optimize.ssr=1:t0, # pre-treatment period
  time.plot=1:tn) # full time period

dataprep.out$X1 # treatment predictors
dataprep.out$X0 # control predictors

dataprep.out$Z0 # treatment outcome variable
dataprep.out$Z1 # control outcome variable

# Optimization (minimizing MSPE over W and V)
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS",
                   genoud = F)
str(synth.out)
# W MATRIX
synth.out$solution.w

# ANNUAL GAP BETWEEN OUTCOME VARIABLE OF TREATMENT AND SYNTHETIC CONTROL- TREATMENT EFFECT
gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot%*%synth.out$solution.w)

# summary of gap in the pre
summary(gap[1:t0,1])
# summary of gap in the post
summary(gap[(t0+1):tn,1])

# TABLES OF RESULTS
synth.tables <- synth.tab(dataprep.res=dataprep.out,
                          synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred

# export covariate values for synth, treated, sample
df_export <- as.data.frame(synth.tables$tab.pred)
df_export$covariates <- row.names(df_export)
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_CA_inmc_pc_mw_cov_values.csv"),
          row.names = F)

# TABLE 2: V MATRIX
synth.tables$tab.v
# export covariate weights
df_export <- as.data.frame(synth.tables$tab.v)
df_export$covariates <- row.names(df_export)
df_export <- df_export %>%
  mutate(across(where(is.list), ~ sapply(., toString)))
write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_CA_inmc_pc_mw_weights.csv"),
          row.names = F)

# TABLE 3: W MATRIX
synth.tables$tab.w

# export state weights
df_export <- as.data.frame(synth.tables$tab.w)

write.csv(df_export, 
          paste0(dir$cleandata,"state_estimation_CA_inmc_pc_mw_cov_values.csv"),
          row.names = F)

# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "CA: State Level Estimation",
          Ylab= "MwH ESB Per installed Capacity",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")



# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(nm_sc$date)),
                    ncol=length(unique(nm_sc$state_id)))
storage_gap[,1]<-gap
print(storage_gap)

# RMSPE FOR PRE INTERVENTION
rmspe_pre <- sqrt(sum(gap[1:t0]^2)/t0) #MSPE: manual calc (Jan 2013- Nov 2016) (if index did not start from 1, then it would be t0-(initial)+1)
rmspe_pre

#MSPE: reported by synth(), used for verification
synth.out$loss.v
sqrt(synth.out$loss.v) # using the square root of this should give you pre-RMSPE

# RMSPE FOR THE POST INTERVENTION
rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0+1)) # manual calc
rmspe_post

# POST RMSPE/PRE RMSPE
rmspe_storage <- c(rep(NA,length(unique(nm_sc$state_id)))) # make an empty set
rmspe_storage[1] <- rmspe_post/rmspe_pre # add the ratio for index 1
rmspe_data <- data.frame(state_i=c(unique(nm_sc$state_id)), # make into a dataframe
                         ratio_rmspe=rmspe_storage)

# falsification test
for(i in 2:length(unique(nm_sc$state_id))){ # i <- 6 
  
  # DATA PREPERATION
  
  dataprep.out <- dataprep(
    foo = nm_sc,
    predictors=c("avg_temp",
                 "avg_tcc",
                 "med_inc",
                 "avg_price_cents_per_kwh",
                 "avg_rev_usd_pc",
                 "avg_consumption_kwh_pc"),
    predictors.op="mean",
    time.variable="date_numeric", # must be a numeric value
    time.predictors.prior=1:t0, # pre-treatment period
    dependent="solar_inmc_pc_kw",
    unit.variable = "state_id",
    unit.names.variable= "state",
    treatment.identifier=i,
    controls.identifier=unique(
      nm_sc[nm_sc$state_id != i,]$state_id),
    time.optimize.ssr=1:t0, # pre-treatment period
    time.plot=1:tn) # full time period
  
  # OPTIMIZATION
  synth.out <- synth(data.prep.obj=dataprep.out,
                     method="BFGS",
                     genoud=FALSE)
  
  gap <- dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT - 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(synth.out$loss.v) < 2*rmspe_pre){ # what if we just did smaller than instead of smaller than twice?
    # GAP (TREATMENT EFFECT)
    storage_gap[,i] <- gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i] <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-48+1))/sqrt(sum(gap[1:t0]^2)/t0)
    
  }
  print(paste0(i," is finished!"))
}
system("say Simulation is complete!")

# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- nm_sc[nm_sc$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# plot
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-20,20),
     col= "grey",
     ylab="Difference in Capacity (MW)",
     xlab="Date",
     main="All States")
for(i in 5:length(storage_gap1)){
  lines(storage_gap1$date,storage_gap1[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap1$date,storage_gap1$V1, lwd=2,)
lines(storage_gap1$date,storage_gap1$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[t0,]$date), col=1, lty=4, lwd= 2)
legend('bottomleft', c("CA","Control States"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)


# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data <- na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# make storage_gap1 a dataframe
storage_gap1 <- as.data.frame(storage_gap1)

# save all the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"state_estimation_CA_inmc_pc_mw_ordered_rmspe_data.csv"), row.names = F)
write.csv(storage_gap1, paste0(dir$cleandata,"state_estimation_CA_inmc_pc_mw_storage_gap.csv"), row.names = F)

