# utility level estimation
#### Estimation 2014 to 2019
# Title: Data viz and econometric estimation
# Author: Jesus Felix
# Start date: 9 NOV 2024 (My homework is due sooooon!!)
################################################################################
# This version does municipio level data first then summarizes for the states

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


##### Load data utility level -----------------------------------------------------------
# add the cloud data from the utility data
utility_all <- read.csv(paste0(dir$cleandata,"utility_nm_full_covariates_10apr2025.csv"))
utility_all <- utility_all[utility_all$year < 2020,]


# change the cloud coverage to reflect the 75% threshold
cloud_tcc <- read.csv(paste0(dir$cleandata, "monthly_cloudy_days_utility_all_75pcnt_cov.csv"))

utility_all <- utility_all %>% select(-cloudy_days)

utility_all <- left_join(utility_all, cloud_tcc %>% select(-eiaid,-utility),
                         by = c("year","month","full_id_y","full_id"))

# make land m_sq to km_sq
utility_all$lnd_sqm <- utility_all$lnd_sqm/1000000
utility_all <- rename(utility_all, "land_sq_km" = "lnd_sqm")

# to make sure we only used balanced panel, confirm the length of times observed by each utility
# each should have 84, if less, kick out
obs_u <- utility_all %>%
  group_by(full_id) %>% tally()

full_obs <- obs_u[obs_u$n == 72,]

utility_all <- utility_all[utility_all$full_id %in% full_obs$full_id,]


# also remove those with na's in covariates
bad_cov_list1 <- unique(utility_all[is.na(utility_all$cloudy_days),]$full_id)
bad_cov_list2 <- unique(utility_all[is.na(utility_all$avg_temp) & 
                        utility_all$year < 2020,]$full_id)
bad_cov_list3 <- unique(utility_all[is.na(utility_all$land_sq_km),]$full_id)
bad_cov_list4 <- unique(utility_all[is.na(utility_all$longitude),]$full_id)
bad_cov_list4 <- unique(utility_all[is.na(utility_all$usd_per_kwh),]$full_id)

bad_cov_list <- unique(c(bad_cov_list1,bad_cov_list2,bad_cov_list3,bad_cov_list4))

utility_all <- utility_all[utility_all$full_id %not_in% bad_cov_list,]

# make a date numeric value
utility_all$date <- date(utility_all$date)

u_date <- as.data.frame(unique(utility_all$date))
u_date$date_numeric <- 1:length(u_date$`unique(utility_all$date)`)
colnames(u_date) <- c("date","date_numeric")
utility_all <- left_join(utility_all,u_date, by = "date")


# Identify utilities in AZ
AZ_utilities <- utility_all[utility_all$state == "AZ",]

# identify utilities that in the following states:
# drop AK, HI, CA, GA, MI, MO, NE, ND, TN, SD, AL, TX, ID, WI, NV, KS
s_drop <- c("AK", "HI","AL", "CA", "GA", "MI", "MO", "NE", "ND", "TN", "SD", "AL", "TX", "ID", "WI", "NV", "KS","NM",
            "LA","UT")

b_states <- utility_all[utility_all$state %in% s_drop,]

# identify good utilities in b_states
g_id <- c("ID_9191","CA_11208","ID_9191","NM_5701","NM_15473","TX_5701","TN_10331")
g_utilities <- b_states[b_states$full_id %in% g_id,]

# make these utilities seperate from data
u_df <- utility_all[utility_all$full_id %not_in% c(AZ_utilities$full_id,
                                                   b_states$full_id),]
# add the good utilities
u_df <- rbind(u_df,g_utilities)



#### Estimation for TEP ------------------------------------------------------------------------

# select TEP from AZ_utilities
tep <- AZ_utilities[AZ_utilities$utility_name == "Tucson Electric Power Co",]

# # theres a month in the TEP data that messes the average
# tep_jul_14 <- tep[tep$date == "2014-07-01",]
# # make a model to fill in this energy sold back
# esb <- lm(nm_per_cust ~ factor(month) + cloudy_days + avg_temp + med_inc + usd_per_kwh +
#             res_customers,
#           data = tep)
# tep_jul_14$month <- as.factor(tep_jul_14$month)
# nm_jul_14 <- predict(esb, tep_jul_14[c("month","cloudy_days","avg_temp","med_inc",
#                                        "usd_per_kwh","res_customers")])
# tep[tep$date == "2014-07-01",]$nm_per_cust <- as.numeric(nm_jul_14)
# make the identifers for the estimation
tep$est_id <- 1

id_list <- as.data.frame(unique(u_df$full_id))
id_list$est_id <- 2:(length(id_list$`unique(u_df$full_id)`)+1)
colnames(id_list) <- c("full_id","est_id")

# combine these to u_df
u_df <- left_join(u_df,id_list, by = "full_id")

# add tep
u_df <- rbind(tep, u_df)
u_df <- arrange(u_df, year, month, est_id)

# is it balanced?
length(unique(u_df$full_id))
length(unique(u_df$full_id))*72
dim(u_df) # if match, then balanced


#select time of treatment
t0 <- 37
t1 <- 72 # last time ob

data <- u_df 

dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp",
               "cloudy_days",
               "med_inc",
               "usd_per_kwh",
               "land_sq_km",
               "avg_consumption_kwh",
               "avg_cost",
               "latitude"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="est_id",
  unit.names.variable="full_id",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:length(id_list$est_id)+1),
  time.optimize.ssr=1:t0,
  time.plot=1:t1)

dataprep.out$X1 # treatment predictors
dataprep.out$X0 # control predictors

dataprep.out$Z0 # treatment outcome variable
dataprep.out$Z1 # control outcome variable

# Optimization (minimizing MSPE over W and V)
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS",
                   Margin.ipop = 0.0008,
                   genoud = F)

str(synth.out)
# W MATRIX
synth.out$solution.w

# ANNUAL GAP BETWEEN OUTCOME VARIABLE OF TREATMENT AND SYNTHETIC CONTROL- TREATMENT EFFECT
gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot%*%synth.out$solution.w)
gap[1:3,1]
summary(gap[(t0+1):t1,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred
sc_x_tab <- as.data.frame(synth.tables$tab.pred)
sc_x_tab$Variable <- c("Avg. Temp. C","N Cloudy Days","Median Inc. per cap.","USD per kWh",
                       "Land sq. km","Avg. kWh consumption","Avg. Cost to utility (USD)",
                       "central latittude")
sc_x_tab <- sc_x_tab[,c(4,1:3)]
# save dataframe
write.csv(sc_x_tab,paste0(dir$cleandata,"variable_table_synthetic_TEP_all_valid_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)
# save dataframe
write.csv(sc_v_tab,paste0(dir$cleandata,"variable_table_synthetic_weights_TEP_all_valid_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("Utility State and Number","Utility ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:length(sc_w_tab$`Utility State and Number`)

# save dataframe
write.csv(sc_w_tab,paste0(dir$cleandata,"weight_table_TEP_all_valid_utilities",
                          format(Sys.time(),"%d_%b_%Y"),".csv"),
          row.names = F)


# g1 <- sc_w_tab[c(1:10),c(1,3)]
# g2 <- sc_w_tab[c(11:20),c(1,3)]
# g3 <- sc_w_tab[c(21:30),c(1,3)]
# g4 <- sc_w_tab[c(31:40),c(1,3)]
# g5 <- sc_w_tab[c(41:47),c(1,3)]
# 
# tab <- as.data.frame(cbind(g1,g2,g3,g4,rbind(g5,c(NA,NA),c(NA,NA),c(NA,NA))))
# colnames(tab) <- c("a","b","c","d","e","f","g","h","i","j")
# 
# flextable(tab) %>%
#   align(align = "center", part = "all") %>%
#   set_header_labels(top = F,values = rep(c("State","W"),5))

date_az <- data[data$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
png(paste0(dir$fig,"treatment_synthetic/synthetic_TEP_all_valid_utilities_",
           format(Sys.time(),"%d_%b_%Y"),".png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "Utility Level Estimation: TEP All Valid Utilities",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")
dev.off()

# Plot with confidence intervals




# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_TEP_all_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "TEP Gap Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)
dev.off()

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gapna<-matrix(NA, nrow=length(unique(data$date)),
                    ncol=length(unique(data$est_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:t0]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0)) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$est_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(Utility_id=c(unique(data$est_id)),
                       ratio_rmspe=rmspe_storage)


listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:20,]

# falsification test
for(i in 95:length(unique(data$est_id))){ # i <- 72 
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp",
                 "cloudy_days",
                 "med_inc",
                 "usd_per_kwh",
                 "land_sq_km",
                 "avg_consumption_kwh",
                 "avg_cost",
                 "latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="est_id",
    unit.names.variable="full_id",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier= unique(
      data[data$est_id != i,]$est_id),
    time.optimize.ssr=1:t0,
    time.plot=1:t1)
  
  

  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   Margin.ipop = 0.005,
                   genoud=FALSE)
  
  # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  # path.plot(synth.res=synth.out,
  #           dataprep.res=dataprep.out,
  #           tr.intake = t0,
  #           Main = "Placebo: Real and Synthetic",
  #           Ylab= "kWh Per Customer",
  #           Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:t0]^2)/(t0))< 2*(sqrt(sum(storage_gap[1:t0,1]^2)/(t0)))){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0))/sqrt(sum(gap[1:t0]^2)/(t0-1))
  } 
  print(i)
}
system("say Simulation is complete!")
# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- data[data$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# save the data
write.csv(storage_gap1,paste0(dir$cleandata,"storage_gap_TEP_all_valid_utilities",
                              format(Sys.time(),"%d_%b_%Y"),".csv"), 
          row.names = F)

stg_1 <- read.csv(paste0(dir$cleandata,
                         'storage_gap_TEP_all_valid_utilities15_Apr_2025.csv'))

# from this let's select the best controls based on the pre-treatment
stg_f <- stg_1[stg_1$date < "2017-01-01",]
stg_sum <- NULL
for(i in 4:length(stg_f)){ # i <- 4
  
  dif <- stg_f[,3] -stg_f[,i]
  
  rmspe <- dif^2
  
  avg_rmspe <- mean(rmspe)
  
  stg_df <- as.data.frame(cbind(names(stg_f[i]),avg_rmspe))
  
  stg_sum <- rbind(stg_sum, stg_df)
  
}



stg_rmspe <- arrange(stg_sum, avg_rmspe)

top_20_utilities <- as.numeric(gsub("V","",stg_rmspe$V1[1:20]))
                         
best_utilities <- u_df[u_df$est_id %in% 
                                top_20_utilities,]

####### tep with best 20 utilities -------------------------------


# select TEP from AZ_utilities
tep <- AZ_utilities[AZ_utilities$utility_name == "Tucson Electric Power Co",]

# # theres a month in the TEP data that messes the average
# tep_jul_14 <- tep[tep$date == "2014-07-01",]
# # make a model to fill in this energy sold back
# esb <- lm(nm_per_cust ~ factor(month) + cloudy_days + avg_temp + med_inc + usd_per_kwh +
#             res_customers,
#           data = tep)
# tep_jul_14$month <- as.factor(tep_jul_14$month)
# nm_jul_14 <- predict(esb, tep_jul_14[c("month","cloudy_days","avg_temp","med_inc",
#                                        "usd_per_kwh","res_customers")])
# tep[tep$date == "2014-07-01",]$nm_per_cust <- as.numeric(nm_jul_14)
# make the identifers for the estimation
tep$est_id <- 1

id_list <- as.data.frame(unique(best_utilities$full_id))
id_list$est_id <- 2:(length(id_list$`unique(best_utilities$full_id)`)+1)
colnames(id_list) <- c("full_id","est_id")

# combine these to u_df
best_utilities <- left_join(best_utilities[-c(29)],id_list, by = "full_id")

# add tep
best_utilities <- rbind(tep, best_utilities)
best_utilities <- arrange(best_utilities, year, month, est_id)

# is it balanced?
length(unique(best_utilities$full_id))
length(unique(best_utilities$full_id))*72
dim(best_utilities) # if match, then balanced


#select time of treatment
t0 <- 37
t1 <- 72 # last time ob

data <- best_utilities 

dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp",
               "cloudy_days",
               "med_inc"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="est_id",
  unit.names.variable="full_id",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:length(id_list$est_id)+1),
  time.optimize.ssr=1:t0,
  time.plot=1:t1)

dataprep.out$X1 # treatment predictors
dataprep.out$X0 # control predictors

dataprep.out$Z0 # treatment outcome variable
dataprep.out$Z1 # control outcome variable

# Optimization (minimizing MSPE over W and V)
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS",
                   Margin.ipop = 0.0008,
                   genoud = F)

str(synth.out)
# W MATRIX
synth.out$solution.w

# ANNUAL GAP BETWEEN OUTCOME VARIABLE OF TREATMENT AND SYNTHETIC CONTROL- TREATMENT EFFECT
gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot%*%synth.out$solution.w)
gap[1:3,1]
summary(gap[(t0+1):t1,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred
sc_x_tab <- as.data.frame(synth.tables$tab.pred)
sc_x_tab$Variable <- c("Avg. Temp. C","N Cloudy Days","Median Inc. per cap.")
sc_x_tab <- sc_x_tab[,c(4,1:3)]
# save dataframe
write.csv(sc_x_tab,paste0(dir$cleandata,"variable_table_synthetic_TEP_20_lesscov_valid_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)
# save dataframe
write.csv(sc_v_tab,paste0(dir$cleandata,"variable_table_synthetic_weights_TEP_20_lesscovvalid_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("Utility State and Number","Utility ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:length(sc_w_tab$`Utility State and Number`)

# save dataframe
write.csv(sc_w_tab,paste0(dir$cleandata,"weight_table_TEP_20_lesscov_valid_utilities",
                          format(Sys.time(),"%d_%b_%Y"),".csv"),
          row.names = F)


# g1 <- sc_w_tab[c(1:10),c(1,3)]
# g2 <- sc_w_tab[c(11:20),c(1,3)]
# g3 <- sc_w_tab[c(21:30),c(1,3)]
# g4 <- sc_w_tab[c(31:40),c(1,3)]
# g5 <- sc_w_tab[c(41:47),c(1,3)]
# 
# tab <- as.data.frame(cbind(g1,g2,g3,g4,rbind(g5,c(NA,NA),c(NA,NA),c(NA,NA))))
# colnames(tab) <- c("a","b","c","d","e","f","g","h","i","j")
# 
# flextable(tab) %>%
#   align(align = "center", part = "all") %>%
#   set_header_labels(top = F,values = rep(c("State","W"),5))

date_az <- data[data$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
png(paste0(dir$fig,"treatment_synthetic/synthetic_TEP_20_valid_utilities_",
           format(Sys.time(),"%d_%b_%Y"),".png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "Utility Level Estimation: TEP & 20 Valid Utilities",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")
dev.off()
# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_TEP_20_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "TEP Gap Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)
dev.off()

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(data$date)),
                      ncol=length(unique(data$est_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:t0]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0)) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$est_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(Utility_id=c(unique(data$est_id)),
                       ratio_rmspe=rmspe_storage)


listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:20,]

# falsification test
for(i in 1:length(unique(data$est_id))){ # i <- 72 
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp",
                 "cloudy_days",
                 "med_inc",
                 "usd_per_kwh",
                 "land_sq_km",
                 "avg_consumption_kwh",
                 "avg_cost",
                 "latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="est_id",
    unit.names.variable="full_id",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier= unique(
      data[data$est_id != i,]$est_id),
    time.optimize.ssr=1:t0,
    time.plot=1:t1)
  
  
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   Margin.ipop = 0.005,
                   genoud=FALSE)
  
  # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  # path.plot(synth.res=synth.out,
  #           dataprep.res=dataprep.out,
  #           tr.intake = t0,
  #           Main = "Placebo: Real and Synthetic",
  #           Ylab= "kWh Per Customer",
  #           Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:t0]^2)/(t0))< 2*(sqrt(sum(storage_gap[1:t0,1]^2)/(t0)))){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0))/sqrt(sum(gap[1:t0]^2)/(t0-1))
  } 
  print(i)
}
system("say Simulation is complete!")
# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- data[data$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# save the data
write.csv(storage_gap1,paste0(dir$cleandata,"storage_gap_TEP_20_valid_utilities",
                              format(Sys.time(),"%d_%b_%Y"),".csv"), 
          row.names = F)







# PLOT
png(paste0(dir$fig,"tests/gap_test_TEP_20_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"), width = 600, height = 500)
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-140,700),
     col= "gray",
     ylab="Difference in NMPC (kWh)",
     xlab="Date",
     main="All Utilities Gap Test")

for(i in 5:length(storage_gap1)){
  lines(storage_gap1$date,storage_gap1[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap1$date,storage_gap1$V1, lwd=2,)
lines(storage_gap1$date,storage_gap1$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[t0,]$date), col=1, lty=4, lwd= 2)
legend('bottomleft', c("TEP","Control Utilities"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)
dev.off()

# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# save the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"rmspe_data_ordered_TEP_20_valid_utilities",
                                    format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# PLOT
#add state abbreviation
u_abb <- as.data.frame(cbind(unique(data$full_id),unique(data$est_id)))
colnames(u_abb) <- c("Utility","Utility_id")
u_abb$Utility_id <- as.numeric(u_abb$Utility_id)
rmspe_data1<- left_join(rmspe_data_ordered,u_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
pchs <- c(20,24)
pchs[rmspe_data1[,1]!=1] <- 20
pchs[rmspe_data1[,1]==1] <- 24

png(paste0(dir$fig,"tests/","ratio_test_20_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"), width = 300, height = 700)
dotchart(rmspe_data1[,2],
         labels=rmspe_data1[,3],
         main="All States",
         xlab="Post RMSPE/Pre RMSPE",
         ylab="",
         col=cols,
         pch=pchs,
         cex=0.9)
dev.off()







#####
#####
####
####
#####
# select TEP from AZ_utilities
tep <- AZ_utilities[AZ_utilities$utility_name == "Tucson Electric Power Co",]

# make the identifers for the estimation
tep$est_id <- 1

id_list <- as.data.frame(unique(u_df$full_id))
id_list$est_id <- 2:(length(id_list$`unique(u_df$full_id)`)+1)
colnames(id_list) <- c("full_id","est_id")

# combine these to u_df
u_df <- left_join(u_df,id_list, by = "full_id")

# add tep
u_df <- rbind(tep, u_df)
u_df <- arrange(u_df, year, month, est_id)

# is it balanced?
length(unique(u_df$full_id))
length(unique(u_df$full_id))*84
dim(u_df) # if match, then balanced


#select time of treatment
t0 <- 37
t1 <- 72 # last time ob

data <- u_df 

dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp",
               "cloudy_days",
               "med_inc",
               "usd_per_kwh",
               "lnd_sqm",
               "longitude",
               "latitude"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="est_id",
  unit.names.variable="full_id",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:length(id_list$est_id)+1),
  time.optimize.ssr=1:t0,
  time.plot=1:t1)

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
gap[1:3,1]
summary(gap[(t0+1):t1,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred
sc_x_tab <- as.data.frame(synth.tables$tab.pred)
sc_x_tab$Variable <- c("Avg. Temp. C","Avg. Cloudy Days","Median Inc. per cap.","USD per kWh",
                       "M Sq. Land","central longitude","central latittude")
sc_x_tab <- sc_x_tab[,c(4,1:3)]
# save dataframe
write.csv(sc_x_tab,paste0(dir$cleandata,"variable_table_synthetic_TEP_all_valid_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)
# save dataframe
write.csv(sc_v_tab,paste0(dir$cleandata,"variable_table_synthetic_weights_all_valid_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("Utility State and Number","Utility ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:length(sc_w_tab$`Utility State and Number`)

# save dataframe
write.csv(sc_w_tab,paste0(dir$cleandata,"weight_table_TEP_all_valid_utilities",
                          format(Sys.time(),"%d_%b_%Y"),".csv"),
          row.names = F)


# g1 <- sc_w_tab[c(1:10),c(1,3)]
# g2 <- sc_w_tab[c(11:20),c(1,3)]
# g3 <- sc_w_tab[c(21:30),c(1,3)]
# g4 <- sc_w_tab[c(31:40),c(1,3)]
# g5 <- sc_w_tab[c(41:47),c(1,3)]
# 
# tab <- as.data.frame(cbind(g1,g2,g3,g4,rbind(g5,c(NA,NA),c(NA,NA),c(NA,NA))))
# colnames(tab) <- c("a","b","c","d","e","f","g","h","i","j")
# 
# flextable(tab) %>%
#   align(align = "center", part = "all") %>%
#   set_header_labels(top = F,values = rep(c("State","W"),5))

date_az <- data[data$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
png(paste0(dir$fig,"treatment_synthetic/synthetic_TEP_all_valid_utilities_",
           format(Sys.time(),"%d_%b_%Y"),".png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "Utility Level Estimation: TEP All Valid Utilities",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")
dev.off()
# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_TEP_all_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)
dev.off()

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gapna<-matrix(NA, nrow=length(unique(data$date)),
                      ncol=length(unique(data$est_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:t0]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0)) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$est_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(data$est_id)),
                       ratio_rmspe=rmspe_storage)


listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:20,]

# falsification test
for(i in 82:length(unique(data$est_id))){ # i <- 72 
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp",
                 "cloudy_days",
                 "med_inc",
                 "usd_per_kwh",
                 "lnd_sqm",
                 "longitude",
                 "latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="est_id",
    unit.names.variable="full_id",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier= unique(
      data[data$est_id != i,]$est_id),
    time.optimize.ssr=1:t0,
    time.plot=1:t1)
  
  
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   Margin.ipop = 0.001,
                   genoud=FALSE)
  
  # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  # path.plot(synth.res=synth.out,
  #           dataprep.res=dataprep.out,
  #           tr.intake = t0,
  #           Main = "Placebo: Real and Synthetic",
  #           Ylab= "kWh Per Customer",
  #           Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:t0]^2)/(t0))< 2*(sqrt(sum(storage_gap[1:t0,1]^2)/(t0)))){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0))/sqrt(sum(gap[1:t0]^2)/(t0-1))
  } 
  print(i)
}
system("say Simulation is complete!")
# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- data[data$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# save the data
write.csv(storage_gap1,paste0(dir$cleandata,"storage_gapall_valid_utilities",
                              format(Sys.time(),"%d_%b_%Y"),".csv"), 
          row.names = F)

# PLOT
png(paste0(dir$fig,"tests/gap_test_TEP_all_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"), width = 600, height = 500)
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-140,170),
     col= "gray",
     ylab="Difference in NMPC (kWh)",
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
legend('bottomleft', c("TEP","Control Utilities"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)
dev.off()

# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# save the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"rmspe_data_ordered_TEP_all_valid_utilities",
                                    format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# PLOT
#add state abbreviation
u_name <- unique(u_df[,c("utility_name","full_id","est_id")])
rmspe_data1<- left_join(rmspe_data_ordered,u_name, by = c("Utility_Id" = "est_id"))


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
pchs <- c(20,24)
pchs[rmspe_data1[,1]!=1] <- 20
pchs[rmspe_data1[,1]==1] <- 24

png(paste0(dir$fig,"tests/","ratio_test_all_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"), width = 300, height = 700)
dotchart(rmspe_data1[,2],
         labels=NULL,
         main="TEP and all Valid Utilities",
         xlab="Post RMSPE/Pre RMSPE",
         ylab="",
         col=cols,
         pch=pchs,
         cex=0.9)
legend(legend = c("Other utilities", "TEP"), "bottomright",
       col = c('black','blue'), pch =  c(20,24))
dev.off()
####
#### Estimation for TEP (top 50 Utilities) ---------------------------------
# select TEP from AZ_utilities
tep <- AZ_utilities[AZ_utilities$utility_name == "Tucson Electric Power Co",]

# select top 20 utilities from previous estimation
sc_w_tab_all <- sc_w_tab
listw <- arrange(sc_w_tab_all,desc(Weight))
listw <- listw[1:20,]
u_df <- u_df[u_df$full_id %in% listw$`Utility State and Number`,]


# make the identifers for the estimation
tep$est_id <- 1

id_list <- as.data.frame(unique(u_df$full_id))
id_list$est_id <- 2:(length(id_list$`unique(u_df$full_id)`)+1)
colnames(id_list) <- c("full_id","est_id")

# combine these to u_df
u_df <- left_join(u_df[,-c(29)],id_list, by = "full_id")

# add tep
u_df <- rbind(tep, u_df)
u_df <- arrange(u_df, year, month, est_id)

# is it balanced?
length(unique(u_df$full_id))
length(unique(u_df$full_id))*72
dim(u_df) # if match, then balanced


#select time of treatment
t0 <- 37
t1 <- 72 # last time ob

data <- u_df 

dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp",
               "cloudy_days",
               "med_inc",
               "usd_per_kwh",
               "land_sq_km",
               "latitude"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="est_id",
  unit.names.variable="full_id",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:length(id_list$est_id)+1),
  time.optimize.ssr=1:t0,
  time.plot=1:t1)

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
gap[1:3,1]
summary(gap[(t0+1):t1,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred
sc_x_tab <- as.data.frame(synth.tables$tab.pred)
sc_x_tab$Variable <- c("Avg. Temp. C","N Cloudy Days","Median Inc. per cap.","USD per kWh",
                       "Land sq. km","central latittude")
sc_x_tab <- sc_x_tab[,c(4,1:3)]
# save dataframe
write.csv(sc_x_tab,paste0(dir$cleandata,"variable_table_synthetic_TEP_20_top_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)
# save dataframe
write.csv(sc_v_tab,paste0(dir$cleandata,"variable_table_synthetic_weights_tep_20_top_utilities_",
                          format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- left_join(sc_w_tab, u_df[c("full_id","utility_name")],
                      by = c("unit.names" = "full_id"), 
                      keep = F,
                      multiple = "first")
sc_w_tab <- sc_w_tab[,c(4,3,1)]
colnames(sc_w_tab) <- c("Utility Name","Utility ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:length(sc_w_tab$`Utility Name`)

# save dataframe
write.csv(sc_w_tab,paste0(dir$cleandata,"weight_table_TEP_20_top_utilities",
                          format(Sys.time(),"%d_%b_%Y"),".csv"),
          row.names = F)


# g1 <- sc_w_tab[c(1:10),c(1,3)]
# g2 <- sc_w_tab[c(11:20),c(1,3)]
# g3 <- sc_w_tab[c(21:30),c(1,3)]
# g4 <- sc_w_tab[c(31:40),c(1,3)]
# g5 <- sc_w_tab[c(41:47),c(1,3)]
# 
# tab <- as.data.frame(cbind(g1,g2,g3,g4,rbind(g5,c(NA,NA),c(NA,NA),c(NA,NA))))
# colnames(tab) <- c("a","b","c","d","e","f","g","h","i","j")
# 
# flextable(tab) %>%
#   align(align = "center", part = "all") %>%
#   set_header_labels(top = F,values = rep(c("State","W"),5))

date_az <- data[data$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
png(paste0(dir$fig,"treatment_synthetic/synthetic_TEP_all_valid_utilities_",
           format(Sys.time(),"%d_%b_%Y"),".png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "Utility Level Estimation: TEP & top 20 Utilities",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")
dev.off()
# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_TEP_all_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)
dev.off()

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gapna<-matrix(NA, nrow=length(unique(data$date)),
                      ncol=length(unique(data$est_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:t0]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0)) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$est_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(data$est_id)),
                       ratio_rmspe=rmspe_storage)




# falsification test
for(i in 82:length(unique(data$est_id))){ # i <- 72 
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp",
                 "cloudy_days",
                 "med_inc",
                 "usd_per_kwh",
                 "lnd_sqm",
                 "longitude",
                 "latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="est_id",
    unit.names.variable="full_id",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier= unique(
      data[data$est_id != i,]$est_id),
    time.optimize.ssr=1:t0,
    time.plot=1:t1)
  
  
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   Margin.ipop = 0.001,
                   genoud=FALSE)
  
  # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  # path.plot(synth.res=synth.out,
  #           dataprep.res=dataprep.out,
  #           tr.intake = t0,
  #           Main = "Placebo: Real and Synthetic",
  #           Ylab= "kWh Per Customer",
  #           Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:t0]^2)/(t0))< 2*(sqrt(sum(storage_gap[1:t0,1]^2)/(t0)))){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):t1]^2)/(t1-t0))/sqrt(sum(gap[1:t0]^2)/(t0-1))
  } 
  print(i)
}
system("say Simulation is complete!")
# remove na columns
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- data[data$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# save the data
write.csv(storage_gap1,paste0(dir$cleandata,"storage_gapall_valid_utilities",
                              format(Sys.time(),"%d_%b_%Y"),".csv"), 
          row.names = F)

# PLOT
png(paste0(dir$fig,"tests/gap_test_TEP_all_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"), width = 600, height = 500)
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-140,170),
     col= "gray",
     ylab="Difference in NMPC (kWh)",
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
legend('bottomleft', c("TEP","Control Utilities"),
       lty=1,
       lwd=2,
       col=c(1,"Gray"),
       bty='n',
       border=NA)
dev.off()

# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# save the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"rmspe_data_ordered_TEP_all_valid_utilities",
                                    format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# PLOT
#add state abbreviation
u_abb <- as.data.frame(cbind(unique(data$full_id),unique(data$est_id)))
colnames(u_abb) <- c("Utility","Utility_id")
u_abb$Utility_id <- as.numeric(u_abb$Utility_id)
rmspe_data1<- left_join(rmspe_data_ordered,u_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
pchs <- c(20,24)
pchs[rmspe_data1[,1]!=1] <- 20
pchs[rmspe_data1[,1]==1] <- 24

png(paste0(dir$fig,"tests/","ratio_test_all_valid_utilities",
           format(Sys.time(),"%d_%b_%Y"),".png"), width = 300, height = 700)
dotchart(rmspe_data1[,2],
         labels=rmspe_data1[,3],
         main="All States",
         xlab="Post RMSPE/Pre RMSPE",
         ylab="",
         col=cols,
         pch=pchs,
         cex=0.9)
dev.off()