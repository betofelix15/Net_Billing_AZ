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

###### estimation data ---------------------------------------------------------------
nm_all_res <- read.csv(paste0(dir$cleandata,'res_net_metering_wcov_2024_12_05_zeros_edited.csv'))

# add the cloud data from the utility data
utility_all <- read.csv(paste0(dir$cleandata,"utility_nm_full_covariates_10apr2025.csv"))

# drop the state_id 
utility_all <- utility_all[,-c(6)]

state_agg <- utility_all %>%
  group_by(year,month,state) %>%
  summarise(
    avg_cloudy_days = mean(cloudy_days, na.rm = T)
  )

# get DC info
dc <- utility_all[utility_all$state == "DC",]
dc <- dc[-c(4,9:11,14,16,17,21:23,25,26)]
dc <- rename(dc, "avg_cloudy_days" = "cloudy_days",
             "avg_temp_c" = "avg_temp",
             "cent_longitude" = "longitude",
             "cent_latitude" = "latitude",
             "aland" = "lnd_sqm",
             "per_capita_inc" = "med_inc")
# remake columns to match
dc$name <- "District of Columbia"


# add to nm_all_res
nm_all_res <- left_join(nm_all_res, state_agg, by = c("year","month","state"))

# turn temp to celcius
nm_all_res$avg_temp_f <- (nm_all_res$avg_temp_f - 32)*(5/9) 
nm_all_res <- rename(nm_all_res, "avg_temp_c" = "avg_temp_f")

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

# all states valid  -------------------------------------------------------

#select time of treatment
t0 <- 37
t1 <- 72 # last time ob

data <- nm_states

dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp_c",
               "avg_cloudy_days",
               "per_capita_inc",
               "usd_per_kwh",
               "aland",
               "cent_longitude",
               "cent_latitude"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="state_id",
  unit.names.variable="state",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:35),
  time.optimize.ssr=1:t0,
  time.plot=1:tn)

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
summary(gap[(t0+1):tn,1])

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
write.csv(sc_x_tab,paste0(dir$cleandata,"variable_table_synthetic_all_states_10apr2025.csv"), row.names = F)

# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)
# save dataframe
write.csv(sc_v_tab,paste0(dir$cleandata,"variable_table_synthetic_all_states_weights_10apr2025.csv"), row.names = F)

# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("State","State ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:34

# save dataframe
write.csv(sc_w_tab,paste0(dir$cleandata,"all_states_weight_table_10apr2025.csv"), row.names = F)


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
png(paste0(dir$fig,"treatment_synthetic/synthetic_all_10apr2025.png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")
dev.off()
# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_all_10apr2025.png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)
dev.off()

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(data$date)),
                    ncol=length(unique(data$state_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:t0]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0)) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$state_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(data$state_id)),
                       ratio_rmspe=rmspe_storage)


listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:20,]

# falsification test
for(i in 2:length(unique(data$state_id))){ # i <- 6 
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp_c",
                 "avg_cloudy_days",
                 "per_capita_inc",
                 "usd_per_kwh",
                 "aland",
                 "cent_longitude",
                 "cent_latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="state_id",
    unit.names.variable="state",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier= unique(
      data[data$state_id != i,]$state_id),
    time.optimize.ssr=1:t0,
    time.plot=1:tn)
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
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
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0))/sqrt(sum(gap[1:t0]^2)/(t0-1))
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
write.csv(storage_gap1,paste0(dir$cleandata,"storage_gap_35_states_11apr2025.csv"), row.names = F)

# PLOT
png(paste0(dir$fig,"tests/all_gap_test_apr2025.png"), width = 600, height = 500)
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
legend('bottomleft', c("AZ","Control States"),
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
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"rmspe_data_ordered_all_states.csv"), row.names = F)

# PLOT
#add state abbreviation
state_abb <- as.data.frame(cbind(unique(data$state),unique(data$state_id)))
colnames(state_abb) <- c("state_abb","state_i")
state_abb$state_i <- as.numeric(state_abb$state_i)
rmspe_data1<- left_join(rmspe_data_ordered,state_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
pchs <- c(20,24)
pchs[rmspe_data1[,1]!=1] <- 20
pchs[rmspe_data1[,1]==1] <- 24

png(paste0(dir$fig,"tests/","all_ratio_test.png"), width = 300, height = 700)
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
#####
# best 20 states (without smallest covariates) -------------------------------------------------------
listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:20,]

data <- rbind(nm_states[nm_states$state_id ==1,],
              nm_states[nm_states$state_id %in% listw$`State ID`,])

# make a new id for each
data <- arrange(data,state_id,date)

# assign the new ID's
data$state_id <- rep(1:length(unique(data$state_id)), each = tn)

# prep data
dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp_c",
               "avg_cloudy_days",
               "per_capita_inc",
               "usd_per_kwh",
               "aland",
               "cent_longitude",
               "cent_latitude"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="state_id",
  unit.names.variable="state",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:21),
  time.optimize.ssr=1:t0,
  time.plot=1:tn)

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
summary(gap[(t0+1):tn,1])

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
write.csv(sc_x_tab,paste0(dir$cleandata,"variable_table_synthetic_top20_states_",format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)
# save dataframe
write.csv(sc_v_tab,paste0(dir$cleandata,"variable_table_synthetic_top20_states_weights_",format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("State","State ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:length(sc_w_tab$`State ID`)

# save dataframe
write.csv(sc_w_tab,paste0(dir$cleandata,"top20_states_weight_table_",format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)


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
png(paste0(dir$fig,"treatment_synthetic/synthetic_top20_",format(Sys.time(),"%d_%b_%Y"),".png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col="red")
dev.off()
# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_top20_",format(Sys.time(),"%d_%b_%Y"),".png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "State Level Estimation",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2014 to Dec 2019)")
abline(v=t0, col= "red", lty=1)
dev.off()

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(data$date)),
                    ncol=length(unique(data$state_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:t0]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0)) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$state_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(data$state_id)),
                       ratio_rmspe=rmspe_storage)


listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:20,]

# falsification test
for(i in 2:length(unique(data$state_id))){ # i <- 6 
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp_c",
                 "avg_cloudy_days",
                 "per_capita_inc",
                 "usd_per_kwh",
                 "aland",
                 "cent_longitude",
                 "cent_latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="state_id",
    unit.names.variable="state",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier= unique(
      data[data$state_id != i,]$state_id),
    time.optimize.ssr=1:t0,
    time.plot=1:tn)
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
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
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):tn]^2)/(tn-t0))/sqrt(sum(gap[1:t0]^2)/(t0-1))
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
write.csv(storage_gap1,paste0(dir$cleandata,"storage_gap_top20_states_",format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# PLOT
png(paste0(dir$fig,"tests/top20_gap_test",format(Sys.time(),"%d_%b_%Y"),".png"), width = 600, height = 500)
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     ylim = c(-140,170),
     col= "gray",
     ylab="Difference in kWh per cust.",
     xlab="Falsification Test",
     main="20 States")

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
dev.off()

# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# save the data
write.csv(rmspe_data_ordered,paste0(dir$cleandata,"rmspe_data_ordered_top20_states_",format(Sys.time(),"%d_%b_%Y"),".csv"), row.names = F)

# PLOT
#add state abbreviation
state_abb <- as.data.frame(cbind(unique(data$state),unique(data$state_id)))
colnames(state_abb) <- c("state_abb","state_i")
state_abb$state_i <- as.numeric(state_abb$state_i)
rmspe_data1<- left_join(rmspe_data_ordered,state_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
pchs <- c(20,24)
pchs[rmspe_data1[,1]!=1] <- 20
pchs[rmspe_data1[,1]==1] <- 24

png(paste0(dir$fig,"tests/","ratio_testtop20_states_",format(Sys.time(),"%d_%b_%Y"),".png"), width = 300, height = 700)
dotchart(rmspe_data1[,2],
         labels=rmspe_data1[,3],
         main="20 States",
         xlab="Post RMSPE/Pre RMSPE",
         ylab="",
         col=cols,
         pch=pchs,
         cex=0.9)
dev.off()




# all states  without Hawaii and Alaska, top 20 (without smallest covariates) -------------------------------------------------------
listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:20,]

data <- rbind(nm_all_res_cont[nm_all_res_cont$state_id ==1,],
              nm_all_res_cont[nm_all_res_cont$state_id %in% listw$`State ID`,])

# make a new id for each
data <- arrange(data,state_id,date)

# assign the new ID's
data$state_id <- rep(1:length(unique(data$state_id)), each = tn)

# prep data
dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp_f",
               "precipitation_in",
               "per_capita_inc",
               "cent_kwh",
               "aland",
               "awater",
               "cent_longitude",
               "cent_latitude"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="state_id",
  unit.names.variable="state",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:length(unique(data$state_id))),
  time.optimize.ssr=1:t0,
  time.plot=1:tn)

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
summary(gap[(t0+1):tn,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred
sc_x_tab <- as.data.frame(synth.tables$tab.pred)
sc_x_tab$Variable <- c("Avg. Temp","Per Capita Income","Annual GDP","Cents per kWh",
                       "M Sq. Land","m. Sq. water","central longitude","central latittude")
# save dataframe
write.csv(sc_x_tab,paste0(dir$cleandata,"top20_variable_table.csv"), row.names = F)

# sc_x_tab <- sc_x_tab[,c(4,1:3)]
# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)

# save dataframe
write.csv(sc_v_tab,paste0(dir$cleandata,"top20_v_weight_table.csv"), row.names = F)

# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("State","State ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:length(sc_w_tab$`State ID`)

# save dataframe
write.csv(sc_w_tab,paste0(dir$cleandata,"top20_states_table.csv"), row.names = F)

# g1 <- sc_w_tab[c(1:10),c(1,3)]
# g2 <- sc_w_tab[c(11:20),c(1,3)]
# 
# 
# tab <- as.data.frame(cbind(g1,g2,g3))
# colnames(tab) <- c("a","b","c","d")
# 
# flextable(tab) %>%
#   align(align = "center", part = "all") %>%
#   set_header_labels(top = F,values = rep(c("State","W"),2))

date_az <- data[data$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
png(paste0(dir$fig,"treatment_synthetic/synthetic_20.png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "Top 20",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")
abline(v=t0, col="red")
dev.off()
# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_20.png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "Top 20",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")
abline(v=t0, col= "red", lty=1)
dev.off()

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(data$date)),
                    ncol=length(unique(data$state_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[(1:t0)]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/tn) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$state_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(data$state_id)),
                       ratio_rmspe=rmspe_storage)

#falsification test
for(i in 2:length(unique(data$state_id))){ # i <- 266 , is giving a hard time
  if(i == 26){ #  has issues for some reason
    next
  }
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp_f",
                 "precipitation_in",
                 "per_capita_inc",
                 "cent_kwh",
                 "aland",
                 "awater",
                 "cent_longitude",
                 "cent_latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="state_id",
    unit.names.variable="state",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier=unique(data$state_id[data$state_id != i]), # choose all but i
    time.optimize.ssr=1:t0,
    time.plot=1:tn)
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   genoud=FALSE)
  
  # # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  # path.plot(synth.res=synth.out,
  #           dataprep.res=dataprep.out,
  #           tr.intake = t0,
  #           Main = "Placebo: Real and Synthetic",
  #           Ylab= "kWh Per Customer",
  #           Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:t0]^2)/(t0-1))<2*sqrt(sum(storage_gap[1:t0,1]^2)/(t0-1))){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):tn]^2)/tn)/sqrt(sum(gap[1:t0]^2)/(t0-1))
  }
  print(i)
}
system("say Simulation is complete!")

storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- data[data$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())
# save data
write.csv(storage_gap1, paste0(dir$cleandata, "top20_storage_gap.csv"), row.names = F)

# PLOT gaps
png(paste0(dir$fig,"tests/20_gap_test.png"), width = 600, height = 500)
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     col= "gray",
     ylim = c(-140,180),
     ylab="Difference in NMPC (kWh)",
     xlab="Date",
     main="Top 20 (Continental)")

for(i in 4:length(storage_gap1)){
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
dev.off()

# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# save data
write.csv(rmspe_data_ordered, paste0(dir$cleandata, "top20_rmspe_data_ordered.csv"), row.names = F)


# PLOT
#add state abbreviation
state_abb <- as.data.frame(cbind(unique(data$state),unique(data$state_id)))
colnames(state_abb) <- c("state_abb","state_i")
state_abb$state_i <- as.numeric(state_abb$state_i)
rmspe_data1<- left_join(rmspe_data_ordered,state_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
pchs <- c(20,24)
pchs[rmspe_data1[,1]!=1] <- 20
pchs[rmspe_data1[,1]==1] <- 24

png(paste0(dir$fig,"tests/","20_ratio_test.png"), width = 300, height = 700)
dotchart(rmspe_data1[,2],
         labels=rmspe_data1[,3],
         main="Top 20",
         xlab="Post RMSPE/Pre RMSPE",
         ylab="",
         col=cols,
         pch=pchs,
         cex=0.9)
dev.off()


# all states  without Hawaii and Alaska, top 10 (without smallest covariates) -------------------------------------------------------
listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:10,]

data <- rbind(nm_all_res_cont[nm_all_res_cont$state_id ==1,],
              nm_all_res_cont[nm_all_res_cont$state_id %in% listw$`State ID`,])

# make a new id for each
data <- arrange(data,state_id,date)

# assign the new ID's
data$state_id <- rep(1:length(unique(data$state_id)), each = tn)

# prep data
dataprep.out<-dataprep(
  foo=data,
  predictors=c("avg_temp_f",
               "precipitation_in",
               "per_capita_inc",
               "cent_kwh",
               "aland",
               "awater",
               "cent_longitude",
               "cent_latitude"),
  predictors.op="mean",
  time.predictors.prior=1:t0,
  dependent="nm_per_cust",
  unit.variable="state_id",
  unit.names.variable="state",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:length(unique(data$state_id))),
  time.optimize.ssr=1:t0,
  time.plot=1:tn)

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
summary(gap[(t0+1):tn,1])

# TABLES OF RESULTS
synth.tables<-synth.tab(dataprep.res=dataprep.out,
                        synth.res=synth.out)
names(synth.tables)
# TABLE 1: COMPARISON OF PRE-TREATMENT VALUES OF TREATMENT AND SYNTHETIC CONTROL UNITS
synth.tables$tab.pred
sc_x_tab <- as.data.frame(synth.tables$tab.pred)
sc_x_tab$Variable <- c("Avg. Temp","Per Capita Income","Annual GDP","Cents per kWh",
                       "M Sq. Land","m. Sq. water","central longitude","central latittude")
sc_x_tab <- sc_x_tab[,c(4,1:3)]

# save the data
write.csv(sc_x_tab, paste0(dir$cleandata,"top10_variable_table.csv"), row.names = F)

# flextable(sc_x_tab) %>%
#   width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)

# save the data
write.csv(sc_v_tab, paste0(dir$cleandata,"top10_v_weight_table.csv"), row.names = F)
# flextable(sc_v_tab) %>%
#   width(j = 1, width = 1, unit = "in") %>%
#   align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("State","State ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:length(sc_w_tab$`State ID`)

# save the data
write.csv(sc_w_tab, paste0(dir$cleandata,"top10_states_weight_table.csv"), row.names = F)

# g1 <- sc_w_tab[c(1:10),c(1,3)]
# 
# tab <- as.data.frame(cbind(g1,g2,g3))
# colnames(tab) <- c("a","b")
# 
# flextable(tab) %>%
#   align(align = "center", part = "all") %>%
#   set_header_labels(top = F,values = rep(c("State","W"),1))

date_az <- data[data$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
png(paste0(dir$fig,"treatment_synthetic/synthetic_10.png"), height = 300, width= 500)
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = t0,
          Main = "Top 10",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")
abline(v=t0, col="red")
dev.off()
# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
png(paste0(dir$fig,"treatment_synthetic/gap_10.png"),  height = 300, width= 500)
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "Top 10",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")
abline(v=t0, col= "red", lty=1)
dev.off()
# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(data$date)),
                    ncol=length(unique(data$state_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:t0]^2)/(t0-1) #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[(1:t0)]^2)/(t0-1)) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[(t0+1):tn]^2)/tn) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$state_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(data$state_id)),
                       ratio_rmspe=rmspe_storage)

#falsification test
for(i in 2:length(unique(data$state_id))){ # i <- 266 , is giving a hard time
  if(i == 26){ #  has issues for some reason
    next
  }
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=data,
    predictors=c("avg_temp_f",
                 "precipitation_in",
                 "per_capita_inc",
                 "cent_kwh",
                 "aland",
                 "awater",
                 "cent_longitude",
                 "cent_latitude"),
    predictors.op="mean",
    time.predictors.prior=1:t0,
    dependent="nm_per_cust",
    unit.variable="state_id",
    unit.names.variable="state",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier=unique(data$state_id[data$state_id != i]), # choose all but i
    time.optimize.ssr=1:t0,
    time.plot=1:tn)
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   genoud=FALSE)
  
  # # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  # path.plot(synth.res=synth.out,
  #           dataprep.res=dataprep.out,
  #           tr.intake = t0,
  #           Main = "Placebo: Real and Synthetic",
  #           Ylab= "kWh Per Customer",
  #           Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:t0]^2)/(t0-1))<2*sqrt(sum(storage_gap[1:t0,1]^2)/(t0-1))){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[(t0+1):tn]^2)/tn)/sqrt(sum(gap[1:t0]^2)/(t0-1))
  }
  print(i)
}
system("say Simulation is complete!")

storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])

storage_gap1$date <- data[data$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())
# save data
write.csv(storage_gap1, paste0(dir$cleandata, "top10_storage_gap.csv"), row.names = F)

# PLOT gaps
png(paste0(dir$fig,"tests/10_gap_test.png"), width = 600, height = 500)
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     col= "gray",
     ylim = c(-140,180),
     ylab="Difference in NMPC (kWh)",
     xlab="Date",
     main="Top 10 (Continental)")

for(i in 4:length(storage_gap1)){
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
dev.off()

# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]

# save data
write.csv(rmspe_data_ordered, paste0(dir$cleandata, "top10_rmspe_data_ordered.csv"), row.names = F)


# PLOT
#add state abbreviation
state_abb <- as.data.frame(cbind(unique(data$state),unique(data$state_id)))
colnames(state_abb) <- c("state_abb","state_i")
state_abb$state_i <- as.numeric(state_abb$state_i)
rmspe_data1<- left_join(rmspe_data_ordered,state_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
pchs <- c(20,24)
pchs[rmspe_data1[,1]!=1] <- 20
pchs[rmspe_data1[,1]==1] <- 24

png(paste0(dir$fig,"tests/","10_ratio_test.png"), width = 300, height = 700)
dotchart(rmspe_data1[,2],
         labels=rmspe_data1[,3],
         main="Top 10",
         xlab="Post RMSPE/Pre RMSPE",
         ylab="",
         col=cols,
         pch=pchs,
         cex=0.9)
dev.off()








##### presenting all 4 together ----------------------------------------




synt_all <- readPNG(paste0(dir$fig,"treatment_synthetic/synthetic_all.png"))
synt_30 <- readPNG(paste0(dir$fig,"treatment_synthetic/synthetic_30.png"))
synt_20 <- readPNG(paste0(dir$fig,"treatment_synthetic/synthetic_20.png"))
synt_10 <- readPNG(paste0(dir$fig,"treatment_synthetic/synthetic_10.png"))

