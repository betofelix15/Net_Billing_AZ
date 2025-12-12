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
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
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

##### import data ----------------------------------------------------------------------

# import the full dataset
nm_all <- read.csv(paste0(dir$cleandata,"res_net_metering_wcov_2024_12_05_zeros_edited.csv"))
nm_all_res <- read.csv(paste0(dir$cleandata,"res_net_metering_wcov_2024_12_03.csv"))



# define the date variable
nm_all$date <- as.Date(nm_all$date)
nm_all_res$date <- as.Date(nm_all_res$date)



##### Data viz ----------------------------------------------------


# plot the az
nm_az <- nm_all[nm_all$state == "AZ" & nm_all$year > 2013,]
nm_az_res <- nm_all_res[nm_all_res$state == "AZ" & nm_all_res$year > 2013,]

# plot(nm_az$date,nm_az$nm_per_cust,
#      type = "l",
#      main = "Solar Net Metering: AZ",
#      xlab = "Date",
#      ylab = "MWH per Customer",
#      col = c("blue","lightblue"))
# lines(nm_az_res$date,nm_az_res$nm_per_cust,
#      col = "lightblue")
# legend("topright",
#         c("All customers","Residential"),
#  col=c("blue","lightblue"),
#  lty = c(1,1),
#  bty="n")


# plot for all except AZ
sum_all_other <- nm_all %>%
  filter(year > 2013 & state != "AZ") %>%
  group_by(date) %>%
  summarize(avg_nm_kwh = mean(nm_per_cust))

sum_res_other <- nm_all_res %>%
  filter(year > 2013 & state != "AZ") %>%
  group_by(date) %>%
  summarize(avg_nm_kwh = mean(nm_per_cust))


plot(sum_all_other$date,sum_all_other$avg_nm_kwh,
     type = "l",
     main = "Solar Net Metering",
     xlab = "Year",
     ylab = "KWH per Customer",
     ylim = c(20,250),
     col = c("blue","gold3"))
lines(nm_az$date,nm_az$nm_per_cust,
      col = "gold3",
      lty = 2,
      lwd = 2)
legend("topleft",
       c("Nat. Avg without AZ","AZ"),
       col=c("blue","gold3"),
       lty = c(1,2),
       lwd = c(1,2),
       bty="n")


plot(sum_all_other$date,
     log(sum_all_other$avg_nm_kwh),
     type = "l",
     main = "Solar Net Metering",
     xlab = "Date",
     ylab = "Log KWH per Customer",
     col = c("blue","gold3"))
lines(nm_az$date,
      log(nm_az$nm_per_cust),
      col = "gold3",
      lty = 2,
      lwd = 2)
legend("bottom",
       c("Nat. Avg","AZ"),
       col=c("blue","gold3"),
       lty = c(1,2),
       lwd = c(1,2),
       bty="n")


### pairs plot for covariates





# with chunk for rmarkdown
# ```{r chart, echo=FALSE, fig.cap="Monthly energy sold back", message=FALSE, warning=FALSE, out.height= 5,}
# # plot the az
# nm_az_res <- nm_all_res[nm_all_res$state == "AZ",]
# 
# # plot for all except AZ
# sum_res_other <- nm_all_res %>%
#   group_by(date) %>%
#   summarize(avg_nm_kwh = mean(nm_per_cust))
# 
# 
# plot(sum_res_other$date,sum_res_other$avg_nm_kwh,
#      type = "l",
#      main = "Residential Solar Net Metering",
#      xlab = "Date",
#      ylab = "NMPC (kWh)",
#      ylim = c(0,400),
#      lwd = 2,
#      col = c("blue","gold4"))
# lines(nm_az_res$date,nm_az_res$nm_per_cust,
#       col = "gold4",
#       lwd = 2,
#       lty = 4)
# legend("topright",
#        c("Nat. Avg","AZ"),
#        col=c("blue","gold4"),
#        lty = c(1,4),
#        lwd = c(2,2),
#        bty="n")
# 
# ```



###### estimation ---------------------------------------------------------------
nm_all_res <- read.csv(paste0(dir$cleandata,'res_net_metering_wcov_2024_12_05_zeros_edited.csv'))

nm_all_res <- nm_all_res[-c(12)] # drop annual gdp
nm_all_res <- arrange(nm_all_res,state_id,date)
nm_all_res$date_numeric <- rep(c(1:144),50)
nm_all_res$cent_kwh <- as.numeric(nm_all_res$cent_kwh) 
nm_all_res_cont <- nm_all_res[!(nm_all_res$state %in% c("HI","AK")),]

# because the states are in order we can redo the state_ids for the continental
# AZ is already number 1
nm_all_res_cont <- arrange(nm_all_res_cont,state_id,date)

# assign the new ID's
nm_all_res_cont$state_id <- rep(1:48, each = 144)


# all states (one less covariate) -------------------------------------------------------


dataprep.out<-dataprep(
  foo=nm_all_res,
  predictors=c("avg_temp_f",
               "precipitation_in",
               "per_capita_inc",
               "cent_kwh",
               "aland",
               "awater",
               "cent_longitude",
               "cent_latitude"),
  predictors.op="mean",
  time.predictors.prior=1:72,
  dependent="nm_per_cust",
  unit.variable="state_id",
  unit.names.variable="state",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:50),
  time.optimize.ssr=1:72,
  time.plot=1:144)

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
summary(gap[73:144,1])

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
flextable(sc_x_tab) %>%
  width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)

flextable(sc_v_tab) %>%
  width(j = 1, width = 1, unit = "in") %>%
  align(i = 1, align = "center", part = "header")
  


# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("State","State ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:49

g1 <- sc_w_tab[c(1:10),c(1,3)]
g2 <- sc_w_tab[c(11:20),c(1,3)]
g3 <- sc_w_tab[c(21:30),c(1,3)]
g4 <- sc_w_tab[c(31:40),c(1,3)]
g5 <- sc_w_tab[c(41:47),c(1,3)]

tab <- as.data.frame(cbind(g1,g2,g3,g4,rbind(g5,c(NA,NA),c(NA,NA),c(NA,NA))))
colnames(tab) <- c("a","b","c","d","e","f","g","h","i","j")

flextable(tab) %>%
  align(align = "center", part = "all") %>%
  set_header_labels(top = F,values = rep(c("State","W"),5))

date_az <- nm_all_res[nm_all_res$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = 72,
          Main = "AZ: Real and Synthetic",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")
abline(v=72, col="red")

# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "AZ: Difference from Synthetic",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")
abline(v=72, col= "red", lty=1)

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(nm_all_res$date)),
                    ncol=length(unique(nm_all_res$state_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:72]^2)/71 #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:72]^2)/71) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[73:144]^2)/70) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(nm_all_res$state_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(nm_all_res$state_id)),
                       ratio_rmspe=rmspe_storage)

#falsification test
for(i in 2:length(unique(nm_all_res$state_id))){ # i <- 6 , CO (6) is giving a hard time
  if(i == 6){ # colorado has issues for some reason
    next
  }
  
  # DATA PREPERATION
  dataprep.out<-dataprep(
    foo=nm_all_res,
    predictors=c("avg_temp_f",
                 "precipitation_in",
                 "per_capita_inc",
                 "cent_kwh",
                 "aland",
                 "awater",
                 "cent_longitude",
                 "cent_latitude"),
    predictors.op="mean",
    time.predictors.prior=1:72,
    dependent="nm_per_cust",
    unit.variable="state_id",
    unit.names.variable="state",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier=unique(nm_all_res$state_id[nm_all_res$state_id != i]), # choose all but i
    time.optimize.ssr=1:72,
    time.plot=1:144)

  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   genoud=FALSE)
  
  # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  path.plot(synth.res=synth.out,
            dataprep.res=dataprep.out,
            tr.intake = 72,
            Main = "Placebo: Real and Synthetic",
            Ylab= "kWh Per Customer",
            Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:72]^2)/71)<2*sqrt(sum(storage_gap[1:72,1]^2)/71)){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[73:144]^2)/70)/sqrt(sum(gap[1:72]^2)/71)
  }
  print(i)
}
system("say Simulation is complete!")
  
storage_gap1<-as.data.frame(storage_gap[,colSums(is.na(storage_gap))<nrow(storage_gap)])
  
storage_gap1$date <- nm_all_res[nm_all_res$state_id == 1,]$date
storage_gap1$date <- as.Date(storage_gap1$date)
storage_gap1$synthetic <- 0

#put these up front 
storage_gap1 <- storage_gap1 %>% select(date,synthetic,everything())

# PLOT
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     col= "gray",
     ylab="Difference in NMPC (kWh)",
     xlab="Date",
     main="")

for(i in 5:length(storage_gap1)){
  lines(storage_gap1$date,storage_gap1[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap1$date,storage_gap1$V1, lwd=2,)
lines(storage_gap1$date,storage_gap1$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[72,]$date), col=1, lty=4, lwd= 2)

  
# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]


# PLOT
#add state abbreviation
state_abb <- as.data.frame(cbind(unique(nm_all_res$state),unique(nm_all_res$state_id)))
colnames(state_abb) <- c("state_abb","state_i")
state_abb$state_i <- as.numeric(state_abb$state_i)
rmspe_data1<- left_join(rmspe_data_ordered,state_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
dotchart(rmspe_data1[,2],
         labels=rmspe_data1[,3],
         main="",
         xlab=expression(atop("Post RMSPE/Pre RMSPE")),
         ylab="",
         col=cols,
         pch=20,
         cex=0.9)
  


# all states  without Hawaii and Alaska, top 30 -------------------------------------------------------
listw <- arrange(sc_w_tab,desc(Weight))
listw <- listw[1:30,]

data <- rbind(nm_all_res_cont[nm_all_res_cont$state_id ==1,],
              nm_all_res_cont[nm_all_res_cont$state_id %in% listw$`State ID`,])

# make a new id for each
data <- arrange(data,state_id,date)

# assign the new ID's
data$state_id <- rep(1:length(unique(data$state_id)), each = 144)

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
  time.predictors.prior=1:72,
  dependent="nm_per_cust",
  unit.variable="state_id",
  unit.names.variable="state",
  time.variable="date_numeric",
  treatment.identifier=1,
  controls.identifier=c(2:length(unique(data$state_id))),
  time.optimize.ssr=1:72,
  time.plot=1:144)

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
summary(gap[73:144,1])

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
flextable(sc_x_tab) %>%
  width(j = 1, width = 1, unit = "in")


# TABLE 2: V MATRIX
synth.tables$tab.v

sc_v_tab <- cbind(sc_x_tab,as.numeric(synth.tables$tab.v))
sc_v_tab <- rename(sc_v_tab, "V Weights" = 5)

flextable(sc_v_tab) %>%
  width(j = 1, width = 1, unit = "in") %>%
  align(i = 1, align = "center", part = "header")



# TABLE 3: W MATRIX
synth.tables$tab.w

sc_w_tab <- as.data.frame(synth.tables$tab.w)
sc_w_tab <- sc_w_tab[,c(2,3,1)]
colnames(sc_w_tab) <- c("State","State ID","Weight")

sc_w_tab <- arrange(sc_w_tab,desc(Weight))
row.names(sc_w_tab) <- 1:49

g1 <- sc_w_tab[c(1:10),c(1,3)]
g2 <- sc_w_tab[c(11:20),c(1,3)]
g3 <- sc_w_tab[c(21:30),c(1,3)]


tab <- as.data.frame(cbind(g1,g2,g3))
colnames(tab) <- c("a","b","c","d","e","f")

flextable(tab) %>%
  align(align = "center", part = "all") %>%
  set_header_labels(top = F,values = rep(c("State","W"),5))

date_az <- data[data$state == "AZ",]$date
# TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS
path.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          tr.intake = 72,
          Main = "AZ: Real and Synthetic",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")

# Monthly GAP BETWEEN TRAJECTORIES OF TREATMENT AND SYNTHETIC CONTROL UNITS- TREATMENT EFFECT
gaps.plot(synth.res=synth.out,
          dataprep.res=dataprep.out,
          Main = "AZ: Difference from Synthetic",
          Ylab= "kWh Per Customer",
          Xlab="Months (Jan 2011 to Dec 2022)")
abline(v=72, col= "red", lty=1)

# STORAGE FOR GAPS (TREATMENT EFFECTS)
storage_gap<-matrix(NA, nrow=length(unique(data$date)),
                    ncol=length(unique(data$state_id)))
storage_gap[,1]<-gap
print(storage_gap)
# RMSPE FOR PRE INTERVENTION
mspe_man <- sum(gap[1:72]^2)/71 #MSPE: manual calc (2011 to 2017)

mspe_syn <- synth.out$loss.v  #MSPE: reported by synth()

rmspe_pre <- sqrt(sum(gap[1:72]^2)/71) #RMSPE: pre-intervention period

rmspe_post <- sqrt(sum(gap[73:144]^2)/70) #RMSPE: post-intervention period

# POST RMSPE/PRE RMSPE
rmspe_storage<-c(rep(NA,length(unique(data$state_id))))
rmspe_storage[1]<-rmspe_post/rmspe_pre
rmspe_data<-data.frame(state_i=c(unique(data$state_id)),
                       ratio_rmspe=rmspe_storage)

#falsification test
for(i in 2:length(unique(data$state_id))){ # i <- 6 , CO (6) is giving a hard time
  if(i == 6){ # colorado has issues for some reason
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
    time.predictors.prior=1:72,
    dependent="nm_per_cust",
    unit.variable="state_id",
    unit.names.variable="state",
    time.variable="date_numeric",
    treatment.identifier=i,
    controls.identifier=unique(data$state_id[data$state_id != i]), # choose all but i
    time.optimize.ssr=1:72,
    time.plot=1:144)
  
  # OPTIMIZATION
  synth.out<-synth(data.prep.obj=dataprep.out,
                   method="BFGS",
                   genoud=FALSE)
  
  # TRAJECTORIES OF PLACEBO UNIT AND ITS SYNTHETIC CONTROL
  path.plot(synth.res=synth.out,
            dataprep.res=dataprep.out,
            tr.intake = 72,
            Main = "Placebo: Real and Synthetic",
            Ylab= "kWh Per Customer",
            Xlab="Months (Jan 2011 to Dec 2022)")
  gap<-dataprep.out$Y1plot-dataprep.out$Y0plot%*%synth.out$solution.w
  # INFERENCE REFINEMENT- 2*RMSPE OF TREATMENT IN PRE-TREATMENT PERIOD
  if(sqrt(sum(gap[1:72]^2)/71)<2*sqrt(sum(storage_gap[1:72,1]^2)/71)){
    # GAP (TREATMENT EFFECT)
    storage_gap[,i]<-gap
    # POST RMSPE/PRE RMSPE
    rmspe_data$ratio_rmspe[i]<-sqrt(sum(gap[73:144]^2)/70)/sqrt(sum(gap[1:72]^2)/71)
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

# PLOT
plot(storage_gap1$date,
     storage_gap1$V2,
     type = "l",
     col= "gray",
     ylab="Difference in NMPC (kWh)",
     xlab="Date",
     main="")

for(i in 5:length(storage_gap1)){
  lines(storage_gap1$date,storage_gap1[,i],
        lwd=1,
        col="grey")
}  
lines(storage_gap1$date,storage_gap1$V1, lwd=2,)
lines(storage_gap1$date,storage_gap1$synthetic,lty= 2)
abline(v=as.numeric(storage_gap1[72,]$date), col=1, lty=4, lwd= 2)


# PLOT ORDERED RATIO OF RMSPE
# REMOVE NA COLUMNS
rmspe_data<-na.omit(rmspe_data)
rmspe_data_ordered<-rmspe_data[order(rmspe_data[,2]),]


# PLOT
#add state abbreviation
state_abb <- as.data.frame(cbind(unique(data$state),unique(data$state_id)))
colnames(state_abb) <- c("state_abb","state_i")
state_abb$state_i <- as.numeric(state_abb$state_i)
rmspe_data1<- left_join(rmspe_data_ordered,state_abb)


cols<-c('black','blue')
cols[rmspe_data1[,1]!=1] <- 'black'
cols[rmspe_data1[,1]==1] <- 'blue'
dotchart(rmspe_data1[,2],
         labels=rmspe_data1[,3],
         main="",
         xlab=expression(atop("Post RMSPE/Pre RMSPE")),
         ylab="",
         col=cols,
         pch=20,
         cex=0.9)


