##/-------------------------------------------------------------------\##
## modeling flood damages based on a set of variables (features) and
# using different modeling frameworks ##
##/-------------------------------------------------------------------\##

##----------------------------------------------------------------
# TESTING/CREATING FLOOD MODELS #
##----------------------------------------------------------------

#------------------------------------------------
#--   EMDAT data: 1985 - 2021   -- #
#------------------------------------------------

rm(list=ls())

library(readxl)
library(reshape2)
library(psych) # for scatter smooth plots
library(caret) # for ML models
library(countrycode) # for converting country name to ISO
library(lubridate) # for converting dates
library(xgboost) # for xbgTree
library(plyr) # for xbgTree
library(e1071) # for support vector regression
library(kernlab) # for vector regression
library(brnn) # for bayesian neural network
library(stringr)

##/ directory
mainDir <- 'D:/PC_Cornell/Data/DFO_floods_damages'
setwd(mainDir)

# load in supporting functions (ML frameworks)
files.sources = list.files("./Programs/functions",full.names = TRUE)
my.functions <- sapply(files.sources, source)


start_time0 <- Sys.time()

##/ data
# Load EMDAT data (1985-2021)
emdat_vars = read.csv("./Data/EM_DAT_floods_1985_2021.csv", header = T)
# Load GDP information
world_gdp = read.csv("./Data/world_gdp.csv", header = T)

# Find fraction of damages data available per country
emdat_vars$Country_Re <- countrycode(emdat_vars$ISO, "iso3c", "country.name")

total_events_country <- table(emdat_vars$Country_Re)
events_country_counts <- cbind(names(total_events_country),as.numeric(total_events_country))

total_events_damages <- table(emdat_vars$Country_Re[!is.na(emdat_vars$Total.Damages...000.US..)])
damages_country_damages_counts <- cbind(names(total_events_damages),as.numeric(total_events_damages))

corres.events_country_counts <- events_country_counts[events_country_counts[,1]%in%damages_country_damages_counts[,1],]

frac.events_with.damages <- round(as.numeric(damages_country_damages_counts[,2])/as.numeric(corres.events_country_counts[,2]),2)
country_fraction_with_damages <- data.frame(cbind(corres.events_country_counts[,1],frac.events_with.damages))

all.available.damages <- data.frame(array(0,c(nrow(events_country_counts),2)))
all.available.damages[,1] <- events_country_counts[,1]
all.available.damages[events_country_counts[,1]%in%country_fraction_with_damages[,1],2]<- as.numeric(country_fraction_with_damages[,2])
all.available.damages$X3 <- as.numeric(events_country_counts[,2])
colnames(all.available.damages) <- c('region','frac_damages','total_events')
all.available.damages_EMDAT <- all.available.damages
# country names, fraction [0-1] with damages info, total #floods

# Get GDP for a certain year (e.g., Year 2022)
countrycode(world_gdp$Country.Code, "iso3c", "country.name")
x <- world_gdp$X2020
gdp_2020.country <- data.frame(countrycode(world_gdp$Country.Code, "iso3c", "country.name"),
                               log10(x))
gdp_2020.country <- gdp_2020.country[apply(1*is.na(gdp_2020.country),1,sum)==0,]
colnames(gdp_2020.country) <- c('region','currentUSD')

# Find GDP for each country and year
nevents = dim(emdat_vars)[1]
emdat_vars['gdp'] = NA # Create GDP column
emdat_vars['iso'] = NA # Create ISO column
for (i in 1:nevents){
  
  iso = emdat_vars$ISO[i]
  yr = emdat_vars$Year[i]
  ann_gdp = world_gdp[which(world_gdp$Country.Code == iso), yr - 1960 + 5] # GDP data starts from 1960
  
  # If no data found, assign NA
  if (length(ann_gdp) < 1){
    ann_gdp <- iso <- NA
  }
  
  emdat_vars['gdp'][i,1] = ann_gdp # Assign GDP
  emdat_vars['iso'][i,1] = iso # Assign ISO
}
# Add Duration for each event based on the start and end dates
start_dates <- as.Date(paste(emdat_vars$Start.Year,emdat_vars$Start.Month,emdat_vars$Start.Day,sep='-'))
end_dates <- as.Date(paste(emdat_vars$End.Year,emdat_vars$End.Month,emdat_vars$End.Day,sep='-'))
duration_events <- as.numeric(end_dates-start_dates+1) # in days
emdat_vars['Duration'] = duration_events # Duration column

# available damage data across all countries
sum(!is.na(emdat_vars$Total.Damages...000.US..))/sum(is.na(emdat_vars$Total.Damages...000.US..))*100
# 48.99722%

# Create dataframe:
# damage
# duration
# deaths
# area
# gdp
# iso
dat_full = data.frame(damage = 1000*emdat_vars$Total.Damages...000.US.., # in USD at the time of event (raw data comes in 1000 USD)
                      duration = emdat_vars$Duration,  # days
                      deaths = emdat_vars$Total.Deaths, 
                      area = emdat_vars$Dis.Mag.Value, # in km2
                      gdp = emdat_vars$gdp,
                      iso = emdat_vars$iso)
idx.holder <- seq(1,nrow(dat_full))
#> 4829 events
# Remove events with any missing data
dummy = apply(dat_full[,-ncol(dat_full)],1,mean,na.rm=F)
df = dat_full[which(!is.na(dummy)),]
real_nevents = dim(df)[1] # Number of events
idx.holder <- idx.holder[which(!is.na(dummy))]
# == 653 events

set.seed(1)
shuffled_indices <- sample(1:real_nevents)
# Apply the shuffled order to the data (randomizing)
df_shuffled <- df[shuffled_indices, ]

# Convert data into log scale
df_shuffled.log <- log(df_shuffled[,-ncol(df_shuffled)]) # remove ISO for country

# # log(damages/gdp)
df_shuffled.log$damage <- log(df_shuffled$damage/df_shuffled$gdp)
df_shuffled.log$gdp <- NULL # remove gdp



### -------------------------------------------------------------
##--- Training/Validating Process ---##

# based on the leave-1-out (for PRESS statistic) #
num.ens_press <- nrow(df_shuffled.log) # leave-1-out procedure for PRESS: PRediction Error Sum of Squares (PRESS statistic)
##-------------------------------------#
train_index <- array(NA,c((num.ens_press-1),num.ens_press))
for(j in 1:num.ens_press){
  train_index[,j] <- c(1:num.ens_press)[-j]
}
df_log.train_press = lapply(1:num.ens_press,function(x){df_shuffled.log[train_index[,x],]}) # Training set: all events except one of them
df_log.test_press = lapply(1:num.ens_press,function(x){df_shuffled.log[-train_index[,x],]}) # Testing set: all other events

# based on the group(country) k-fold #
num.ens_kgroup <- length(unique(df_shuffled$iso)) # 99 ## countries exist with data to work on: ranging from 1 to 86 events per country
##-------------------------------------#
train_index <- list()
countries_vec <- unique(df_shuffled$iso)
for(j in 1:num.ens_kgroup){
  train_index[[j]] <- which(df_shuffled$iso%in%countries_vec[j])
}
df_log.train_kgroup = lapply(1:num.ens_kgroup,function(x){df_shuffled.log[-train_index[[x]],]}) # Training set: all countries except one of them
df_log.test_kgroup = lapply(1:num.ens_kgroup,function(x){df_shuffled.log[train_index[[x]],]}) # Testing set: all other countries



### -------------------------------------------------------------
### -------------------------------------------------------------
### -------------------------------------------------------------
### -------------------------------------------------------------


### -------------------------------------------------------------
##--- Run LM ---##
lst.lm.stats <- list()

# leave-1-out (PRESS) #
outofsample_test.predict <- array(NA,c(num.ens_press,2))
insample_train.predict <- array(NA,c(num.ens_press,(num.ens_press-1),2))
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_press){
  print(s)
  out <- linear_model_fit(training.data=df_log.train_press[[s]],
                          testing.data=df_log.test_press[[s]])
  outofsample_test.predict[s,] <- cbind(df_log.test_press[[s]]$damage,
                                        out$out.of.sample_estimate)
  insample_train.predict[s,,] <- cbind(df_log.train_press[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.lm.stats[[1]] <- list(outofsample_test.predict,
                          insample_train.predict,
                          param)
press.lm.stats <- sum((lst.lm.stats[[1]][[1]][,1]-lst.lm.stats[[1]][[1]][,2])^2)

# group k-fold #
outofsample_test.predict <- list()
insample_train.predict <- list()
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_kgroup){
  print(s)
  out <- linear_model_fit(training.data=df_log.train_kgroup[[s]],
                          testing.data=df_log.test_kgroup[[s]])
  outofsample_test.predict[[s]] <- cbind(df_log.test_kgroup[[s]]$damage,
                                         out$out.of.sample_estimate)
  insample_train.predict[[s]] <- cbind(df_log.train_kgroup[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.lm.stats[[2]] <- list(outofsample_test.predict,
                          insample_train.predict,
                          param)



### -------------------------------------------------------------
##--- Run RF ---##
lst.rf.stats <- list()

# leave-1-out (PRESS) #
outofsample_test.predict <- array(NA,c(num.ens_press,2))
insample_train.predict <- array(NA,c(num.ens_press,(num.ens_press-1),2))
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_press){
  print(s)
  out <- rf_model_fit(training.data=df_log.train_press[[s]],
                      testing.data=df_log.test_press[[s]])
  outofsample_test.predict[s,] <- cbind(df_log.test_press[[s]]$damage,
                                        out$out.of.sample_estimate)
  insample_train.predict[s,,] <- cbind(df_log.train_press[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.rf.stats[[1]] <- list(outofsample_test.predict,
                          insample_train.predict,
                          param)
press.rf.stats <- sum((lst.rf.stats[[1]][[1]][,1]-lst.rf.stats[[1]][[1]][,2])^2)

# group k-fold #
outofsample_test.predict <- list()
insample_train.predict <- list()
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_kgroup){
  print(s)
  out <- rf_model_fit(training.data=df_log.train_kgroup[[s]],
                      testing.data=df_log.test_kgroup[[s]])
  outofsample_test.predict[[s]] <- cbind(df_log.test_kgroup[[s]]$damage,
                                         out$out.of.sample_estimate)
  insample_train.predict[[s]] <- cbind(df_log.train_kgroup[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.rf.stats[[2]] <- list(outofsample_test.predict,
                          insample_train.predict,
                          param)



### -------------------------------------------------------------
##--- Run XG Boost ---##
lst.xgb.stats <- list()

# leave-1-out (PRESS) #
outofsample_test.predict <- array(NA,c(num.ens_press,2))
insample_train.predict <- array(NA,c(num.ens_press,(num.ens_press-1),2))
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_press){
  print(s)
  out <- xgb_model_fit(training.data=df_log.train_press[[s]],
                       testing.data=df_log.test_press[[s]])
  outofsample_test.predict[s,] <- cbind(df_log.test_press[[s]]$damage,
                                        out$out.of.sample_estimate)
  insample_train.predict[s,,] <- cbind(df_log.train_press[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.xgb.stats[[1]] <- list(outofsample_test.predict,
                           insample_train.predict,
                           param)
press.xgb.stats <- sum((lst.xgb.stats[[1]][[1]][,1]-lst.xgb.stats[[1]][[1]][,2])^2)

# group k-fold #
outofsample_test.predict <- list()
insample_train.predict <- list()
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_kgroup){
  print(s)
  out <- xgb_model_fit(training.data=df_log.train_kgroup[[s]],
                       testing.data=df_log.test_kgroup[[s]])
  outofsample_test.predict[[s]] <- cbind(df_log.test_kgroup[[s]]$damage,
                                         out$out.of.sample_estimate)
  insample_train.predict[[s]] <- cbind(df_log.train_kgroup[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.xgb.stats[[2]] <- list(outofsample_test.predict,
                           insample_train.predict,
                           param)


### -------------------------------------------------------------
##--- Run SVR ---##
lst.svr.stats <- list()

# leave-1-out (PRESS) #
outofsample_test.predict <- array(NA,c(num.ens_press,2))
insample_train.predict <- array(NA,c(num.ens_press,(num.ens_press-1),2))
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_press){
  print(s)
  out <- svr_model_fit(training.data=df_log.train_press[[s]],
                       testing.data=df_log.test_press[[s]])
  outofsample_test.predict[s,] <- cbind(df_log.test_press[[s]]$damage,
                                        out$out.of.sample_estimate)
  insample_train.predict[s,,] <- cbind(df_log.train_press[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.svr.stats[[1]] <- list(outofsample_test.predict,
                           insample_train.predict,
                           param)
press.svr.stats <- sum((lst.svr.stats[[1]][[1]][,1]-lst.svr.stats[[1]][[1]][,2])^2)

# group k-fold #
outofsample_test.predict <- list()
insample_train.predict <- list()
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_kgroup){
  print(s)
  out <- svr_model_fit(training.data=df_log.train_kgroup[[s]],
                       testing.data=df_log.test_kgroup[[s]])
  outofsample_test.predict[[s]] <- cbind(df_log.test_kgroup[[s]]$damage,
                                         out$out.of.sample_estimate)
  insample_train.predict[[s]] <- cbind(df_log.train_kgroup[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.svr.stats[[2]] <- list(outofsample_test.predict,
                           insample_train.predict,
                           param)



### -------------------------------------------------------------
##--- Run BRNN ---##
lst.brnn.stats <- list()

# leave-1-out (PRESS) #
outofsample_test.predict <- array(NA,c(num.ens_press,2))
insample_train.predict <- array(NA,c(num.ens_press,(num.ens_press-1),2))
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_press){
  print(s)
  out <- brnn_model_fit(training.data=df_log.train_press[[s]],
                        testing.data=df_log.test_press[[s]])
  outofsample_test.predict[s,] <- cbind(df_log.test_press[[s]]$damage,
                                        out$out.of.sample_estimate)
  insample_train.predict[s,,] <- cbind(df_log.train_press[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.brnn.stats[[1]] <- list(outofsample_test.predict,
                            insample_train.predict,
                            param)
press.brnn.stats <- sum((lst.brnn.stats[[1]][[1]][,1]-lst.brnn.stats[[1]][[1]][,2])^2)

# group k-fold #
outofsample_test.predict <- list()
insample_train.predict <- list()
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_kgroup){
  print(s)
  out <- brnn_model_fit(training.data=df_log.train_kgroup[[s]],
                        testing.data=df_log.test_kgroup[[s]])
  outofsample_test.predict[[s]] <- cbind(df_log.test_kgroup[[s]]$damage,
                                         out$out.of.sample_estimate)
  insample_train.predict[[s]] <- cbind(df_log.train_kgroup[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.brnn.stats[[2]] <- list(outofsample_test.predict,
                            insample_train.predict,
                            param)




### -------------------------------------------------------------
##--- Run KNN ---##
lst.knn.stats <- list()

# leave-1-out (PRESS) #
outofsample_test.predict <- array(NA,c(num.ens_press,2))
insample_train.predict <- array(NA,c(num.ens_press,(num.ens_press-1),2))
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_press){
  print(s)
  out <- knn_model_fit(training.data=df_log.train_press[[s]],
                       testing.data=df_log.test_press[[s]])
  outofsample_test.predict[s,] <- cbind(df_log.test_press[[s]]$damage,
                                        out$out.of.sample_estimate)
  insample_train.predict[s,,] <- cbind(df_log.train_press[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.knn.stats[[1]] <- list(outofsample_test.predict,
                           insample_train.predict,
                           param)
press.knn.stats <- sum((lst.knn.stats[[1]][[1]][,1]-lst.knn.stats[[1]][[1]][,2])^2)

# group k-fold #
outofsample_test.predict <- list()
insample_train.predict <- list()
param <- list()
start_time <- Sys.time()
for(s in 1:num.ens_kgroup){
  print(s)
  out <- knn_model_fit(training.data=df_log.train_kgroup[[s]],
                       testing.data=df_log.test_kgroup[[s]])
  outofsample_test.predict[[s]] <- cbind(df_log.test_kgroup[[s]]$damage,
                                         out$out.of.sample_estimate)
  insample_train.predict[[s]] <- cbind(df_log.train_kgroup[[s]]$damage,
                                       out$in.sample_estimate)
  param[[s]] <- out$param
  
  rm(out)
}
end_time <- Sys.time(); run.time.total <- end_time - start_time
print(round(run.time.total,2))
lst.knn.stats[[2]] <- list(outofsample_test.predict,
                           insample_train.predict,
                           param)



### --------------------- save input data ----------------------
lst_EMDAT <- list(df_shuffled,df_shuffled.log)
names(lst_EMDAT) <- c('df_shuffled','df_shuffled.log')
saveRDS(lst_EMDAT,
        './Data/processed.data.files/lst_EMDAT.data_training.testing.rds')

### --------------------------save metrics-----------------------
### -------------------------------------------------------------

lst.EMDAT.evaluations <- list(lst.lm.stats,
                              lst.rf.stats,
                              lst.xgb.stats,
                              lst.svr.stats,
                              lst.brnn.stats,
                              lst.knn.stats)
names(lst.EMDAT.evaluations) <- c('lst.lm.stats','lst.rf.stats',
                                  'lst.xgb.stats','lst.svr.stats',
                                  'lst.brnn.stats','lst.knn.stats')
saveRDS(lst.EMDAT.evaluations,
        './Data/output.evaluations/lst.EMDAT.evaluations.rds')

end_time0 <- Sys.time(); run.time.total0 <- end_time0 - start_time0
print(start_time0);print(end_time0)
print(round(run.time.total0,2))
# done. #