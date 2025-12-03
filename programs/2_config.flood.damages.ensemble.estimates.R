##/-------------------------------------------------------------------\##
## modeling flood damages based on a set of variables (features) and
# using different modeling frameworks ##
##/-------------------------------------------------------------------\##


##----------------------------------------------------------------
# ESTIMATING FLOOD DAMAGES #
##----------------------------------------------------------------


rm(list=ls())

library(readxl)
library(reshape2)
library(hydroGOF) # for GoF
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
rm(list = ls())
mainDir <- 'E:/PC_Cornell/Data/DFO_floods_damages'
setwd(mainDir)

# load in supporting functions
files.sources = list.files("./Programs/functions",full.names = TRUE)
my.functions <- sapply(files.sources, source)

# Load GDP information
world_gdp = read.csv("./Data/world_gdp.csv", header = T)


##- Load DFO'21 data (1985-2021) -##
##----------------------------------------------------------------
{  
# dfo21_vars = read_xlsx('./Data/FloodArchive_1985_2021.xlsx', col_names = T)
dfo21_vars = readRDS('./Data/processed.data.files/processed.plus.DFO_floods_1985_2021.rds') # there is a separate script to produce this
dfo21_vars$ISO <- array(NA,dim(dfo21_vars$Country))
dfo21_vars$GDP <- array(NA,dim(dfo21_vars$Country))
for(k in 1:dim(dfo21_vars$Country)[1]){
  for(j in 1:dim(dfo21_vars$Country)[2]){
    dfo21_vars$ISO[k,j,] <- countrycode(as.character(dfo21_vars$Country[k,j,]),
                                        origin = 'country.name', 
                                        destination = 'iso3c')
    
    iso <- unique(na.omit(dfo21_vars$ISO[k,j,]))
    if (length(iso)>0){
      yr = as.numeric(format(as.Date(dfo21_vars$Began[k,j,],origin = '1970-01-01'),'%Y'))
      yr <- yr[!is.na(yr)]
      ann_gdp = world_gdp[which(world_gdp$Country.Code == iso),
                          yr - 1960 + 5] # GDP data starts from 1960
      
      # If no data found, assign NA
      if (length(ann_gdp) < 1){
        ann_gdp = rep(NA,dim(dfo21_vars$Country)[3])
      }
      
      dfo21_vars$GDP[k,j,1:length(ann_gdp)] = as.numeric(ann_gdp) # Assign GDP
    }
  }
  print(k)
}

# Create dataframe:
# duration
# deaths
# area
# gdp
dat0 = data.frame(duration = as.numeric(as.matrix(dfo21_vars$Duration)), 
                 deaths = as.numeric(as.matrix(dfo21_vars$Dead)), 
                 area = as.numeric(as.matrix(dfo21_vars$Area)),
                 gdp = as.numeric(as.matrix(dfo21_vars$GDP)),
                 country = as.vector(as.matrix(dfo21_vars$Country)),# extras are below
                 lat = as.numeric(as.matrix(dfo21_vars$lat)),
                 lon = as.numeric(as.matrix(dfo21_vars$long)),
                 Began = as.numeric(as.matrix(dfo21_vars$Began)),
                 Ended = as.numeric(as.matrix(dfo21_vars$Ended)),
                 MainCause = as.vector(as.matrix(dfo21_vars$MainCause)))
dat <- dat0[,c(1,2,3,4)]# The first four: duration, deaths, area, gdp

dim(dat)[1]-sum(sapply(1:dim(dat)[1],function(x){return(sum(is.na(dat[x,])))})==dim(dat)[2])
#> 4741 events
# Remove events with any missing data
dummy = apply(dat, 1, mean) 
idx_holder <- !is.na(dummy)
df_dfo21 = dat[which(idx_holder),]

# Convert data into log scale

df_dfo21$deaths[df_dfo21$deaths==0] <- 1 # making 0 damages death to 1 to avoid Inf after getting its log
real_nevents = dim(df_dfo21)[1] # Number of events
#> 4589 events

df_dfo21_log <- log(df_dfo21) # *will use for estimating damages
rest.df <- dat0[which(idx_holder),]

}


##- Load EMDAT data (1985-2021) -##
##----------------------------------------------------------------
{
emdat_vars = read.csv("./Data/EM_DAT_floods_1985_2021.csv", header = T)
# Find GDP for each country and year
nevents = dim(emdat_vars)[1]
emdat_vars['gdp'] = NA # Create GDP column
for (i in 1:nevents){
  
  iso = emdat_vars$ISO[i]
  yr = emdat_vars$Year[i]
  ann_gdp = world_gdp[which(world_gdp$Country.Code == iso), yr - 1960 + 5] # GDP data starts from 1960
  
  # If no data found, assign NA
  if (length(ann_gdp) < 1){
    ann_gdp = NA
  }
  
  emdat_vars['gdp'][i,1] = ann_gdp # Assign GDP
}
# Add Duration for each event based on the start and end dates
start_dates <- as.Date(paste(emdat_vars$Start.Year,emdat_vars$Start.Month,emdat_vars$Start.Day,sep='-'))
end_dates <- as.Date(paste(emdat_vars$End.Year,emdat_vars$End.Month,emdat_vars$End.Day,sep='-'))
duration_events <- as.numeric(end_dates-start_dates+1) # in days
emdat_vars['Duration'] = duration_events # Duration column

# Create dataframe:
# damage
# duration
# deaths
# area
# gdp
dat = data.frame(damage = 1000*emdat_vars$Total.Damages...000.US.., # in USD (raw data is in 1000 USD)
                 duration = emdat_vars$Duration,  # days
                 deaths = emdat_vars$Total.Deaths, 
                 area = emdat_vars$Dis.Mag.Value, # in km2
                 gdp = emdat_vars$gdp)
idx.holder <- seq(1,nrow(dat))
#> 4829 events
# Remove events with any missing data
dummy = apply(dat, 1, mean)
df_emdat = dat[which(!is.na(dummy)),]

df_emdat_log <- log(df_emdat)
# # log(damages/gdp)
df_emdat_log$damage <- log(df_emdat$damage/df_emdat$gdp) # *will use it for training the models
df_emdat_log$gdp <- NULL
dim(df_emdat_log)
#> 653 events

# Create dataframe:
# duration
# deaths
# area
# gdp
dat = data.frame(duration = emdat_vars$Duration,  # days
                 deaths = emdat_vars$Total.Deaths, 
                 area = emdat_vars$Dis.Mag.Value, # in km2
                 gdp = emdat_vars$gdp)
#> 4829 events
# Remove events with any missing data
dummy = apply(dat, 1, mean)
idx_holder <- !is.na(dummy)
df_emdat_all = dat[which(!is.na(dummy)),]
df_emdat_all_log <- log(df_emdat_all) # *will use it for estimating damages (if needed)
dim(df_emdat_all_log)
#> 1367 events

dat0 = data.frame(duration = as.numeric(as.matrix(emdat_vars$Duration)),  # days
                  deaths = as.numeric(as.matrix(emdat_vars$Total.Deaths)), 
                  area = as.numeric(as.matrix(emdat_vars$Dis.Mag.Value)), # in km2
                  gdp = as.numeric(as.matrix(emdat_vars$gdp)),
                  country = as.vector(as.matrix(emdat_vars$Country)),# extras are below
                  lat = as.numeric(as.matrix(emdat_vars$Latitude)),
                  lon = as.numeric(as.matrix(emdat_vars$Longitude)),
                  Began = as.numeric(as.matrix(start_dates)),
                  Ended = as.numeric(as.matrix(end_dates)),
                  dismagvalue = as.vector(as.matrix(emdat_vars$Dis.Mag.Value)))
rest.emdat_all.df <- dat0[which(idx_holder),]
#> 1367 events

}


# Load DFO data (1985-2015)
##----------------------------------------------------------------
{
dir.to.file <- "./Data/DFO_floods_1985_2015.xlsx"
sheets_file <- excel_sheets(dir.to.file)
print(sheets_file)
filt.in <- c('numeric','numeric','numeric','numeric','numeric','numeric',
             'text','text','numeric','numeric','numeric')
dfo15_vars <- list()
for (k in 1:length(sheets_file)){
  dfo15_vars[[k]] <- read_xlsx(dir.to.file,
                               sheet = sheets_file[k],
                               col_names = FALSE,
                               col_types = filt.in[k])
}
names(dfo15_vars) <- sheets_file

# Find GDP for each country and year
max_nevents = dim(dfo15_vars$Countries)[1]
max_nyears = dim(dfo15_vars$Countries)[2]
dfo15_vars[['gdp']] = array(NA,c(max_nevents,max_nyears)) # Create GDP matrix
for (i in 1:max_nevents){
  for (j in 1:max_nyears){
    # English to ISO
    if (!is.na(dfo15_vars$Duration[i,j])){
      iso = countrycode(as.character(dfo15_vars$Countries[i,j]),
                        origin = 'country.name', 
                        destination = 'iso3c')
      yr = as.numeric(format(ymd(dfo15_vars$Start_Date[i,j]),'%Y'))
      ann_gdp = world_gdp[which(world_gdp$Country.Code == iso),
                          yr - 1960 + 5] # GDP data starts from 1960
      
      # If no data found, assign NA
      if (length(ann_gdp) < 1){
        ann_gdp = NA
      }
      
      dfo15_vars$gdp[i,j] = ann_gdp # Assign GDP
    }
  }
}

# Create dataframe:
# damage
# duration
# deaths
# area
# gdp
dat = data.frame(damage = as.numeric(as.matrix(dfo15_vars$Damages)),
                 duration = as.numeric(as.matrix(dfo15_vars$Duration)), 
                 deaths = as.numeric(as.matrix(dfo15_vars$Death)), 
                 area = as.numeric(as.matrix(dfo15_vars$AffectedAreaKm2)),
                 gdp = as.numeric(as.matrix(dfo15_vars$gdp)))
dim(dat)[1]-sum(sapply(1:dim(dat)[1],function(x){return(sum(is.na(dat[x,])))})==5)
#> 4311 events
# Remove events with any missing data
dummy = apply(dat, 1, mean)
df_dfo15 = dat[which(!is.na(dummy)),]
df_dfo15 <- df_dfo15 [!df_dfo15$damage==0,] # eliminating rows with zero damages
real_nevents = dim(df_dfo15)[1] # Number of events
#> 882 events
# Convert data into log scale
df_dfo15$deaths[df_dfo15$deaths==0] <- 1 # making 0 damages death to 1 to avoid Inf after getting its log

df_dfo15_log <- log(df_dfo15) # will use it for training the models
# # log(damages/gdp)
df_dfo15_log$damage <- log(df_dfo15$damage/df_dfo15$gdp) # *will use it for training the models
df_dfo15_log$gdp <- NULL
}


##----------------------------------------------------------------
# DFO'15+EM-DAT'21 combined #
df_emdat_dfo15 <- rbind(df_emdat,df_dfo15)

real_nevents <- nrow(df_emdat_dfo15)
#-- concatenate the two datasets --#
df <- df_emdat_dfo15
set.seed(1)
shuffled_indices <- sample(1:real_nevents)
# Apply the shuffled order to the data
df <- df[shuffled_indices, ]
df_emdat_dfo15_log <- log(df)
df_emdat_dfo15_log$damage <- log(df$damage/df$gdp)
df_emdat_dfo15_log$gdp <- NULL # remove gdp
##----------------------------------------------------------------



# modeling function with a list of machine learning models
estimate.machine_learning.models <- function(training.data,
                                           new.data,
                                           min_try){
  # Linear Models: Linear Regression
  # Tree-based Models: Random Forest Regression
  # Tree-based Models: Extreme Gradient Boosting (XGBoost)
  # Support Vector Machines: Support Vector Regression (SVR) (Support Vector Machines with Radial Basis Function Kernel)
  # Neural Networks: Bayesian Regularized Neural Networks (BRNN)
  # Non-parametric Regression: k-Nearest Neighbors (k-NN) Regression
  
  df_log.train <- training.data
  df_log.predict <- new.data
  country_gdp <- exp(df_log.predict$gdp)
  num.ens <- min_try
  
  
  print('-- LM Started --')
  start_time <- Sys.time()
  # lm: Linear Model <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # lm_fit = lm(damage ~ duration + deaths + area, data = df_log.train)
  lm_fit = lm(damage ~ ., data = df_log.train)
  lm.out <- predict(lm_fit,df_log.predict)

  end_time <- Sys.time(); run.time.total <- end_time - start_time
  print('-- LM Finished --');print(round(run.time.total,2))
  
  
  print('-- RF Started --')
  start_time <- Sys.time()
  # rf: Random Forest <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  rf.out <- array(NA,c(dim(df_log.predict)[1],num.ens))
  for (i in 1:num.ens){
    set.seed(i)
    rf_fit <- train(damage ~ ., data=df_log.train,
                    method='rf',
                    importance=TRUE,
                    trControl=trainControl(method='cv',number=10))
    
    rf.out[,i] <- predict(rf_fit,df_log.predict)
    print(i)
  }
  rf.out <- apply(rf.out,1,median)
  end_time <- Sys.time(); run.time.total <- end_time - start_time
  print('-- RF Finished --');print(round(run.time.total,2))
  
  
  print('-- XGBoost Started --')
  start_time <- Sys.time()
  # xgb: eXtreme Gradient Boosting <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  xgb.out <- array(NA,c(dim(df_log.predict)[1],num.ens))
  for (i in 1:num.ens){
    set.seed(i)
    xgb_fit <- train(damage ~ ., data=df_log.train,
                     method='xgbTree',
                     trControl=trainControl(method='cv',number=10))
    
    xgb.out[,i] <- predict(xgb_fit,df_log.predict)
    print(i)
  }
  xgb.out <- apply(xgb.out,1,median)
  end_time <- Sys.time(); run.time.total <- end_time - start_time
  print('-- XGBoost Finished --');print(round(run.time.total,2))
  
  
  print('-- SVR Started --')
  start_time <- Sys.time()
  # svr: Support Vector Regression <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  svr.out <- array(NA,c(dim(df_log.predict)[1],num.ens))
  for (i in 1:num.ens){
    set.seed(i)
    svr_fit <- train(damage ~ ., data=df_log.train,
                     method='svmRadial',
                     trControl=trainControl(method='cv',number=10))
    
    svr.out[,i] <- predict(svr_fit,df_log.predict)
    print(i)
  }
  svr.out <- apply(svr.out,1,median)
  end_time <- Sys.time(); run.time.total <- end_time - start_time
  print('-- SVR Finished --');print(round(run.time.total,2))
  
  
  print('-- BRNN Started --')
  start_time <- Sys.time()
  # brnn: Bayesian Regularized Neural Networks <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  brnn.out <- array(NA,c(dim(df_log.predict)[1],num.ens))
  for (i in 1:num.ens){
    set.seed(i)
    # from brnn package
    # mpg_brnn <- brnn(xtrain,ytrain,neurons=2,normalize = TRUE,epochs = 1000,Monte_Carlo = TRUE)
    # ypred_brnn <- predict.brnn(mpg_brnn,xtest)
    # or from caret package
    brnn_fit <- train(damage ~ ., data=df_log.train,
                      method='brnn',
                      trControl=trainControl(method='cv',number=10))
    
    brnn.out[,i] <- predict(brnn_fit,df_log.predict)
    print(i)
  }
  brnn.out <- apply(brnn.out,1,median)
  end_time <- Sys.time(); run.time.total <- end_time - start_time
  print('-- BRNN Finished --');print(round(run.time.total,2))
  
  
  print('-- K-NN Started --')
  start_time <- Sys.time()
  # knn: k-Nearest Neighbors <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  knn_fit <- train(damage ~ ., data=df_log.train,
                   method='knn',
                   trControl=trainControl(method='cv',number=10))
  knn.out <- predict(knn_fit,df_log.predict)
  end_time <- Sys.time(); run.time.total <- end_time - start_time
  print('-- K-NN Finished --');print(round(run.time.total,2))
  
  # convert log(damages/gdp) to exp(.)
  all.predict <- data.frame(exp(cbind(lm.out,
                                  rf.out,
                                  xgb.out,
                                  svr.out,
                                  brnn.out,
                                  knn.out)))
  
  # convert to true $ USD using GDP
  out <- all.predict*country_gdp
  
  colnames(out) <- c('lm_USD',
                     'rf_USD',
                     'xgb_USD',
                     'svr_USD',
                     'brnn_USD',
                     'knn_USD')
  return(out)
}


##------ DAMAGE ESTIMATES ------##
# estimating DFO'21 based on the EM-DAT training
dfo21_predict.damages_wt_EMDAT <- estimate.machine_learning.models(training.data=df_emdat_log,
                                                                       new.data=df_dfo21_log,
                                                                       min_try=10)


# estimating DFO'21 based on the DFO'15 training
dfo21_predict.damages_wt_dfo15 <- estimate.machine_learning.models(training.data=df_dfo15_log,
                                                                       new.data=df_dfo21_log,
                                                                       min_try=10)

# estimating DFO'21 based on the DFO'15+EM-DAT training
dfo21_predict.damages_wt_dfo15_emdat <- estimate.machine_learning.models(training.data=df_emdat_dfo15_log,
                                                                   new.data=df_dfo21_log,
                                                                   min_try=10)

# estimating EM-DAT (all events with attributes) based on the EM-DAT training
emdat_predict.damages_wt_emdat <- estimate.machine_learning.models(training.data=df_emdat_log,
                                                                         new.data=df_emdat_all_log,
                                                                         min_try=10)
##-----------------------------##

rest.df2 <- cbind(rest.df)
rest.df2$Began <- as.Date(rest.df2$Began,origin='1970-01-01')
rest.df2$Ended <- as.Date(rest.df2$Ended,origin='1970-01-01')

rest.emdat_all.df2 <- cbind(rest.emdat_all.df)
rest.emdat_all.df2$Began <- as.Date(rest.emdat_all.df2$Began,origin='1970-01-01')
rest.emdat_all.df2$Ended <- as.Date(rest.emdat_all.df2$Ended,origin='1970-01-01')

set1 <- cbind(rest.df2,round(dfo21_predict.damages_wt_EMDAT,-3)) # round off to represent in 1000's USD
set2 <- cbind(rest.df2,round(dfo21_predict.damages_wt_dfo15,-3))
set3 <- cbind(rest.df2,round(dfo21_predict.damages_wt_dfo15_emdat,-3))
set4 <- cbind(rest.emdat_all.df2,round(emdat_predict.damages_wt_emdat,-3))

order_idx.dates <- order(rest.df2$Began)
set1 <- set1[order_idx.dates,]
set2 <- set2[order_idx.dates,]
set3 <- set3[order_idx.dates,]

order_idx2.dates <- order(rest.emdat_all.df2$Began)
set4 <- set4[order_idx2.dates,]


df.outputs <- list(set1,set2,set3,set4)
names(df.outputs) <- c('DFO21_damages_wt_EMDAT21',
                       'DFO21_damages_wt_DFO15',
                       'DFO21_damages_wt_DFO15.EMDAT21',
                       'EMDAT21_damages_wt_EMDAT21')
# saved on May 07, 2025
saveRDS(df.outputs,
        './Data/output.data.files/ML.DFO21.EMDAT21.damages.predictions_20250507.rds')

# saving in CSV format with convenient dates format
df.outputs$DFO21_damages_wt_EMDAT21$Began <- format(df.outputs$DFO21_damages_wt_EMDAT21$Began,
                                                    '%Y/%m/%d')
df.outputs$DFO21_damages_wt_EMDAT21$Ended <- format(df.outputs$DFO21_damages_wt_EMDAT21$Ended,
                                                    '%Y/%m/%d')
df.outputs$DFO21_damages_wt_DFO15$Began <- format(df.outputs$DFO21_damages_wt_DFO15$Began,
                                                    '%Y/%m/%d')
df.outputs$DFO21_damages_wt_DFO15$Ended <- format(df.outputs$DFO21_damages_wt_DFO15$Ended,
                                                    '%Y/%m/%d')
df.outputs$DFO21_damages_wt_DFO15.EMDAT21$Began <- format(df.outputs$DFO21_damages_wt_DFO15.EMDAT21$Began,
                                                  '%Y/%m/%d')
df.outputs$DFO21_damages_wt_DFO15.EMDAT21$Ended <- format(df.outputs$DFO21_damages_wt_DFO15.EMDAT21$Ended,
                                                  '%Y/%m/%d')
df.outputs$EMDAT21_damages_wt_EMDAT21$Began <- format(df.outputs$EMDAT21_damages_wt_EMDAT21$Began,
                                                          '%Y/%m/%d')
df.outputs$EMDAT21_damages_wt_EMDAT21$Ended <- format(df.outputs$EMDAT21_damages_wt_EMDAT21$Ended,
                                                          '%Y/%m/%d')
write.table(df.outputs$DFO21_damages_wt_EMDAT21,
            sep = ",",row.names = FALSE,col.names = TRUE,quote = TRUE,
            './Data/output.data.files/DFO21_damages_wt_EMDAT21.csv')
write.table(df.outputs$DFO21_damages_wt_DFO15,
            sep = ",",row.names = FALSE,col.names = TRUE,quote = TRUE,
            './Data/output.data.files/DFO21_damages_wt_DFO15.csv')
write.table(df.outputs$DFO21_damages_wt_DFO15.EMDAT21,
            sep = ",",row.names = FALSE,col.names = TRUE,quote = TRUE,
            './Data/output.data.files/DFO21_damages_wt_DFO15_EMDAT.combined.csv')
write.table(df.outputs$EMDAT21_damages_wt_EMDAT21,
            sep = ",",row.names = FALSE,col.names = TRUE,quote = TRUE,
            './Data/output.data.files/EMDAT21_damages_wt_EMDAT21.csv')


# Go to ./Data/output.data.files/... *.csv files to check the damages estimates #

