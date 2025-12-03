##/-------------------------------------------------------------------\##
## explainable flood damage modeling functions based on 
## Variable Importance (vip) ##
##/-------------------------------------------------------------------\##
rm(list=ls())

library(readxl)
library(reshape2)
library(caret) # for ML models
library(xgboost) # for xbgTree
library(plyr) # for xbgTree
library(e1071) # for support vector regression
library(kernlab) # for vector regression
library(brnn) # for bayesian neural network
library(viridis) # for color plotting
library(patchwork) # for combining plots consistently
library(lime) # for LIME
library(dplyr) # for computations

##/ directory
mainDir <- 'D:/PC_Cornell/Data/DFO_floods_damages'
setwd(mainDir)

#------------------------------------------------
#--   EMDAT data: 1985 - 2021   -- #
#------------------------------------------------
lst_EMDAT <- readRDS('./Data/processed.data.files/lst_EMDAT.data_training.testing.rds')

df_shuffled <- lst_EMDAT$df_shuffled
df_shuffled.log <-lst_EMDAT$df_shuffled.log
# dim(df_shuffled.log)
# [1] 653   4

x <- colnames(df_shuffled.log)[-1]
y <- colnames(df_shuffled.log)[1]



#------------------------------------------------
## Linear model fit ##
lm_fit = train(damage ~ ., data=df_shuffled.log,
               method='lm',
               trControl=trainControl(method='cv',number=10))
lm_fit_emdat <- summary(lm_fit)$coefficients
#------------------------------------------------
## ensemble of Random forest fits for vip ##
num_ens <- 50
all.mat_prct_incMSE <- array(NA,c(num_ens,ncol(df_shuffled.log)-1))
for(k in 1:num_ens){
  print(k)
  set.seed(k)
  rf_fit = train(damage ~ ., data=df_shuffled.log,
                 method='rf',
                 importance=TRUE,
                 trControl=trainControl(method='cv',number=10))
  
  # evaluate variable importance (i.e., mean %IncMSE divided by its standard deviation): https://stats.stackexchange.com/questions/109270/caret-varimp-for-randomforest-model
  a1 <- varImp(rf_fit)
  a2 <- varImp(rf_fit, scale = FALSE)
  b <- data.frame(a1$importance,a2$importance)
  ww <- order(rownames(b))
  mat_prct_incMSE <- b[ww,1] # variable importance
  all.mat_prct_incMSE[k,] <- mat_prct_incMSE
}
mat_prct_incMSE <- data.frame(all.mat_prct_incMSE)
colnames(mat_prct_incMSE) <- names(df_shuffled.log[-1])
mat_prct_incMSE_emdat <- mat_prct_incMSE


##

#------------------------------------------------
#--   DFO15 data: 1985 - 2015   -- #
#------------------------------------------------
lst_DFO15 <- readRDS('./Data/processed.data.files/lst_DFO15.data_training.testing.rds')

df_shuffled <- lst_DFO15$df_shuffled
df_shuffled.log <-lst_DFO15$df_shuffled.log
# dim(df_shuffled.log)
# [1] 653   4

x <- colnames(df_shuffled.log)[-1]
y <- colnames(df_shuffled.log)[1]



#------------------------------------------------
## Linear model fit ##
lm_fit = train(damage ~ ., data=df_shuffled.log,
               method='lm',
               trControl=trainControl(method='cv',number=10))
lm_fit_dfo <- summary(lm_fit)$coefficients
#------------------------------------------------
## ensemble of Random forest fits for vip ##
num_ens <- 50
all.mat_prct_incMSE <- array(NA,c(num_ens,ncol(df_shuffled.log)-1))
for(k in 1:num_ens){
  print(k)
  set.seed(k)
  rf_fit = train(damage ~ ., data=df_shuffled.log,
                 method='rf',
                 importance=TRUE,
                 trControl=trainControl(method='cv',number=10))
  
  # evaluate variable importance (i.e., mean %IncMSE divided by its standard deviation): https://stats.stackexchange.com/questions/109270/caret-varimp-for-randomforest-model
  a1 <- varImp(rf_fit)
  a2 <- varImp(rf_fit, scale = FALSE)
  b <- data.frame(a1$importance,a2$importance)
  ww <- order(rownames(b))
  mat_prct_incMSE <- b[ww,1] # variable importance
  all.mat_prct_incMSE[k,] <- mat_prct_incMSE
}
mat_prct_incMSE <- data.frame(all.mat_prct_incMSE)
colnames(mat_prct_incMSE) <- names(df_shuffled.log[-1])
mat_prct_incMSE_dfo <- mat_prct_incMSE


## plot vip from RF for EMDAT & DFO15 ##

ww.mom <- 12; hh.mom <- 6
file.name.fig <- paste0("./Figures/boxplot.prct.vip.RF_EMDAT.DFO15.png")
png(file.name.fig,width=ww.mom,height=hh.mom,units="in",res=300)
par(mfrow=c(1,2),mai=c(0.5,1,0.5,0.5))

labs_title <- c('EM-DAT','DFO15')
color_blind_friendly_palette <- colorRampPalette(c("#fc8d62", "#66c2a5", "#a6d854", "#8da0cb", "#e78ac3", "#ffd92f"))
my.box.cols <- color_blind_friendly_palette(length(labs_title))

covar_labels <- colnames(mat_prct_incMSE_emdat)
rf_vip <- list(mat_prct_incMSE_emdat,mat_prct_incMSE_dfo)
min.y <- 0; max.y <- 100
for(k in 1:length(labs_title)){
  boxplot(rf_vip[[k]],boxwex=0.5,
          main=labs_title[k],
          ylim=c(min.y,max.y),horizontal = F,
          outline=T,outcex=0.6,outcol="gray80",
          frame =TRUE,names=covar_labels,
          pch=16, col = my.box.cols[k],
          cex.axis=1.15, cex.lab=1.15,
          cex.main=2,font.main=1,
          ylab='vip [%]',las=1,
          axes = TRUE, ann = TRUE)
  grid(col='gray80')
  
 
}
dev.off()



## combined LM coefficients #
df_lm_fit_emdat <- data.frame(
  Term = factor(names(lm_fit_emdat[,1]), 
                levels = names(lm_fit_emdat[,1])),
  Estimate = lm_fit_emdat[,1],
  StdError = lm_fit_emdat[,2],
  Source = "EM-DAT"
)
# Data for lm_fit_dfo and lm_fit_emdat
df_lm_fit_dfo <- data.frame(
  Term = factor(names(lm_fit_dfo[,1]), 
                levels = names(lm_fit_dfo[,1])),
  Estimate = lm_fit_dfo[,1],
  StdError = lm_fit_dfo[,2],
  Source = "DFO"
)

ww.mom <- 8; hh.mom <- 6
file.name.fig <- paste0("./Figures/barplot.coef.LM_EMDAT.DFO15.png")
png(file.name.fig,width=ww.mom,height=hh.mom,units="in",res=300)
par(mfrow=c(1,1),mai=c(1.5,1,0.5,0.5),mar=c(5,5,5,5))

combined_data <- rbind(df_lm_fit_emdat,df_lm_fit_dfo)
# Filter out intercept and non-positive estimates for plotting
filtered_data <- combined_data[combined_data$Term != "(Intercept)", ]
# Create a bar plot without log scale
barplot_positions <- barplot(height = filtered_data$Estimate, names.arg = filtered_data$Term, 
                             ylim = range(filtered_data$Estimate + filtered_data$StdError, filtered_data$Estimate - filtered_data$StdError),
                             col = ifelse(filtered_data$Source == "EM-DAT", "red", "blue"), 
                             las = 2, cex.names = 1, cex.axis = 1.25, axis.lty = 0,
                             xlab = "", ylab = "Coefficient", main = "LM")
# Add error bars
arrows(x0 = barplot_positions, 
       y0 = filtered_data$Estimate - filtered_data$StdError,
       x1 = barplot_positions, 
       y1 = filtered_data$Estimate + filtered_data$StdError,
       angle = 90, code = 3, length = 0.1, col = ifelse(filtered_data$Source == "EM-DAT", "red", "blue"))
grid(NA,6,col='gray80')
abline(h=0,col='black')

legend("bottomright",legend = c("EM-DAT","DFO"), fill = c("red", "blue"))

dev.off()

##The end. #



