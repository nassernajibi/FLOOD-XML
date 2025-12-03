# configuration function
rf_model_fit <- function(training.data,
                         testing.data){
  # Tree-based Models: Random Forest Regression
  
  
  print('-- RF Started --')
  
  rf_fit = train(damage ~ ., data=training.data,
                 method='rf',
                 importance=TRUE,
                 trControl=trainControl(method='cv',number=10))
  
  # in-sample #
  fit.data <- predict(rf_fit,training.data)
  
  # out-of-sample #
  predict.data <- predict(rf_fit,testing.data)
  
  
  # evaluate variable importance (i.e., mean %IncMSE divided by its standard deviation): https://stats.stackexchange.com/questions/109270/caret-varimp-for-randomforest-model
  a1 <- varImp(rf_fit)
  a2 <- varImp(rf_fit, scale = FALSE)
  b <- data.frame(a1$importance,a2$importance)
  ww <- order(rownames(b))
  mat_prct_incMSE <- b[ww,1] # variable importance
  best_parameters.rf_fit <- rf_fit$bestTune  # Best hyperparameters
  
  param <- list(mat_prct_incMSE,best_parameters.rf_fit)
  
  
  out <- list(predict.data,fit.data,param)
  names(out) <- c('out.of.sample_estimate',
                  'in.sample_estimate',
                  'param')
  return(out)
}


