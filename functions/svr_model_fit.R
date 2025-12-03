# configuration function
svr_model_fit <- function(training.data,
                             testing.data){
  # Support Vector Machines: Support Vector Regression (SVR) (Support Vector Machines with Radial Basis Function Kernel)
  

  print('-- SVR Started --')
  
  svr_fit <- train(damage ~ ., data=training.data,
                   method='svmRadial',
                   trControl=trainControl(method='cv',number=10))
  
  # in-sample #
  fit.data <- predict(svr_fit,training.data)
  
  # out-of-sample #
  predict.data <- predict(svr_fit,testing.data)
  
  
  best_parameters.svr_fit <- as.matrix(svr_fit$bestTune)  # Best hyperparameters
  
  param <- best_parameters.svr_fit
  

  out <- list(predict.data,fit.data,param)
  names(out) <- c('out.of.sample_estimate',
                  'in.sample_estimate',
                  'param')
  return(out)
}

# e.g.,
# cv_results.svr_fit <- svr_fit$results  # Cross-validation results
# ->> parameters::
#   parameter  label
# 1     sigma  Sigma
# 2         C   Cost
