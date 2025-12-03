# configuration function
knn_model_fit <- function(training.data,
                             testing.data){
  # Non-parametric Regression: k-Nearest Neighbors (k-NN) Regression
  
  
  print('-- K-NN Started --')
  
  knn_fit <- train(damage ~ ., data=training.data,
                    method='knn',
                    trControl=trainControl(method='cv',number=10))
  
  # in-sample #
  fit.data <- predict(knn_fit,training.data)
  
  # out-of-sample #
  predict.data <- predict(knn_fit,testing.data)
  
  
  best_parameters.knn_fit <- as.matrix(knn_fit$bestTune)  # Best hyperparameters
  
  param <- best_parameters.knn_fit
  

  
  out <- list(predict.data,fit.data,param)
  names(out) <- c('out.of.sample_estimate',
                  'in.sample_estimate',
                  'param')
  return(out)
}

# RMSE was used to select the optimal model using the smallest value.
# e.g.,
# cv_results.knn_fit <- knn_fit$results  # Cross-validation results
# ->> parameters::
# parameter         label
# 1     k      #Neighbors