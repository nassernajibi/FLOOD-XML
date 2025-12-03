# configuration function
brnn_model_fit <- function(training.data,
                             testing.data){
  # Neural Networks: Bayesian Regularized Neural Networks (BRNN)
  

  print('-- BRNN Started --')
  
  brnn_fit <- train(damage ~ ., data=training.data,
                    method='brnn',
                    trControl=trainControl(method='cv',number=10))
  
  # in-sample #
  fit.data <- predict(brnn_fit,training.data)
  
  # out-of-sample #
  predict.data <- predict(brnn_fit,testing.data)
  
  
  best_parameters.brnn_fit <- as.matrix(brnn_fit$bestTune)  # Best hyperparameters
  
  param <- best_parameters.brnn_fit
  

  out <- list(predict.data,fit.data,param)
  names(out) <- c('out.of.sample_estimate',
                  'in.sample_estimate',
                  'param')
  return(out)
}

# e.g.,
# cv_results.brnn_fit <- brnn_fit$results  # Cross-validation results
# ->> parameters::
# parameter       label
# 1   neurons  # Neurons