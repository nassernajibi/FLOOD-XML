# configuration function
xgb_model_fit <- function(training.data,
                             testing.data){
  # Tree-based Models: Extreme Gradient Boosting (XGBoost)
  

  print('-- XGBoost Started --')

  xgb_fit <- train(damage ~ ., data=training.data,
                   method='xgbTree',
                   trControl=trainControl(method='cv',number=10))
  
  # in-sample #
  fit.data <- predict(xgb_fit,training.data)
  
  # out-of-sample #
  predict.data <- predict(xgb_fit,testing.data)
  
  
  best_parameters.xgb_fit <- as.matrix(xgb_fit$bestTune)  # Best hyperparameters
  
  param <- best_parameters.xgb_fit
  

  out <- list(predict.data,fit.data,param)
  names(out) <- c('out.of.sample_estimate',
                  'in.sample_estimate',
                  'param')
  return(out)
}

# e.g.,
# cv_results.xgb_fit <- xgb_fit$results  # Cross-validation results
# ->> parameters::
#         parameter                            label
# 1          nrounds           # Boosting Iterations
# 2        max_depth                  Max Tree Depth
# 3              eta                       Shrinkage
# 4            gamma          Minimum Loss Reduction
# 5 colsample_bytree      Subsample Ratio of Columns
# 6 min_child_weight  Minimum Sum of Instance Weight
# 7        subsample            Subsample Percentage
