# configuration function
linear_model_fit <- function(training.data,
                             testing.data){
  # Linear Models: Linear Regression
  

  print('-- LM Started --')

  lm_fit = lm(damage ~ ., data = training.data)
  
  # in-sample #
  fit.data <- predict(lm_fit,training.data)
  
  # out-of-sample #
  predict.data <- predict(lm_fit,testing.data)
  
  lm.vars.pval <- summary(lm_fit)$coefficients
  lm_fit.coefficients <- coef(lm_fit)# coefficients of the linear model (all
  
  param <- list(lm.vars.pval,lm_fit.coefficients)
  

  out <- list(predict.data,fit.data,param)
  names(out) <- c('out.of.sample_estimate',
                  'in.sample_estimate',
                  'param')
  return(out)
}


