# configuration function
performance_model_measures <- function(obs_vec,
                                       sim_vec){
  # a list of performance measures
  # PRESS: prediction sum of squares
  
  # AIC: Akaike’s Information Criterion
  # BIC: Bayesian Information Criterion
  # APC: Amemiya’s Prediction Criterion
  # predicted.R2: predicted R2
  # SSTO: total sum of squares
  # SSE: error sum of squares
  # SSR: regression sum of squares
  obs <- obs_vec
  sim <- sim_vec
  
  n <- length(obs)                     # Number of observations
  p <- 4            # Number of parameters including the intercept
  
  
  names_stats <- c("PRESS",
                   "AIC","BIC","predicted.R2",
                   "SSE","SSR")
  
  # as.matrix(names_stats)
  # [,1]          
  # [1,] "PRESS"       
  # [2,] "AIC"         
  # [3,] "BIC"         
  # [4,] "predicted.R2"
  # [5,] "SSE"         
  # [6,] "SSR"   
  
  press <- sum((obs-sim)^2)
  
  # Sum of Squares Total (SSTO)
  SSTO <- sum((obs - mean(obs))^2)
  
  # Sum of Squares for Error (SSE)
  SSE <- sum((obs - sim)^2)
  
  # Sum of Squares for Regression (SSR)
  SSR <- SSTO - SSE
  
  
  AIC_value <- n * log(SSE / n) + 2 * p
  
  
  BIC_value <- n * log(SSE / n) + p * log(n)
  
  
  # Predicted R-squared (leave-one-out cross-validation)
  predicted_R2 <- 1 - (SSE / SSTO)
  
  # Print results
  results <- data.frame(
    Metric = names_stats,
    Value = c(press, 
              AIC_value, 
              BIC_value, 
              predicted_R2,
              SSE,
              SSR))
  
  return(results)
  
}