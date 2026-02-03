# Helper function to create an IPMA dataset with Importance and Performance
# Revised Aug 8, 2025 - includes inversion handling for negative importance

get_ipma_df <- function(data, sem, predictors, predicted) {
  
  # Importance = total effects
  Importance_df <- as.data.frame(summary(sem)$total_effects)
  Importance <- Importance_df[predictors, predicted]
  
  # Performance = means of all latent variable scores
  Performance_df <- as.data.frame(sapply(data, mean))
  Performance <- Performance_df[predictors, 1]
  
  # Invert names and take absolute values where importance is negative
  renamed_predictors <- ifelse(Importance < 0,
                               paste0(predictors, "-inv"),
                               predictors)
  Importance <- abs(Importance)
  
  # Combine into IPMA dataframe
  ipma_df <- data.frame(
    predictor = renamed_predictors,
    Importance = Importance,
    Performance = Performance
  )
  
  return(ipma_df)
}
