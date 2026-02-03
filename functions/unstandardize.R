# Helper function to obtain a dataset with the unstandardized latent variables 
# using the output of the SEMinR pls model estimation as function argument.
# Revised May 23, 2025

unstandardize <- function(sem) {
  
  # Extract the original indicators
  original_indicators_df <- as.data.frame(sem$rawdata)
  
  # Extract the standardized weights and store results in a dataframe
  standardized_weights_matrix <- sem$outer_weights
  standardized_weights_df <- as.data.frame(as.table(standardized_weights_matrix))
  colnames(standardized_weights_df) <- c("Indicator", "Latent variable", "Standardized weight")
  standardized_weights_df <- subset(standardized_weights_df, `Standardized weight` != 0)
  
  # Match SDs to indicators
  standardized_weights_df$SD <- sem$sdData[as.character(standardized_weights_df$Indicator)]
  
  # Compute unstandardized weights
  standardized_weights_df$`Unstandardized weight` <- 
  standardized_weights_df$`Standardized weight` / standardized_weights_df$SD  # Following Ringle and Sarstedt 2016 
  
   # Normalize weights within each latent variable
  sum_weights <- tapply(
    standardized_weights_df$`Unstandardized weight`, 
    standardized_weights_df$`Latent variable`, 
    sum
  )
  
  standardized_weights_df$`Sum unstandardized weight` <- 
    sum_weights[match(standardized_weights_df$`Latent variable`, names(sum_weights))]
  
  standardized_weights_df$`Normalized unstandardized weight` <- 
    standardized_weights_df$`Unstandardized weight` / standardized_weights_df$`Sum unstandardized weight`
  
  # Compute unstandardized latent variables
  unstandardized_latent_variable_df <- data.frame(matrix(nrow = nrow(original_indicators_df), ncol = 0))
  
  for (latent_variable in unique(standardized_weights_df$`Latent variable`)) {
    latent_data <- subset(standardized_weights_df, `Latent variable` == latent_variable)
    indicators <- as.character(latent_data$Indicator)
    weights <- latent_data$`Normalized unstandardized weight`
    indicator_scores <- original_indicators_df[, indicators, drop = FALSE]
    latent_score <- rowSums(sweep(indicator_scores, 2, weights, `*`))
    unstandardized_latent_variable_df[[latent_variable]] <- latent_score
  }
  
  return(unstandardized_latent_variable_df)
}
