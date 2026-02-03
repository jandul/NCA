# Helper function to create a cIPMA dataset with Importance, Performance, 
# and Bottlenecks (percentage and number of bottleneck cases)
# Revised August 9, 2025

get_cipma_df <- function(ipma_df, bottlenecks, necessity) { 
  
  # Extract number and percentage of single bottleneck cases
  cases_num <- bottlenecks$bottleneck_cases_num
  cases_per <- bottlenecks$bottleneck_cases_per

  # Combine IPMA data with bottleneck info
  cipma_df <- cbind(
    ipma_df,
    cases_per,
    cases_num
  )
  rownames(cipma_df) <- rownames(ipma_df)
  colnames(cipma_df)[4:5] <- c("Bottleneck_cases_per", "Bottleneck_cases_num")
  
  
  # Add necessity classification
  cipma_df$Necessity <- necessity
  
  # Add predictor labels:
  cipma_df$Predictor_with_cases <- ifelse(
    cipma_df$Necessity == "yes",
    paste0(
      cipma_df$predictor, 
      " (", 
      round(cipma_df$Bottleneck_cases_per, 0), "%, n=",
      cipma_df$Bottleneck_cases_num,
      ")"
    ),
    paste0(cipma_df$predictor, " (NN)")
  )
  
  return(cipma_df)
}


