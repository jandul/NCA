# Helper function to create a BIPMA dataset with Importance, Performance, 
# and Bottlenecks (percentage and number of SINGLE bottleneck cases)
# Revised November 8, 2025

get_bipma_df <- function(ipma_df, single_bottlenecks, necessity, sufficiency) { 
  
  # Extract number and percentage of single bottleneck cases
  single_cases_num <- single_bottlenecks$single_bottleneck_cases_num
  single_cases_per <- single_bottlenecks$single_bottleneck_cases_per
  
  # Combine IPMA data with bottleneck info
    bipma_df <- cbind(
    ipma_df,
    single_cases_per,
    single_cases_num
  )
  
  rownames(bipma_df) <- rownames(ipma_df)
  colnames(bipma_df)[4:5] <- c("Single_bottleneck_cases_per", "Single_bottleneck_cases_num")
  
  # Add necessity classification
  bipma_df$Necessity <- necessity
  
  # Add corner classification
  bipma_df$Corner <- corner
  
  # Add target_outcome
  bipma_df$Target_outcome <- target_outcome
  
  # Add probabilistic sufficiency classification
  bipma_df$Sufficiency <- sufficiency
  
  # Add predictor labels:
  bipma_df$Predictor_with_single_cases <- ifelse(
    bipma_df$Necessity == "yes",
    paste0(
      bipma_df$predictor,
      " (",
      round(bipma_df$Single_bottleneck_cases_per, 0), "%, n=",
      bipma_df$Single_bottleneck_cases_num,
      ifelse(
        bipma_df$Corner %in% c(2, 3, 4),
        paste0(", corner=", bipma_df$Corner),
        ""
      ),
      ")"
    ),
    paste0(bipma_df$predictor, " (NN)")
  )
  
  return(bipma_df)
}
