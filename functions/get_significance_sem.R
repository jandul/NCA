# Helper function to get significance of SEM predictors
# Revised August 4, 2025

get_significance_sem <- function(sem, nboot = 5000) {
  
  # Run bootstrap
  boot_model <- bootstrap_model(sem, nboot = nboot) # alpha is fixed at 0.05 
  
  # Extract direct and total path matrices
  df_direct <- as.data.frame(summary(boot_model)[[2]])
  df_total  <- as.data.frame(summary(boot_model)[[6]])
  
  # Rename confidence interval columns
  colnames(df_direct)[5:6] <- c("CI_low", "CI_high")
  colnames(df_total)[5:6]  <- c("CI_low", "CI_high")
  
  # Add Path column from rownames
  df_direct <- cbind(Path = rownames(df_direct), df_direct)
  df_total  <- cbind(Path = rownames(df_total), df_total)
  
  # Reset row names
  rownames(df_direct) <- NULL
  rownames(df_total)  <- NULL
  
  # Keep original order
  path_order <- df_direct$Path
  
  # Merge direct and total
  merged_df <- merge(df_total, df_direct, by = "Path", suffixes = c("_total", "_direct"))
  
  # Clean column names for safe access
  colnames(merged_df) <- make.names(colnames(merged_df))
  
  # Calculate indirect effect
  merged_df$Indirect_Estimate <- merged_df$Original.Est._total - merged_df$Original.Est._direct
  
  # Significance flags
  merged_df$Sig_Direct   <- (merged_df$CI_low_direct > 0 | merged_df$CI_high_direct < 0)
  merged_df$Sig_Total    <- (merged_df$CI_low_total > 0 | merged_df$CI_high_total < 0)
  merged_df$Sig_Indirect <- merged_df$Sig_Total & !merged_df$Sig_Direct
  
  # Final summary
  summary_df <- merged_df[, c(
    "Path",
    "Original.Est._direct", "Indirect_Estimate", "Original.Est._total",
    "Sig_Direct", "Sig_Indirect", "Sig_Total"
  )]
  
  # Round for readability
  summary_df <- within(summary_df, {
    Original.Est._direct  <- round(Original.Est._direct, 3)
    Indirect_Estimate     <- round(Indirect_Estimate, 3)
    Original.Est._total   <- round(Original.Est._total, 3)
  })
  
  # Preserve original order
  summary_df$Path <- factor(summary_df$Path, levels = path_order)
  summary_df <- summary_df[order(summary_df$Path), ]
  
  colnames(summary_df)[2:4] <- c("Direct", "Indirect", "Total")
  
  # Extract predicted variable from path (RHS of arrow)
  get_rhs <- function(path_string) {
    strsplit(as.character(path_string), "->")[[1]][2] |> trimws()
  }
  summary_df$Predicted <- sapply(as.character(summary_df$Path), get_rhs)
  
  # Return final table
  return(summary_df)
}
