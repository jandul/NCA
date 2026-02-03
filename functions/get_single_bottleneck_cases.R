# Helper function to find the number of single bottleneck cases per condition  
# Revised December 9, 2025

get_single_bottleneck_cases <- function(data, conditions, outcome, corner = 1, target_outcome, ceiling) {
  # Check if target_outcome is within the range of the outcome variable
  outcome_min <- min(data[[outcome]], na.rm = TRUE)
  outcome_max <- max(data[[outcome]], na.rm = TRUE)
  
  if (length(corner) == 1 && corner == 1) {corner <- rep(1, length(conditions))}
  
  if (target_outcome < outcome_min || target_outcome > outcome_max) {
    stop(
      sprintf(
        "Error: target_outcome (%f) is out of range for the outcome variable '%s'. Minimum: %f, Maximum: %f",
        target_outcome, outcome, outcome_min, outcome_max
      )
    )
  }
  
  # Initialize matrices to store bottlenecks
  bottleneck_matrix <- matrix(0, nrow = nrow(data), ncol = length(conditions))
  colnames(bottleneck_matrix) <- conditions
  
  threshold <- setNames(numeric(length(conditions)), conditions)
  eps <- .Machine$double.eps^0.5  # Smallest safe numerical tolerance
  
  for (condition in conditions) {
    
    # Find index of the condition
    condition_index <- which(conditions == condition)[1]  # Get the index of the current condition
    
    # Get corner value
    corner_val <- corner[condition_index]
    
    # Check if corner_val is NA or not in valid range (1 to 4)
    if (is.na(corner_val)) {
      warning(sprintf("Warning: corner value for condition '%s' is NA. Defaulting to 1.", condition))
      corner_val <- 1L
    } else if (!(corner_val %in% 1:4)) {
      warning(sprintf("Warning: corner value for condition '%s' (%d) is out of valid range (1-4). Defaulting to 1.", condition, corner_val))
      corner_val <- 1L
    }
    
    # Perform NCA for the given condition
    model1 <- nca_analysis(
      data, 
      condition, 
      outcome, 
      corner = corner_val,
      ceilings = ceiling, 
      bottleneck.y = "actual", 
      bottleneck.x = "actual", 
      steps = c(target_outcome, NA)
    )
    # Extract accurate threshold
    threshold[condition] <- attr(model1[["bottlenecks"]][[ceiling]][[condition]],
                                 "mpx")[1, 1]
    
    if (corner_val %in% c(1, 3)) {
      bottleneck_matrix[, condition] <- as.numeric(
        data[[condition]] < (threshold[condition] - eps)
      )
    } else {
      bottleneck_matrix[, condition] <- as.numeric(
        data[[condition]] > (threshold[condition] - eps)
      )
    }
    
    # If no cases are bottlenecks, reset to 0
    if (sum(bottleneck_matrix[, condition]) == nrow(data)) {
      bottleneck_matrix[, condition] <- 0
    }
    if (threshold[condition] == -Inf) {
      threshold[condition] <- 0
    }
  }
  
  # Calculate the total bottlenecks for each row
  total_bottlenecks_per_row <- rowSums(bottleneck_matrix)
  
  # Rowname outputs
  single_bottleneck_matrix <- bottleneck_matrix & (total_bottlenecks_per_row == 1)
  single_bottleneck_case_names <- lapply(colnames(bottleneck_matrix), function(cn) {
    rownames(data)[single_bottleneck_matrix[, cn] == 1]
  })
  names(single_bottleneck_case_names) <- colnames(bottleneck_matrix)
  any_bottleneck_case_names <- rownames(data)[total_bottlenecks_per_row > 0]

  
  
  # Count the number of rows where each case is the sole bottleneck
  single_bottlenecks_per_condition <- colSums(bottleneck_matrix & (total_bottlenecks_per_row == 1))
  
  single_bottleneck_cases_num <- as.data.frame (single_bottlenecks_per_condition) # number
  colnames(single_bottleneck_cases_num)[1] <- paste0("Bottlenecks (Y = ", target_outcome, ")")
  single_bottleneck_cases_per <- as.data.frame (100 * (single_bottleneck_cases_num / nrow(data))) # percentage
 
  return(list(
    single_bottleneck_cases_per = single_bottleneck_cases_per,
    single_bottleneck_cases_num = single_bottleneck_cases_num,
    threshold = threshold,
    single_bottleneck_case_names = single_bottleneck_case_names,
    any_bottleneck_case_names = any_bottleneck_case_names
  ))
  
}
