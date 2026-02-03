# Helper function to get outliers 
# Revised June 1, 2025

get_outliers <- function (data, conditions, outcome, ceiling, k, row.numbers = TRUE) {
  if (row.numbers) {
    rownames(data) <- NULL
  }

  # Apply nca_outliers for each condition in the conditions vector
  outliers_list <- lapply(conditions, function (condition) {
    nca_outliers(data, x = condition, y = outcome, ceiling = ceiling, k = k)
  })

  # Extract the k datapoints for the first outlier, add NA's if needed
  k_outliers <- lapply(outliers_list, function(o) {
    # Check if o is a valid list/matrix and has a non-NULL, non-NA string in position [1,1]
    if (is.null(o) || is.null(o[[1, 1]]) || is.na(o[[1, 1]]) || !is.character(o[[1, 1]])) {
      return(rep(NA, k))
    }
    
    parts <- unlist(strsplit(o[[1, 1]], " - ", fixed = TRUE))
    if (row.numbers) {
      parts <- as.numeric(parts)
    }
    c(parts, rep(NA, k - length(parts)))
  })
  
  outliers_df <- do.call(rbind, k_outliers)
  colnames(outliers_df) <- paste("Outlier", 1:k)
  rownames(outliers_df) <- conditions

  return(list(outliers_list = outliers_list, outliers_df = outliers_df))
}
