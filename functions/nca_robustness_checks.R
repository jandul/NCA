# Helper function  to conduct robustness check
# Revised February 3, 2026

nca_robustness_checks <- function(data,
                                  conditions, 
                                  outcome, 
                                  ceiling = ceiling, 
                                  scope = scope,
                                  d_threshold = 0.10,
                                  p_threshold = 0.05, 
                                  outliers = 0, 
                                  test.rep = 1000,
                                  bottleneck.y = "percentile", 
                                  target_outcome = target_outcome, 
                                  plots = FALSE) {
  
  # Remove outliers if needed
  if (outliers != 0) {
    source("get_outliers.R")
    out <- get_outliers(data, conditions, outcome, ceiling, outliers)
    all_outliers <- unname(unlist(out$outliers_df)[!is.na(out$outliers_df)])
    data <- data[-all_outliers, ]
  }
  
  # Run the NCA analysis
  model <- nca_analysis(
    data,
    conditions, 
    outcome, 
    ceilings = ceiling, 
    bottleneck.x = 'percentile',
    bottleneck.y = bottleneck.y, 
    steps = c(target_outcome, NA),
    test.rep = test.rep,
    scope = scope
  )
  
  # View scatter plots
  if (plots) {
  nca_output(model, summaries = FALSE) #add pdf=plots to save pdf scatter plots
  }
  
  # Extract effect size and p value
  effect_size <- sapply(conditions, function(e) model$summaries[[e]][[2]][[2]])
  p_value <- sapply(conditions, function(p) model$summaries[[p]][[2]][[6]])
  necessity <- ifelse(effect_size >= d_threshold & p_value <= p_threshold, "yes", "no")
  
  # Extract the bottlenecks data frame from the model
  b <- as.data.frame(model$bottlenecks[[1]])
  row_index <- which(b[[1]] == target_outcome)
  row_values <- b[row_index, -1]
  Bottlenecks1 <- as.numeric(row_values) # percentage bottleneck cases 
  Bottlenecks2 <- round(Bottlenecks1 * nrow(data) / 100) # number bottleneck cases
  
  # Dataframe results
  results_all <- data.frame(
    Condition = conditions,
    Effect_size = effect_size,
    p_value = p_value,
    Necessity = necessity,
    Bottlenecks_percent = Bottlenecks1,
    Bottlenecks_count = Bottlenecks2
  )
  
  return(results_all)
}

##### Example #####

library(NCA)

# Select data
data(nca.example)
data <- nca.example

# Specify conditions and outcome
conditions <- colnames(data)[1:2]
outcome <- colnames(data)[3]

# Specify what is changed (and could be changed) 
ceiling <- "ce_fdh"
scope <- NULL
d_threshold <- 0.10 
p_threshold <- 0.05 
outliers <- 0 
target_outcome <- quantile(data[[outcome]], 0.75, na.rm = TRUE)

rob_check <- nca_robustness_checks(data = data,
                                   conditions = conditions, 
                                   outcome = outcome,
                                   ceiling = ceiling, 
                                   scope = scope,
                                   d_threshold =  d_threshold,
                                   p_threshold = p_threshold,
                                   outliers = outliers,
                                   target_outcome = target_outcome, 
                                   test.rep = 1000,
                                   bottleneck.y = "percentile", 
                                   plots = FALSE
)
rob_check
