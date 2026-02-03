# R scripts Appendix 'Demonstration NCA with PLS-SEM using R' in
# Dul, J. (2026). Necessary Condition Analysis - NCA. Principles and Application. 
# Chapman & Hall/CRC Press.
# February 3, 2026

# Download the following helper functions and script from https://jandul.github.io/NCA/.
# get_indicator_data_TAM.R
# estimate_sem_model_TAM.R
# get_significance_sem.R
# unstandardize.R
# get_outliers.R
# get_ipma_df.R
# get_ipma_plot.R
# get_bottleneck_cases.R
# get_cipma_df.R
# get_cipma_plot.R
# get_single_bottleneck_cases.R
# get_bipma_df.R
# get_bipma_plot.R

#----
# Get indicator data
source("get_indicator_data_TAM.R")
df <- get_indicator_data_TAM()
head(df, 3)

#----
# Estimate SEM model
source("estimate_sem_model_TAM.R")
TAM_pls <- estimate_sem_model_TAM(df)
summary(TAM_pls)

#----
# Estimate significance SEM effects
source("get_significance_sem.R")
TAM_sig <- get_significance_sem (sem = TAM_pls,
                                 nboot = 5000) # This may take some time
TAM_sig

#----
# Unstandardize data
source("unstandardize.R")
dataset <- unstandardize(TAM_pls)
head(dataset, 3)

#----
# Min-max normalize data
data <- dataset
scale = c(0, 100) #new percentages scale
library(NCA) # use normalize utility of NCA 
min_max <- c(rep(c(1,5), 5), c(1,7)) # min and max values of original scale
dataset1 <-  nca_util_normalize(data = data, scale = scale, min_max = min_max)
head(dataset1, 3)

#----
# Conduct NCA - Technology use
library(NCA)
set.seed(123)
model1.technology <- nca_analysis(dataset, 1:5, 6,
                                  ceilings = "ce_fdh",
                                  test.rep = 10000) # This may take some time
print(model1.technology)

#----
# Conduct NCA - Adoption intention
model1.adoption <- nca_analysis(dataset, 1:4, 5,
                                ceilings = "ce_fdh",
                                test.rep = 10000) # This may take some time
print(model1.adoption)

#----
# Bottleneck table Technology use: actual-actual
library(NCA)
bottleneck.y = "actual"
bottleneck.x = "actual"
steps = c(1,2,3,4,5,6,7)
model2.technology <- nca_analysis(dataset,# unstandardized (non-normalized)
                                  1:5, # five conditions
                                  6, # outcome
                                  ceilings = "ce_fdh",
                                  bottleneck.x = bottleneck.x,
                                  bottleneck.y = bottleneck.y,
                                  steps = steps)
nca_output (model2.technology, summaries = FALSE, plots = FALSE,
            bottlenecks = TRUE)

#----
# Bottleneck table Technology use: actual - percentile
library(NCA)
bottleneck.y = "actual"
bottleneck.x = "percentile"
steps=seq(0, 100, 5)
model3.technology <- nca_analysis(dataset1, 1:5, 6,
                                  ceilings = "ce_fdh",
                                  bottleneck.x = bottleneck.x,
                                  bottleneck.y = bottleneck.y,
                                  steps = steps)
nca_output (model3.technology, summaries = FALSE, plots = FALSE,
            bottlenecks = TRUE)

#----
# Bottleneck cases - Technology use 
source("get_bottleneck_cases.R")
data <- dataset1
predicted <- "Technology use"
predictors <- c("Perceived usefulness",
                "Compatibility",
                "Perceived ease of use",
                "Emotional value",
                "Adoption intention")
corner = 1 # default; expected empty space for all conditions
target_outcome <- 85
ceiling <- "ce_fdh"
bottlenecks_technology <- get_bottleneck_cases(data = data,
                                               conditions = predictors,
                                               outcome = predicted,
                                               corner = corner,
                                               target_outcome = target_outcome,
                                               ceiling = ceiling)
bottlenecks_technology

#----
#NERT - Adoption intention
library(kableExtra)
library(knitr)
if (is_latex_output()) {
  nec_header <- kableExtra::linebreak("Necessity\neffect size", align = "c")
} else {
  nec_header <- "Necessity<br>effect size"
}

mmt <- data.frame(
  Row = c("Perceived usefulness",
          "Compatibility",
          "Perceived ease of use",
          "Emotional value",
          "R$^2$",
          "R$^2$(Adjusted)"
  ),
  
  `Path coefficient ` = c("0.227", "0.045", "0.088", "0.515", "0.539", "0.528"),
  `p-value`     =  c("<0.05", "$ns$", "$ns$", "<0.05", "", ""),
  `Necessity effect size` = c("0.12", "0.08", "0.15","0.21", "", ""),
  `p-value`     =  c("0.002", "0.010", "0.007", "<0.001","", ""),
  check.names = FALSE
)

note_text2 <- c(
  "N = 174.",
  "$ns$ is non-significant for alpha = 0.05.",
  "SEM: p-value estimated from 5,000 bootstrap resamples.",
  "NCA: all necessary conditions evaluated with CE-FDH;",
  "    p-value estimated from 10,000 permutations."
)

kbl(
  mmt,
  booktabs = TRUE,
  caption = "Summary of the SEM and NCA results for outcome Adoption intention.",
  col.names = c("",
                "Path coefficient",
                "p-value",
                nec_header,
                "p-value"
  ),
  align = c("l", "c","c", "c", "r"),
  escape = FALSE) %>%
  kable_styling(full_width = FALSE, latex_options = c("HOLD_position")) %>%
  column_spec(1, width = "8em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "6em") %>%
  column_spec(4, width = "5em", border_left = TRUE) %>% 
  column_spec(5, width = "5em") %>%
  pack_rows("Predictors", 1,4) %>%
  pack_rows("Model fit (SEM)", 5, 6, hline_before = TRUE) %>%
  footnote(general = note_text2, escape = FALSE)


#----
#NERT - Technology use
library(kableExtra)
library(knitr)
if (is_latex_output()) {
  nec_header <- kableExtra::linebreak("Necessity\neffect size", align = "c")
} else {
  nec_header <- "Necessity<br>effect size"
}

mmt <- data.frame(
  Row = c("Perceived usefulness",
          "Compatibility",
          "Perceived ease of use",
          "Emotional value",
          "Adoption intention",
          "R$^2$",
          "R$^2$(Adjusted)"
  ),
  
  `Direct path coefficient ` = c("0.050", "0.107", "0.010", "0.137", "0.437", "0.539", "0.528"),
  `p-value`     =  c("$ns$", "$ns$", "$ns$", "$ns$","< 0.05",  "", ""),
  `Total path coefficient ` = c("0.149", "0.127", "0.049", "0.362", "0.437", "", ""),
  `p-value`     =  c("$ns$", "$ns$", "$ns$", "<0.05","<0.05",  "", ""),
  `Necessity effect size` = c("0.24", "0.21", "0.24","0.33", "0.29", "", ""),
  `p-value`     =  c("0.001", "<0.001", "0.015", "<0.001","<0.001", "" , ""),
  check.names = FALSE
)

note_text2 <- c(
  "N = 174.",
  "$ns$ is non-significant for $\\alpha$ = 0.05.",
  "SEM: p-value estimated from 5,000 bootstrap resamples.",
  "NCA: all necessary conditions evaluated with CE-FDH;",
  "    p-value estimated from 10,000 permutations."
)

kbl(
  mmt,
  booktabs = TRUE,
  caption = "Summary of the SEM and NCA results for outcome Technology use.",
  col.names = c("",
                "Direct path coefficient",
                "p-value",
                "Total path coefficient",
                "p-value",
                nec_header,
                "p-value"
  ),
  align = c("l", "c", "c","c", "c", "r"),
  escape = FALSE   
  
) %>%
  kable_styling(full_width = FALSE, latex_options = c("HOLD_position")) %>%
  column_spec(1, width = "8em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "4em") %>%
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "4em") %>%
  column_spec(6, width = "4em", border_left = TRUE) %>% 
  
  pack_rows("Predictors", 1,5) %>%
  pack_rows("Model fit (SEM)", 6, 6, hline_before = TRUE) %>%
  footnote(general = note_text2, escape = FALSE)

#----
# IPMA  - dataset
source("get_ipma_df.R")
data <- dataset1 # 0-100 normalized unstandardized
sem <- TAM_pls
predicted <- "Technology use"
predictors <-  c("Perceived usefulness", 
                 "Compatibility",
                 "Perceived ease of use",
                 "Emotional value",
                 "Adoption intention")
IPMA_df_technology <- get_ipma_df(data = data, 
                                  sem = sem, 
                                  predictors = predictors, 
                                  predicted = predicted)
IPMA_df_technology

#----
# IPMA  - plot
source("get_ipma_plot.R")
ipma_df <- IPMA_df_technology 
x_range <- c(0,0.6) # range of the Importance axis
y_range <- c(0,100) # range of the Performance axis
IPMA_plot_technology <- get_ipma_plot(ipma_df = ipma_df,
                                      x_range = x_range,
                                      y_range = y_range) 

#----
# cIPMA  - dataset
source("get_cipma_df.R")
d_threshold <- 0.10 
p_threshold <- 0.05
model <- model1.technology
effect_size <- sapply(predictors, function(p) model$summaries[[p]][[2]][[2]])
p_value <- sapply(predictors, function(p) model$summaries[[p]][[2]][[6]])
necessity <- ifelse(effect_size >= d_threshold & p_value <= p_threshold,
                    "yes", "no")

ipma_df <- IPMA_df_technology
bottlenecks <- bottlenecks_technology
CIPMA_df_technology <- get_cipma_df(ipma_df = ipma_df, 
                                    bottlenecks = bottlenecks, 
                                    necessity = necessity)
CIPMA_df_technology

#----
# cIPMA  - plot
source("get_cipma_plot.R")
cipma_df <- CIPMA_df_technology 
x_range <- c(0,0.6)
y_range <- c(0,100)
size_limits = c(0,100)
size_range <- c(0.5,50) # the size of the bulbs
name_plot <- "Original" 
cIPMA_plot_technology <- get_cipma_plot(cipma_df = cipma_df,
                                        x_range = x_range,
                                        y_range = y_range,
                                        size_limits = size_limits,
                                        size_range = size_range)

#----
# Single bottleneck cases - Target outcome 85
source("get_single_bottleneck_cases.R")
data <- dataset1
ceilings = "ce_fdh"
target_outcome <- 85
predicted <- "Technology use"
predictors <-  c("Perceived usefulness", 
                 "Compatibility",
                 "Perceived ease of use",
                 "Emotional value",
                 "Adoption intention")
corner = 1 # default expected empty space for all conditions
single_bottlenecks_technology <- get_single_bottleneck_cases(
  data, 
  conditions = predictors,
  outcome = predicted,
  corner = corner,
  target_outcome=target_outcome, 
  ceiling = ceilings)
single_bottlenecks_technology

#----
# Significant total effects (SEM)
sem_sig <- TAM_sig
for_select_predicted <- sem_sig[sem_sig$Predicted == predicted, ]
sufficiency <- ifelse(for_select_predicted$Sig_Total, "yes", "no")
names(sufficiency) <- predictors

#----
# BIPMA  - dataset
source("get_bipma_df.R")
ipma_df <- IPMA_df_technology
single_bottlenecks <- single_bottlenecks_technology
BIPMA_df_technology <- get_bipma_df(ipma_df = ipma_df,
                                    single_bottlenecks=single_bottlenecks, 
                                    necessity = necessity, 
                                    sufficiency = sufficiency)
BIPMA_df_technology

#----
# Bipma - plot
source("get_bipma_plot.R")
bipma_df <- BIPMA_df_technology 
x_range <- c(0,0.6)
y_range <- c(0,100)
size_limits = c(0,100)
size_range <- c(0.5,50) # the size of the bulbs
BIPMA_plot_technology <- get_bipma_plot(bipma_df = bipma_df,
                                        x_range = x_range, 
                                        y_range = y_range, 
                                        size_limits = size_limits, 
                                        size_range = size_range)

