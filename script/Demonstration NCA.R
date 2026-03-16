# Demonstration NCA
# February 19, 2026

# Testing three hypotheses buyer-supplier service outsourcing relationships:
# H1: Contractual detail (X1) is necessary for innovation performance (Y)
# H2: Goodwill trust (X2) is necessary for innovation performance (Y)
# H3: Competence trust (X3) is necessary for innovation performance (Y)
# Data from Van der Valk et al. (2016)
# Data available for 48 relationships.
# Data embedded in the NCA software as 'nca.example2'.

#----Install and load NCA package and data

# Install and load the NCA package
# install.packages("NCA")  # remove "#" in front if package is not installed
library(NCA)  # load the package

# Limit computation time
test.rep <- 1000 # for final analysis set 10000

# Load the data
data("nca.example2")  # load the data
data <- nca.example2  # rename the data for convenience
nrow(data)            # number of rows (cases)
head(data)            # display the first rows of the data


#----Visual inspection 

# Create XY-plots for visual inspection
# Using base R functions (not part of the NCA package)
plot(data$`Contractual detail`, data$Innovation,
     xlab = "Contractual detail",
     ylab = "Innovation")
plot(data$`Goodwill trust`, data$Innovation,
     xlab = "Goodwill trust",
     ylab = "Innovation")
plot(data$`Competence trust`, data$Innovation,
     xlab = "Competence trust",
     ylab = "Innovation")


#----Estimate effect size and p-value

# Conduct NCA
set.seed(123) # for reproducible results
model1 <- nca_analysis(data,
                       x = c('Contractual detail',
                             'Goodwill trust',
                             'Competence trust'),
                       y = 'Innovation',
                       ceilings = 'ce_fdh',
                       corner = 1,          # default upper-left corner
                       scope = NULL,        # default empirical scope
                       test.rep = test.rep) 

# Print main results
print(model1)

# Display XY-plots with ceiling lines
nca_output(model1, summaries = FALSE)


#---- Evaluate model fit

# Print detailed output
nca_output(model1, plots = FALSE, summaries = TRUE)


# Extract model fit metrics
xs = c('Contractual detail', 'Goodwill trust', 'Competence trust')
ceiling <- "ce_fdh"
metrics <- c("Complexity", "Fit", "Ceiling accuracy", "Noise", "Exceptions",
             "Support", "Spread", "Sharpness")

tab1 <- sapply(xs, function(x) {
  sapply(metrics, function(p) {
    nca_extract(model1, x = x, ceiling = ceiling, param = p)
  })
})

# Print model fit metrics
tab1_df <- data.frame(Metric = rownames(tab1), tab1,
                      row.names = NULL, check.names = FALSE)
tab1_df


#----Robustness checks

# Perform robustness check with other ceiling line
set.seed(123) # for reproducible results
model2 <- nca_analysis(data,
                       x = c('Contractual detail', 
                             'Goodwill trust',
                             'Competence trust'),
                       y = 'Innovation',
                       ceilings = "cr_fdh",
                       corner = 1,          # default upper-left corner
                       scope = NULL,        # default empirical scope
                       test.rep = test.rep)     
print(model2)
nca_output(model2, summaries = FALSE)

# Extract model fit metrics
xs = c('Contractual detail', 'Goodwill trust', 'Competence trust')
ceiling <- "cr_fdh"
metrics <- c("Complexity", "Fit", "Ceiling accuracy", "Noise", "Exceptions",
             "Support", "Spread", "Sharpness")
tab2 <- sapply(xs, function(x) {
  sapply(metrics, function(p) {
    nca_extract(model2, x = x, ceiling = ceiling, param = p)
  })
})

# Print model fit metrics
tab2_df <- data.frame(Metric = rownames(tab2), tab2, 
                      row.names = NULL, check.names = FALSE)
tab2_df


# Robustness check with different scopes corresponding to Likert scales
model3 <- nca_analysis(data,
                       x = c('Contractual detail', 
                             'Goodwill trust',
                             'Competence trust'),
                       y = 'Innovation',
                       ceilings = "ce_fdh",
                       corner = 1,          # default upper-left corner
                       scope = list (c(1,7,1,5), c(1,5,1,5), c(1,5,1,5)),  
                       test.rep = test.rep)     
print(model3)
nca_output(model3, summaries = FALSE)

# Extract model fit metrics
xs = c('Contractual detail', 'Goodwill trust', 'Competence trust')
ceiling <- "ce_fdh"
metrics <- c("Complexity", "Fit", "Ceiling accuracy", "Noise", "Exceptions",
             "Support", "Spread", "Sharpness")
tab3 <- sapply(xs, function(x) {
  sapply(metrics, function(p) {
    nca_extract(model3, x = x, ceiling = ceiling, param = p)
  })
})

# Print model fit metrics
tab3_df <- data.frame(Metric = rownames(tab3), tab3,
                      row.names = NULL, check.names = FALSE)
tab3_df

# Single outlier analysis
nca_outliers(data, 2, 1, plotly = TRUE, ceiling = 'ce_fdh')  # Contractual detail
nca_outliers(data, 3, 1, plotly = TRUE, ceiling = 'ce_fdh')  # Goodwill trust
nca_outliers(data, 4, 1, plotly = TRUE, ceiling = 'ce_fdh')  # Competence trust

# Double outliers analysis (two potential outliers)
nca_outliers(data, 2, 1, plotly = TRUE, k = 2, ceiling = 'ce_fdh')  # Contractual detail
nca_outliers(data, 3, 1, plotly = TRUE, k = 2, ceiling = 'ce_fdh')  # Goodwill trust
nca_outliers(data, 4, 1, plotly = TRUE, k = 2, ceiling = 'ce_fdh')  # Competence trust


# Remove two potential outliers and perform NCA again
data1 <- data[-c(8, 45), ]
model4 <- nca_analysis(data1,
                       c('Contractual detail',
                         'Goodwill trust',
                         'Competence trust'),
                       'Innovation',
                       ceilings = 'ce_fdh',
                       corner = 1,
                       test.rep = test.rep)
model4
nca_output(model4, summaries = FALSE)


#----Bottleneck analysis: necessity-in-degree

# Default bottleneck analysis with percentage range for X and Y
nca_output(model1, bottlenecks = TRUE, summaries = FALSE, plots = FALSE)

# Bottleneck analysis with actual values for X and Y
model5 <- nca_analysis(data,
                       c('Contractual detail',
                         'Goodwill trust',
                         'Competence trust'),
                       'Innovation',
                       ceilings = 'ce_fdh',
                       bottleneck.x = 'actual',
                       bottleneck.y = 'actual',
                       steps = seq(1, 5, by = 1)) # values of Likert scale
nca_output(model5, bottlenecks = TRUE, summaries = FALSE, plots = FALSE)

# Bottleneck analysis with actual values for Y and percentile values for X
model6 <- nca_analysis(data,
                       c('Contractual detail',
                         'Goodwill trust',
                         'Competence trust'),
                       'Innovation',
                       ceilings = 'ce_fdh',
                       bottleneck.x = 'percentile',
                       bottleneck.y = 'actual',
                       steps = seq(1, 5, by = 1))
nca_output(model6, bottlenecks = TRUE, summaries = FALSE, plots = FALSE)


#----Statistical difference tests

# Contrast test: effect size difference between two conditions
test.type <- "contrast"
set.seed(123)
difference_contrast <- nca_difference(data1 = data,
                                      x = c("Goodwill trust", "Competence trust"),
                                      y = "Innovation",
                                      ceilings = "cr_fdh",
                                      test.rep = 100, # for final analysis set 10000 
                                      test.type = "contrast")
print(difference_contrast)

# Independence test: effect size difference between two datasets
data1 <- data[1:24, ]
data2 <- data[25-48, ]
set.seed(123)
difference_independent <- nca_difference(data1 = data1,
                                         data2 = data2,
                                         x = c("Contractual detail"),
                                         y = "Innovation",
                                         ceilings = "ce_fdh",
                                         test.rep = 100, # for final analysis set 10000 
                                         test.type = "independent")
print(difference_independent)

# Paired test: effect size differences between 2 measurements
data1 <- subset(nca.example2, select = c(1,2)) # select X and Y for T1 
# Simulate changes of X and Y at T2  
set.seed(123)
data2 <- data1
data2$`Contractual detail` <- pmin(5, pmax(1, data2$`Contractual detail`
                                           + sample(-1:1, nrow(data2), TRUE)
)
)
data2$`Innovation` <- pmax(1, ifelse(data2$`Innovation` 
                                     >= 4, data2$`Innovation` 
                                     - 1, data2$`Innovation`)
)
set.seed(123)
difference_paired <- nca_difference(data1=data1, 
                                    data2=data2,
                                    x = "Contractual detail",
                                    y = "Innovation",
                                    ceiling = "ce_fdh",
                                    test.rep = 100, # for final analysis set 10000 
                                    test.type = "paired")
print(difference_paired)

#----Power analysis

# Power analysis
set.seed(123)
power <- nca_power(n = 50, effect = 0.2) # This may take some time
print(power)


#### Random sample
# Get random sample with necessity

set.seed(123)
random <- nca_random (n = 100, intercepts = 0.2, slopes = 1) 
head(random)


# Save main results
nca_output(model6, summaries = TRUE, plots = TRUE, bottlenecks = TRUE, pdf = TRUE)


#---- Help

# Access help documentation for the NCA package and specific functions
help(package = "NCA")
help("nca_analysis")
help("nca_output")
help("nca_outliers")
help("nca_random")
help("nca_power")

