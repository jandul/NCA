# R scripts Appendix 'Software' in
# Dul, J. (2026). Necessary Condition Analysis - NCA. Principles and Application. 
# Chapman & Hall/CRC Press.
# February 2, 2026

#----
# Install and load the NCA package
install.packages("NCA") # to install the NCA package (only ones)
library (NCA) # load the NCA package (for each new session)

#----
# General help
help(package = "NCA")

# Specific help 
help("nca_analysis")

#----
# Load the data
data(nca.example2)
data <- nca.example2 #load the example data from the NCA package and rename it
head(data) #show the first rows

#----
# Conduct NCA with column names
model1 <- nca_analysis(data, "Contractual detail", "Innovation")
model1

#----
# Conduct NCA with column numbers
model2 <- nca_analysis(data, 2,1)
model2

#----
# Conduct multiple NCA with three conditions
model3 <- nca_analysis(data, 2:4,1)
model3

#----
# Conduct NCA with selected ceiling line
model4 <- nca_analysis(data,2:4,1, ceilings = 'cr_fdh')
model4

#----
# Calculate the p-value
model5 <- nca_analysis(data, 2:4,1, test.rep = 1000) # This may take some time
model5

#----
# Show detailed results
model6 <- nca_analysis(data, 2,1, ceilings = 'cr_fdh', test.rep = 1000) # This may take some time
nca_output(model6)

#----
# Show interactive XY-plot(s)
nca_output(model1, plotly = TRUE, summaries = FALSE)

#----
# Show the bottleneck table
model7 <- nca_analysis(data, 2:4, 1, ceilings = 'cr_fdh')
nca_output(model7, bottlenecks = TRUE, summaries = FALSE)

#----
# Save main results
nca_output(model7, summaries = TRUE, plots = TRUE, bottlenecks = TRUE, pdf = TRUE)

#----
# Outlier analysis
outlier <- nca_outliers(data, 2, 1, ceiling = 'cr_fdh')
print(outlier)

#----
# Contrast test: effect size difference between two conditions
test.type <- "contrast"
set.seed(123)
difference_contrast <- nca_difference(data1 = data,
                                      x = c("Goodwill trust", "Competence trust"),
                                      y = "Innovation",
                                      ceilings = "cr_fdh",
                                      test.rep = 1000, #This make take some time
                                      test.type = "contrast")
print(difference_contrast)

#----
# Independence test: effect size difference between two datasets
data1 <- data[1:24, ]
data2 <- data[25-48, ]
set.seed(123)
difference_independent <- nca_difference(data1 = data1,
                                         data2 = data2,
                                         x = c("Contractual detail"),
                                         y = "Innovation",
                                         ceilings = "cr_fdh",
                                         test.rep = 1000, #This make take some time
                                         test.type = "independent")
print(difference_independent)

#----
# Paired test: effect size differences between 2 measurements
data1 <- subset(nca.example2, select = c(1,2)) #select X and Y for T1 
# Simulate changes of X and Y at T2  
set.seed(123)
data2 <- data1
data2$`Contractual detail` <- pmin(5, pmax(1, data2$`Contractual detail`
                                           + sample(-1:1, nrow(data2), TRUE)))
data2$`Innovation` <- pmax(1, ifelse(data2$`Innovation` 
                                     >= 4, data2$`Innovation` 
                                     - 1, data2$`Innovation`))
difference_paired <- nca_difference(data1=data1, 
                                    data2=data2,
                                    x = "Contractual detail",
                                    y = "Innovation",
                                    ceiling = "cr_fdh",
                                    test.rep = 1000,
                                    test.type = "paired")
print(difference_paired)

#----
# Power analysis
power <- nca_power(n = 50, effect = 0.2, ceiling = "cr_fdh") # This may take some time
print(power)

#----
# Random sample generation
random <- nca_random (n = 100, intercepts = 0.2, slopes = 1)
head(random)



