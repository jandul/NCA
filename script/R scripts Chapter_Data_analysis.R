# R scripts Chapter 'Data analysis' in
# Dul, J. (2026). Necessary Condition Analysis - NCA. Principles and Application. 
# Chapman & Hall/CRC Press.
# February 2, 2026

# To reduce computation time test.rep = 1000
# For final analysis use 10000

#----
# Load the NCA package and the example
library(NCA)
data(nca.example)
# Run the NCA analysis
model <- nca_analysis(nca.example, 1, 3, ceilings = "cr_fdh")
nca_output(model, summaries = FALSE)

#----
library(NCA)
data(nca.example)
# Run the NCA analysis with the specified ceiling line
model <- nca_analysis(nca.example, 1, 3, ceilings = "cr_fdh")
# Extract intercept & slope
Intercept <- nca_extract(model = model, x = "Individualism", ceiling = "cr_fdh",
                         param = "Intercept")
Slope <- nca_extract(model = model, x = "Individualism", ceiling = "cr_fdh",
                     param = "Slope")
# Print the ceiling line equation
cat(
  "Ceiling line equation (CR-FDH): Y =",
  round(Intercept, 3), "+",
  round(Slope, 3), "* X", "\n"
)

#----
library(NCA)
data(nca.example)
# Run the NCA analysis with the specified ceiling line
# 10,000 repetitions for estimating the p-value
set.seed(123) # for reproducibility of the estimated p-value
model <- nca_analysis(nca.example, 1, 3, ceilings = "cr_fdh", test.rep = 13)
## Do test for : cr_fdh - IndividualismDone test for: cr_fdh - Individualism
# Show the results
print(model)

#----
library(NCA)
data(nca.example)
# Run the NCA outlier analysis
outliers <- nca_outliers (nca.example, 1, 3, ceiling = 'cr_fdh')
# Show the results
print(outliers)

#----
library(NCA)
data(nca.example)
nca_outliers (nca.example, 1, 3, ceiling = 'cr_fdh' , plotly = TRUE)

#----
library(NCA)
data(nca.example)
outliers <- nca_outliers (nca.example, 1, 3, ceiling = 'cr_fdh', k = 2,
                          condensed = TRUE)
print(outliers)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,] #exclude Japan
set.seed(123) # for replicability of estimated p-value
modelN <- nca_analysis(nca.exampleN, 1,3, ceilings = 'cr_fdh', test.rep = 1000)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,] #exclude Japan
set.seed(123) # for replicability of estimated p-value
modelN <- nca_analysis(nca.exampleN, 1,3, ceilings = 'cr_fdh', test.rep = 1000)
nca_output(modelN, plots = FALSE, summaries = TRUE)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
set.seed(123)
modelFINAL <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp', test.rep = 1000)
nca_output(modelFINAL, summaries = TRUE, plots)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
modelFINAL <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp')
nca_output (modelFINAL, summaries = FALSE, plots = FALSE,
            bottlenecks = TRUE)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
modelFINAL <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp',
                           step.size = 5)
nca_output (modelFINAL, summaries = FALSE, plots = FALSE,
            bottlenecks = TRUE)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
modelFINAL <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp',
                           bottleneck.y = "actual",
                           bottleneck.x = "actual")
nca_output (modelFINAL, plots = FALSE, summaries = FALSE,
            bottlenecks = TRUE)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
min_y <- min(nca.exampleN$`Innovation performance`)
max_y <- max(nca.exampleN$`Innovation performance`)
modelFINAL <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp',
                           bottleneck.y = "actual",
                           bottleneck.x = "actual",
                           steps = c(min_y, seq(10, 210, by = 10),max_y))
nca_output (modelFINAL, plots = FALSE, summaries = FALSE,
            bottlenecks = TRUE)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
modelFINAL <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp',
                           bottleneck.y = "percentile",
                           bottleneck.x = "actual",
                           steps = c(0, 90))
nca_output (modelFINAL, plots = FALSE, summaries = FALSE,
            bottlenecks = TRUE)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
min_y <- min(nca.exampleN$`Innovation performance`)
max_y <- max(nca.exampleN$`Innovation performance`)
modelFINAL <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp',
                           bottleneck.y = "actual",
                           bottleneck.x = "percentile")
nca_output (modelFINAL, plots = FALSE, summaries = FALSE,
            bottlenecks = TRUE)

#----
data(nca.example)
nca.exampleN <- nca.example[-14,]
set.seed(123)
modelFINAL <- nca_analysis(nca.exampleN,
                           c(1,2),3,
                           ceilings = 'c_lp', test.rep = 13,
                           bottleneck.y = "actual",
                           bottleneck.x = "percentile",
                           steps = c(min_y, seq(10, 210, by = 10),max_y))
print(modelFINAL)
nca_output (modelFINAL, summaries = FALSE,
            bottlenecks = TRUE)


#----
nca.exampleN <- nca.example[-14,]
#ceiling line change
set.seed(123)
modelN2 <- nca_analysis(nca.exampleN, 1,3, ceilings = 'ce_fdh', test.rep = 13)
modelN2
nca_output(modelN2, summaries = FALSE)

#----
library (NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
set.seed(123)
modelN3 <- nca_analysis(nca.exampleN, 1,3, ceilings = 'c_lp', test.rep = 13,
                        scope = c(6,91,0,258))
modelN3

#----
library (NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
#outlier change 1 (include the earlier removed outlier)
set.seed(123)
modelN4 <- nca_analysis(nca.example, 1,3, ceilings = 'c_lp', test.rep = 13)
modelN4

#----
nca_output(modelN4, summaries = FALSE)
modelN4b <- nca_analysis(nca.example, 1,3, ceilings = "c_lp",
                         bottleneck.y = 'percentile',
                         bottleneck.x = 'percentile',
                         steps = c(85,90,95))
nca_output(modelN4b, summaries = FALSE, bottlenecks = T)

#outlier change 2 (remove an additional outlier)
nca_outliers(nca.example, 1,3, ceiling = 'c_lp', k = 2, condensed = TRUE) #Japan and Finland


#----
nca.exampleN2 <- nca.example[-c(7,14),]
set.seed(123)
modelN5 <- nca_analysis(nca.exampleN2, 1,3, ceilings = 'c_lp', test.rep = 13)
modelN5
nca_output(modelN5, summaries = FALSE)

modelN5b <- nca_analysis(nca.exampleN2, 1,3, ceilings = "c_lp",
                         bottleneck.y = 'percentile',
                         bottleneck.x = 'percentile',
                         steps = c(85,90,95))
nca_output(modelN5b, summaries = FALSE, bottlenecks = T)

#----
library(NCA)
data(nca.example)
nca.exampleN <- nca.example[-14,]
modelN <- nca_analysis(nca.exampleN, 1,3, ceilings = "c_lp",
                       bottleneck.y = 'percentile',
                       bottleneck.x = 'percentile',
                       steps = c(85,90,95))
nca_output (modelN, plots = FALSE, summaries = FALSE, bottlenecks = TRUE)



