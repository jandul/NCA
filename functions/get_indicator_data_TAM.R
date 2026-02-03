# Helper function for getting indicator data of TAM example
#revised May 23, 2025

# Literature:
# Hauff, S., Richter, N. F., Sarstedt, M., & Ringle, C. M. (2024). 
# Importance and performance in PLS-SEM and NCA: Introducing the combined importance-performance map analysis (cIPMA).
# Journal of Retailing and Consumer Services, 78, 103723.
# https://doi.org/10.1016/j.jretconser.2024.103723

# Richter, N. F., Schubring, S., Hauff, S., Ringle, C. M., & Sarstedt, M. (2020). 
# When predictors of outcomes are necessary: Guidelines for the combined use of PLS-SEM and NCA.
# Industrial Management & Data Systems, 120(12), 2243–2267. 
# https://doi.org/10.1108/IMDS-11-2019-0638

# Richter, N. F., Hauff, S., Ringle, C. M., Sarstedt, M., Kolev, A. E., & Schubring, S. (2023). 
# How to apply necessary condition analysis in PLS-SEM. In Partial least squares path modeling: Basic concepts, methodological issues and applications (pp. 267–297). 
# Springer. 
# https://link.springer.com/chapter/10.1007/978-3-031-37772-3_10

# Sarstedt, M., Richter, N. F., Hauff, S., & Ringle, C. M. (2024). 
# Combined importance–performance map analysis (cIPMA) in partial least squares structural equation modeling (PLS–SEM): A SmartPLS 4 tutorial. 
# Journal of Marketing Analytics, 1–15. 
# https://doi.org/10.1057/s41270-024-00325-y

# Data sources: 
# Schubring, S., & Richter, N. (2023). 
# Extended TAM (Version V4). 
# Mendeley Data. 
# https://doi.org/10.17632/pd5dp3phx2.4https://osf.io/35a6d/files/osfstorage?view_only=849040255d43480696cd60a43d664660

# Richter, N. F., Hauff, S., Kolev, A. E., & Schubring, S. (2023).
# Dataset on an extended technology acceptance model: A combined application of PLS-SEM and NCA. 
# Data in Brief, 48, 109190. 
# https://doi.org/10.1016/j.dib.2023.109190


get_indicator_data_TAM <- function(){

# Load the data  
df0 <- read.csv("Extended TAM.csv")

# Preview the data
head(df0, 3)

# Make a new dataset with only indicator scores of SEM model (without last four variables)
df1 <- df0[, 1:(ncol(df0) - 4)]

# Rename the indicators 
colnames(df1) <- c("PU1","PU2","PU3",
                   "CO1","CO2","CO3",
                   "EOU1", "EOU2","EOU3",
                   "EMV1", "EMV2","EMV3",
                   "AD1",  "AD2","AD3",
                   "USE")

# Preview the new dataset
head(df1,3)

return(df1)
}
