# Helper function to estimate SEM model for the TAM example
# revised 23 May, 2025

estimate_sem_model_TAM <- function(data){
# Load library for conducting SEM
library(seminr)

# Specify the measurement model
TAM_mm <- constructs(
  composite("Perceived usefulness", multi_items("PU", 1:3)),
  composite("Compatibility", multi_items("CO", 1:3)),
  composite("Perceived ease of use", multi_items("EOU", 1:3)),
  composite("Emotional value", multi_items("EMV", 1:3)),
  composite("Adoption intention", multi_items("AD", 1:3)),
  composite("Technology use", single_item("USE"))
)

# Specify the structural model
TAM_sm <- relationships(
  paths(from = "Perceived usefulness", to = c("Adoption intention", "Technology use")),
  paths(from = "Compatibility",         to = c("Adoption intention", "Technology use")),
  paths(from = "Perceived ease of use", to = c("Adoption intention", "Technology use")),
  paths(from = "Emotional value",       to = c("Adoption intention", "Technology use")),
  paths(from = "Adoption intention",    to = c("Technology use"))
)

# Estimate the pls model (using indicator data)

# Conduct pls estimation with raw indicator scores
TAM_pls <- estimate_pls(data = data,
                        measurement_model = TAM_mm,
                        structural_model = TAM_sm)

# Print path coefficients and model fit measures
summary(TAM_pls)

return (TAM_pls)
}
