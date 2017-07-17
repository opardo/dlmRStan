# Installation
install.packages("devtools")
library(devtools)
install_github("opardo/dlmRStan")

# Load package
library(dlmRStan)

# Load data (Market Research)
data("dlmRStan3")
dataset <- dlmRStan3
formula <- awareness ~ .

# Fit model
model <- dlmRStan(
  formula = formula,
  dataset = dataset,
  betas_range = c(0, 0.07),
  intercept_range = c(30,50),
  chains = 4,
  iter = 1000,
  warmup = 500
)

# Fitting validation
model$validation

# Show insights
model$insights$plots

# Parameters' errors
model$parameters$betas_sd
model$parameters$contribution_sd
