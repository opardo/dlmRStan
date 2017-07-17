data("dlmRStan3")
dataset <- dlmRStan3

formula <- awareness ~ .

model <- dlmRStan(
  formula = awareness ~ .,
  dataset = dlmRStan3,
  betas_range = c(0, 0.07),
  intercept_range = c(30,50),
  chains = 4,
  iter = 1000,
  warmup = 500
)

model$validation

model$insights$plots
