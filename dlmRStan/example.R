data("dlmRStan2")

dataset <- brdataset2

formula <- AWARENESS ~ .

parallel_fit <- TRUE
apriori_means <- NULL
apriori_covariances <- NULL
apriori_intercept_mean <- NULL
apriori_intercept_var <- 0.4
sigma_lower <- 5
intercept_range <- c(0, as.numeric(quantile(dataset[[toString(formula[2])]], 0.025)))
betas_range <- c(-1, 1)

model <- dlmRStan(
  formula,
  dataset,
  parallel_fit,
  apriori_means,
  apriori_covariances,
  apriori_intercept_mean,
  apriori_intercept_var,
  sigma_lower,
  intercept_range,
  betas_range
)

model$validation

model$insights$plots
