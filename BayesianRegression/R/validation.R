extract_validation.BR_model <- function(model){

  y_hat <- as.numeric(model$fit$parameters$yhat)
  y_original <- as.numeric(model$model_data$original$Y)
  log_lik <- model$fit$stan_parameters$log_lik

  model$validation$MSE <- mean((y_hat - y_original) ^ 2)
  model$validation$MAE <- mean(abs(y_hat - y_original))
  model$validation$R2 <- cor(diff(y_hat), diff(y_original)) ^ 2
  model$validation$loo <- loo(log_lik)
  model$validation$waic <- waic(log_lik)

  return(model)
}