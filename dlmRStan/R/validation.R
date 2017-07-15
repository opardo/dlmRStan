extract_validation <- function(dlmRS){

  y_hat <- as.numeric(dlmRS$fit$parameters$yhat)
  y_original <- as.numeric(dlmRS$model_data$Y)
  log_lik <- dlmRS$fit$stan_parameters$log_lik

  dlmRS$validation$MSE <- mean((y_hat - y_original) ^ 2)
  dlmRS$validation$MAE <- mean(abs(y_hat - y_original))
  dlmRS$validation$R2 <- cor(diff(y_hat), diff(y_original)) ^ 2
  dlmRS$validation$loo <- loo(log_lik)
  dlmRS$validation$waic <- waic(log_lik)

  return(dlmRS)
}