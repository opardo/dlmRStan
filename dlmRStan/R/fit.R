fit_model <- function(dlmRS){
  dlmRS <- dlmRS %>%
    fit_stan %>%
    get_stan_parameters %>%
    get_model_parameters
  return(dlmRS)
}

fit_stan <- function(dlmRS){

  if (!dlmRS$input$remove_intercept) {
    stan_input <- list(
      m = dlmRS$model_data$m,
      p = dlmRS$model_data$p,
      y = as.numeric(dlmRS$model_data$Y),
      X = dlmRS$model_data$X,
      apriori_means = dlmRS$a_priori$means,
      apriori_covariances = dlmRS$a_priori$covariances,
      intercept_mean = dlmRS$a_priori$intercept_mean,
      intercept_var = dlmRS$a_priori$intercept_var,
      betas_lower = dlmRS$meta_parameters$betas_range[1],
      betas_upper = dlmRS$meta_parameters$betas_range[2],
      intercept_lower = dlmRS$meta_parameters$intercept_range[1],
      intercept_upper = dlmRS$meta_parameters$intercept_range[2],
      sigma_lower = dlmRS$a_priori$sigma_lower
    )
    file <- system.file(
      "models",
      "normal_normal.stan",
      package = "dlmRStan"
    )
    dlmRS$fit$fit_stan <- stan(
      file = file,
      data = stan_input,
      pars = c("beta","yhat","intercept","sigma","log_lik")
    )
  } else {
    stan_input <- list(
      m = dlmRS$model_data$m,
      p = dlmRS$model_data$p,
      y = as.numeric(dlmRS$model_data$Y),
      X = dlmRS$model_data$X,
      apriori_means = dlmRS$a_priori$means,
      apriori_covariances = dlmRS$a_priori$covariances,
      betas_lower = dlmRS$meta_parameters$betas_range[1],
      betas_upper = dlmRS$meta_parameters$betas_range[2],
      sigma_lower = dlmRS$a_priori$sigma_lower
    )
    file <- system.file(
      "models",
      "normal_normal_without_intercept.stan",
      package = "dlmRStan"
    )
    dlmRS$fit$fit_stan <- stan(
      file = file,
      data = stan_input,
      pars = c("beta","yhat","sigma","log_lik")
    )
  }
  return(dlmRS)
}

get_stan_parameters <- function(dlmRS){
  dlmRS$fit$stan_parameters$beta <- get_stan_parameter(dlmRS$fit$fit_stan,"beta")
  dlmRS$fit$stan_parameters$yhat <- get_stan_parameter(dlmRS$fit$fit_stan,"yhat")
  dlmRS$fit$stan_parameters$sigma <- get_stan_parameter(dlmRS$fit$fit_stan,"sigma")
  dlmRS$fit$stan_parameters$log_lik <- extract_log_lik(dlmRS$fit$fit_stan)
  if(!dlmRS$input$remove_intercept) {
    dlmRS$fit$stan_parameters$intercept <- get_stan_parameter(dlmRS$fit$fit_stan,"intercept")
  }
  return(dlmRS)
}

get_model_parameters <- function(dlmRS){

  dlmRS <- dlmRS %>%
    extract_beta %>%
    extract_contribution %>%
    extract_yhat

  return(dlmRS)
}

extract_beta <- function(dlmRS){
  covariates_beta <- extract_covariates_beta(dlmRS)
  if(!dlmRS$input$remove_intercept) {
    intercept_beta <- data_frame(
      Intercept = as.numeric(dlmRS$fit$stan_parameters$intercept)
    )
    covariates_beta <- cbind(intercept_beta, covariates_beta)
  }
  dlmRS$fit$parameters$beta <- covariates_beta
  return(dlmRS)
}

extract_contribution <- function(dlmRS){
  beta <- dlmRS$fit$parameters$beta
  if(!dlmRS$input$remove_intercept) {
    X <- cbind(
      Intercept = rep(1,nrow(beta)),
      dlmRS$model_data$X
    )
  } else {
    X <- dlmRS$model_data$X
  }
  dlmRS$fit$parameters$contribution <- as_data_frame(as.matrix(beta) * as.matrix(X))
  return(dlmRS)
}

extract_yhat <- function(dlmRS){
  dlmRS$fit$parameters$yhat <- as.numeric(dlmRS$fit$stan_parameters$yhat)
  return(dlmRS)
}

extract_covariates_beta <- function(dlmRS){
  beta <- dlmRS$fit$stan_parameters$beta %>%
    as_data_frame %>%
    mutate(coordinates = get_coordinates(row.names(.))) %>%
    separate(coordinates, into=c("observation","variable"), sep=",") %>%
    mutate(observation = as.numeric(observation)) %>%
    spread(variable, value) %>%
    arrange(observation) %>%
    select(-observation)
  colnames(beta) <- colnames(dlmRS$model_data$X)
  return(beta)
}

get_coordinates <- function(string){
  coordinates <- string %>%
    gsub("beta","",.) %>%
    gsub("\\[","",.) %>%
    gsub("\\]","",.)
  return(coordinates)
}

get_stan_parameter <- function(fit, parameter){
  return(get_posterior_mean(fit, par = parameter)[,'mean-all chains'])
}
