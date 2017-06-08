fit_model.BR_model <- function(model){
  model <- model %>%
    fit_stan %>%
    get_stan_parameters %>%
    get_model_parameters
  return(model)
}

fit_stan.BR_model <- function(model){
  data <- list(
    m = model$model_data$m,
    p = model$model_data$p,
    y = as.numeric(model$model_data$original$Y),
    X = model$model_data$original$X,
    apriori_means = model$a_priori$means,
    apriori_covariances = model$a_priori$covariances,
    intercept_mean = model$a_priori$intercept_mean,
    intercept_var = model$a_priori$intercept_var,
    intercept_upper = model$a_priori$intercept_upper,
    sigma_lower = model$a_priori$sigma_lower
  )

  if(model$meta_parameters$positive_betas){
    file <- system.file("models", "dynamic_normal_normal_positive.stan", package = "BayesianRegression")
  } else {
    file <- system.file("models", "dynamic_normal_normal_free.stan", package = "BayesianRegression")
  }

  model$fit$fit_stan <- stan(
    file = file,
    data = data,
    pars = c("beta","yhat","intercept","sigma","log_lik")
  )
  return(model)
}

get_stan_parameters.BR_model <- function(model){
  model$fit$stan_parameters$beta <- get_stan_parameter(model$fit$fit_stan,"beta")
  model$fit$stan_parameters$yhat <- get_stan_parameter(model$fit$fit_stan,"yhat")
  model$fit$stan_parameters$intercept <- get_stan_parameter(model$fit$fit_stan,"intercept")
  model$fit$stan_parameters$sigma <- get_stan_parameter(model$fit$fit_stan,"sigma")
  model$fit$stan_parameters$log_lik <- extract_log_lik(model$fit$fit_stan)
  return(model)
}

get_model_parameters.BR_model <- function(model){

  model <- model %>%
    extract_beta %>%
    extract_contribution %>%
    extract_yhat

  return(model)
}

extract_beta.BR_model <- function(model){

  covariates_beta <- extract_covariates_beta(model)
  intercept_beta <- data_frame(
    Intercept = as.numeric(model$fit$stan_parameters$intercept)
  )

  model$fit$parameters$beta <- cbind(intercept_beta, covariates_beta)

  return(model)
}

extract_contribution.BR_model <- function(model){

  beta <- model$fit$parameters$beta
  X <- cbind(
    Intercept = rep(1,nrow(beta)),
    model$model_data$original$X
  )

  model$fit$parameters$contribution <- as_data_frame(as.matrix(beta) * as.matrix(X))

  return(model)
}

extract_yhat.BR_model <- function(model){
  model$fit$parameters$yhat <- as.numeric(model$fit$stan_parameters$yhat)
  return(model)
}

extract_covariates_beta.BR_model <- function(model){
  beta <- model$fit$stan_parameters$beta %>%
    as_data_frame %>%
    mutate(coordinates = get_coordinates(row.names(.))) %>%
    separate(coordinates, into=c("observation","variable"), sep=",") %>%
    mutate(observation = as.numeric(observation)) %>%
    spread(variable, value) %>%
    arrange(observation) %>%
    select(-observation)
  colnames(beta) <- colnames(model$model_data$scaled$X)
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
