fit_model <- function(dlmRS){
  dlmRS <- dlmRS %>%
    fit_stan %>%
    extract_parameters %>%
    remove_stan_output
  return(dlmRS)
}

fit_stan <- function(dlmRS){
  if (!dlmRS$input$remove_intercept) {
    stan_input <- list(
      m = dlmRS$data$m,
      p = dlmRS$data$p,
      y = as.numeric(dlmRS$data$Y),
      X = dlmRS$data$X,
      betas_means = dlmRS$a_priori$betas_means,
      betas_covariances = dlmRS$a_priori$betas_covariances,
      intercept_mean = dlmRS$a_priori$intercept_mean,
      intercept_sd = dlmRS$a_priori$intercept_sd,
      betas_lower = dlmRS$input$betas_range[1],
      betas_upper = dlmRS$input$betas_range[2],
      intercept_lower = dlmRS$input$intercept_range[1],
      intercept_upper = dlmRS$input$intercept_range[2],
      sigma_lower = dlmRS$input$sigma_lower
    )
    file <- system.file(
      "models",
      "normal_normal.stan",
      package = "dlmRStan"
    )
    parameters <- c("beta","yhat","intercept","sigma","log_lik")
  } else {
    stan_input <- list(
      m = dlmRS$data$m,
      p = dlmRS$data$p,
      y = as.numeric(dlmRS$data$Y),
      X = dlmRS$data$X,
      betas_means = dlmRS$a_priori$betas_means,
      betas_covariances = dlmRS$a_priori$betas_covariances,
      betas_lower = dlmRS$input$betas_range[1],
      betas_upper = dlmRS$input$betas_range[2],
      sigma_lower = dlmRS$input$sigma_lower
    )
    file <- system.file(
      "models",
      "normal_normal_without_intercept.stan",
      package = "dlmRStan"
    )
    parameters <-  c("beta","yhat","sigma","log_lik")
  }

  dlmRS$fit$fit_stan <- stan(
    file = file,
    data = stan_input,
    pars = parameters,
    chains = dlmRS$input$chains,
    iter = dlmRS$input$iter,
    warmup = dlmRS$input$warmup,
    thin = dlmRS$input$thin
  )
  return(dlmRS)
}

extract_parameters <- function(dlmRS){
  dlmRS$fit$parameters$yhat <- as.numeric(get_stan_parameter(dlmRS$fit$fit_stan,"yhat"))
  dlmRS$fit$parameters$sigma <- get_stan_parameter(dlmRS$fit$fit_stan,"sigma")
  dlmRS$fit$parameters$log_lik <- extract_log_lik(dlmRS$fit$fit_stan)
  dlmRS$fit$parameters$beta <- organize_beta(dlmRS)
  dlmRS$fit$parameters$beta_sd <- organize_beta_sd(dlmRS)
  dlmRS <- extract_contribution(dlmRS)
  return(dlmRS)
}

remove_stan_output <- function(dlmRS){
  dlmRS$fit$fit_stan <- NULL
  return(dlmRS)
}

get_stan_parameter <- function(fit, parameter){
  return(get_posterior_mean(fit, par = parameter)[,'mean-all chains'])
}

organize_beta <- function(dlmRS){
  if(!dlmRS$input$remove_intercept) {
    covariates_beta <- extract_covariates_beta(dlmRS)
    intercept_beta <- data_frame(
      Intercept = get_stan_parameter(dlmRS$fit$fit_stan,"intercept")
    )
    beta <- cbind(intercept_beta, covariates_beta)
  } else {
    beta <- extract_covariates_beta(dlmRS)
  }
  return(beta)
}

organize_beta_sd <- function(dlmRS) {
  if(!dlmRS$input$remove_intercept) {
    covariates_sd <- extract_covariates_sd(dlmRS)
    intercept_sd <- tidy(dlmRS$fit$fit_stan, pars = "intercept") %>%
      select(`std.error`) %>%
      rename(Intercept = `std.error`)
    beta_sd <- cbind(intercept_sd, covariates_sd)
  } else {
    beta_sd <- extract_covariates_sd(dlmRS)
  }
  return(beta_sd)
}

extract_contribution <- function(dlmRS){
  beta <- dlmRS$fit$parameters$beta
  beta_sd <- dlmRS$fit$parameters$beta_sd
  if(!dlmRS$input$remove_intercept) {
    X <- cbind(Intercept = rep(1,nrow(beta)), dlmRS$data$X)
  } else {
    X <- dlmRS$data$X
  }
  dlmRS$fit$parameters$contribution <- as_data_frame(as.matrix(beta) * as.matrix(X))
  dlmRS$fit$parameters$contribution_sd <- as_data_frame(as.matrix(beta_sd) * as.matrix(X))
  return(dlmRS)
}

extract_covariates_beta <- function(dlmRS){
  beta <- get_stan_parameter(dlmRS$fit$fit_stan,"beta") %>%
    as_data_frame %>%
    mutate(coordinates = get_coordinates(row.names(.))) %>%
    separate(coordinates, into=c("observation","variable"), sep=",") %>%
    mutate(observation = as.numeric(observation)) %>%
    spread(variable, value) %>%
    arrange(observation) %>%
    select(-observation)
  colnames(beta) <- colnames(dlmRS$data$X)
  return(beta)
}

extract_covariates_sd <- function(dlmRS) {
  beta_sd <- tidy(dlmRS$fit$fit_stan, pars = "beta") %>%
    select(term, `std.error`) %>%
    mutate(coordinates = get_coordinates(term)) %>%
    select(-term) %>%
    separate(coordinates, into=c("observation","variable"), sep=",") %>%
    mutate(observation = as.numeric(observation)) %>%
    spread(variable, `std.error`) %>%
    arrange(observation) %>%
    select(-observation)
  colnames(beta_sd) <- colnames(dlmRS$data$X)
  return(beta_sd)
}

get_coordinates <- function(string){
  coordinates <- string %>%
    gsub("beta","",.) %>%
    gsub("\\[","",.) %>%
    gsub("\\]","",.)
  return(coordinates)
}
