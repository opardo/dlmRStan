BR_dlm <- function(
  dataset, dep_variable,
  parallel_fit = FALSE,
  apriori_means=NULL, apriori_covariances=NULL,
  apriori_intercept_mean=NULL, apriori_intercept_var=NULL,
  apriori_intercept_upper=NULL, sigma_lower=NULL,
  positive_betas=FALSE
){

  if(parallel_fit){
    options(mc.cores = parallel::detectCores())
  }

  model <- build_BR_model(
      dataset,dep_variable,
      parallel_fit,
      apriori_means, apriori_covariances,
      apriori_intercept_mean, apriori_intercept_var,
      apriori_intercept_upper, sigma_lower,
      positive_betas
    ) %>%
    scale_data %>%
    load_model_data %>%
    load_a_priori %>%
    fit_model %>%
    extract_validation %>%
    get_insights

  return(model)
}