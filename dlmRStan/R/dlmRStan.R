dlmRStan <- function(
  formula,
  dataset,
  parallel_fit=FALSE,
  apriori_means=NULL,
  apriori_covariances=NULL,
  apriori_intercept_mean=NULL,
  apriori_intercept_var=NULL,
  sigma_lower=NULL,
  intercept_range=c(0, as.numeric(quantile(dataset[[toString(formula[2])]], 0.025))),
  betas_range=c(-10, 10)
){

  if(parallel_fit){
    options(mc.cores = parallel::detectCores())
  }

  dlmRS <- build_dlmRS(
    formula,
    dataset,
    apriori_means,
    apriori_covariances,
    apriori_intercept_mean,
    apriori_intercept_var,
    sigma_lower,
    intercept_range,
    betas_range
  ) %>%
    load_model_data %>%
    load_a_priori %>%
    fit_model %>%
    extract_validation %>%
    get_insights

  return(dlmRS)
}