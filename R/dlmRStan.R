dlmRStan <- function(
  formula,
  dataset,
  betas_range=c(-10, 10),
  intercept_range=c(0, as.numeric(quantile(dataset[[toString(formula[2])]], 0.025))),
  apriori_betas_means=NULL,
  apriori_betas_covariances=NULL,
  apriori_intercept_mean=NULL,
  apriori_intercept_sd=NULL,
  sigma_lower=0.8 * sd(dataset[[toString(formula[2])]]),
  chains=4,
  iter=11e3,
  warmup=1e3,
  thin=5,
  parallel_fit=TRUE
){

  if (parallel_fit) {
    options(mc.cores = parallel::detectCores())
  }

  dlmRS <- build_dlmRS(
    formula,
    dataset,
    betas_range,
    intercept_range,
    apriori_betas_means,
    apriori_betas_covariances,
    apriori_intercept_mean,
    apriori_intercept_sd,
    sigma_lower,
    chains,
    iter,
    warmup,
    thin
  ) %>%
    load_data %>%
    load_a_priori %>%
    fit_model %>%
    extract_validation %>%
    get_insights

  return(dlmRS)
}
