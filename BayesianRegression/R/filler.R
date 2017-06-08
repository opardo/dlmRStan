#' Create BR model from CSV
#'
#' Creates BR_model object from a dataset saved in a CSV. The CSV must be in the long format.
#'
#' @param The filepath to a dataset
#' @return The imputed dataset
#' @examples
#' model <- build_BR_model_from_csv("a.csv")
#' @export
# build_BR_model_from_csv <- function(file_name,dep_variable){
#   return(build_BR_model(read_model_csv(file_name,dep_variable)))
# }

#' Create BR model from data frame
#'
#' Creates BR_model object from a dataset in a dataframe. The dataset must be in the long format.
#'
#' @param The filepath to a dataset
#' @return The imputed dataset
#' @examples
#' data(brazilUL)
#' model <- buil_UL_model(brazilUL)
#' @export
build_BR_model <- function(
  dataset,dep_variable,
  parallel_fit=FALSE,
  apriori_means=NULL, apriori_covariances=NULL,
  apriori_intercept_mean=NULL, apriori_intercept_var=NULL,
  apriori_intercept_upper=NULL, sigma_lower=NULL,
  positive_betas=FALSE
){
  colnames(dataset)[which(colnames(dataset)==dep_variable)] <- "Y"
  BR_model <- structure(list(), class = "BR_model")
  BR_model$raw_data <- dataset
  BR_model$scale <- list()
  BR_model$model_data <- list()
  BR_model$a_priori <- list()
  BR_model$fit <- list()
  BR_model$validation <- list()
  BR_model$insights <- list()

  BR_model$meta_parameters <- list()
  BR_model$meta_parameters$parallel_fit <- parallel_fit
  BR_model$meta_parameters$apriori_means <- apriori_means
  BR_model$meta_parameters$apriori_covariances <- apriori_covariances
  BR_model$meta_parameters$apriori_intercept_mean <- apriori_intercept_mean
  BR_model$meta_parameters$apriori_intercept_var <- apriori_intercept_var
  BR_model$meta_parameters$apriori_intercept_upper <- apriori_intercept_upper
  BR_model$meta_parameters$sigma_lower <- sigma_lower
  BR_model$meta_parameters$positive_betas <- positive_betas

  return(BR_model)
}

#Dispatchers. Please register your functions here.
dispatcher_creator <- function(function_name){
  return(
    function (x, ...) {
      UseMethod(function_name, x)
    }
  )
}

scale_data <- dispatcher_creator("scale_data")

load_model_data <- dispatcher_creator("load_model_data")
load_original_X_Y <- dispatcher_creator("load_original_X_Y")
load_scaled_X_Y <- dispatcher_creator("load_scaled_X_Y")
load_dimensions <- dispatcher_creator("load_dimensions")

load_a_priori <- dispatcher_creator("load_a_priori")
calculate_a_priori_parameters <- dispatcher_creator("calculate_a_priori_parameters")

fit_model <- dispatcher_creator("fit_model")
fit_stan <- dispatcher_creator("fit_stan")
get_stan_parameters <- dispatcher_creator("get_stan_parameters")
get_model_parameters <- dispatcher_creator("get_model_parameters")
extract_beta <- dispatcher_creator("extract_beta")
extract_contribution <- dispatcher_creator("extract_contribution")
extract_yhat <- dispatcher_creator("extract_yhat")
extract_covariates_beta <- dispatcher_creator("extract_covariates_beta")

extract_validation <- dispatcher_creator("extract_validation")

get_insights <- dispatcher_creator("get_insights")
insights_model_adjustment <- dispatcher_creator("insights_model_adjustment")
insights_efficiencies <- dispatcher_creator("insights_efficiencies")
insights_contributions <- dispatcher_creator("insights_contributions")

