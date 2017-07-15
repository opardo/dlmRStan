#' Create BR model from CSV
#'
#' Creates dlmRS object from a dataset saved in a CSV. The CSV must be in the long format.
#'
#' @param The filepath to a dataset
#' @return The imputed dataset
#' @examples
#' model <- build_dlmRS_from_csv("a.csv")
#' @export
# build_dlmRS_from_csv <- function(file_name,dep_variable){
#   return(build_dlmRS(read_model_csv(file_name,dep_variable)))
# }

#' Create BR model from data frame
#'
#' Creates dlmRS object from a dataset in a dataframe. The dataset must be in the long format.
#'
#' @param The filepath to a dataset
#' @return The imputed dataset
#' @examples
#' data(brazilUL)
#' model <- buil_UL_model(brazilUL)
#' @export
build_dlmRS <- function(
  formula,
  dataset,
  apriori_means,
  apriori_covariances,
  apriori_intercept_mean,
  apriori_intercept_var,
  sigma_lower,
  intercept_range,
  betas_range
){

  dlmRS <- structure(list(), class = "dlmRS")

  dlmRS$input <- list()
  dlmRS$input$formula <- formula
  dlmRS$input$dataset <- dataset
  dlmRS$input$remove_intercept <- grepl("+0", gsub(" ", "", toString(formula[3])))

  dlmRS$meta_parameters <- list()
  dlmRS$meta_parameters$apriori_means <- apriori_means
  dlmRS$meta_parameters$apriori_covariances <- apriori_covariances
  dlmRS$meta_parameters$apriori_intercept_mean <- apriori_intercept_mean
  dlmRS$meta_parameters$apriori_intercept_var <- apriori_intercept_var
  dlmRS$meta_parameters$sigma_lower <- sigma_lower
  dlmRS$meta_parameters$intercept_range <- intercept_range
  dlmRS$meta_parameters$betas_range <- betas_range

  dlmRS$model_data <- list()
  dlmRS$a_priori <- list()
  dlmRS$fit <- list()
  dlmRS$validation <- list()
  dlmRS$insights <- list()

  return(dlmRS)
}

#Dispatchers. Please register your functions here.
dispatcher_creator <- function(function_name){
  return(
    function (x, ...) {
      UseMethod(function_name, x)
    }
  )
}

# scale_data <- dispatcher_creator("scale_data")
#
# load_model_data <- dispatcher_creator("load_model_data")
# load_original_X_Y <- dispatcher_creator("load_original_X_Y")
# load_scaled_X_Y <- dispatcher_creator("load_scaled_X_Y")
# load_dimensions <- dispatcher_creator("load_dimensions")
#
# load_a_priori <- dispatcher_creator("load_a_priori")
# calculate_a_priori_parameters <- dispatcher_creator("calculate_a_priori_parameters")
#
# fit_model <- dispatcher_creator("fit_model")
# fit_stan <- dispatcher_creator("fit_stan")
# get_stan_parameters <- dispatcher_creator("get_stan_parameters")
# get_model_parameters <- dispatcher_creator("get_model_parameters")
# extract_beta <- dispatcher_creator("extract_beta")
# extract_contribution <- dispatcher_creator("extract_contribution")
# extract_yhat <- dispatcher_creator("extract_yhat")
# extract_covariates_beta <- dispatcher_creator("extract_covariates_beta")
#
# extract_validation <- dispatcher_creator("extract_validation")
#
# get_insights <- dispatcher_creator("get_insights")
# insights_model_adjustment <- dispatcher_creator("insights_model_adjustment")
# insights_efficiencies <- dispatcher_creator("insights_efficiencies")
# insights_contributions <- dispatcher_creator("insights_contributions")
#
