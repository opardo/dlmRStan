build_dlmRS <- function(
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
){

  dlmRS <- structure(list(), class = "dlmRS")

  dlmRS$input <- list()
  dlmRS$input$formula <- formula
  dlmRS$input$dataset <- dataset
  dlmRS$input$remove_intercept <- grepl("+0", gsub(" ", "", toString(formula[3])))
  dlmRS$input$betas_range <- betas_range
  dlmRS$input$apriori_betas_means <- apriori_betas_means
  dlmRS$input$apriori_betas_covariances <- apriori_betas_covariances
  dlmRS$input$sigma_lower <- sigma_lower
  dlmRS$input$chains <- chains
  dlmRS$input$iter <- iter
  dlmRS$input$warmup <- warmup
  dlmRS$input$thin <- thin

  if(!dlmRS$input$remove_intercept) {
    dlmRS$input$intercept_range <- intercept_range
    dlmRS$input$apriori_intercept_mean <- apriori_intercept_mean
    dlmRS$input$apriori_intercept_sd <- apriori_intercept_sd
  }

  dlmRS$data <- list()
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
