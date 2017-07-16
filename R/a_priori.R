load_a_priori <- function(dlmRS){

  dlmRS <- dlmRS %>%
    calculate_a_priori_parameters

  if(!is.null(dlmRS$input$apriori_betas_means)){
    dlmRS$a_priori$betas_means <- dlmRS$input$apriori_betas_means
  }
  if(!is.null(dlmRS$input$apriori_betas_covariances)){
    dlmRS$a_priori$betas_covariances <- dlmRS$input$apriori_betas_covariances
  }

  if(!dlmRS$input$remove_intercept) {
    if(!is.null(dlmRS$input$apriori_intercept_mean)){
      dlmRS$a_priori$intercept_mean <- dlmRS$input$apriori_intercept_mean
    }
    if(!is.null(dlmRS$input$apriori_intercept_sd)){
      dlmRS$a_priori$intercept_sd <- dlmRS$input$apriori_intercept_sd
    }
  }

  return(dlmRS)
}

calculate_a_priori_parameters <- function(dlmRS){

  formula <- dlmRS$input$formula
  dataset <- dlmRS$input$dataset
  betas_range <- dlmRS$input$betas_range
  remove_intercept <- dlmRS$input$remove_intercept

  if(!remove_intercept) {
    intercept_range <- dlmRS$input$intercept_range
  }

  first_trend <- get_first_trend(formula, dataset)
  least_squares_model <- calculate_least_squares_model(formula, first_trend)
  Y <- as.numeric(dlmRS$data$Y)

  dlmRS$a_priori$betas_means <- a_priori_betas_means(least_squares_model)
  dlmRS$a_priori$betas_covariances <- a_priori_betas_covariances(least_squares_model)

  if(!remove_intercept) {
    dlmRS$a_priori$intercept_mean <- a_priori_intercept_mean(
      formula,
      least_squares_model,
      first_trend
    )
    dlmRS$a_priori$intercept_sd <- a_priori_intercept_sd(
      formula,
      least_squares_model,
      first_trend
    )
  }

  return(dlmRS)
}

get_first_trend <- function(formula, dataset){
  dataset_with_t <- dataset %>% mutate(t = 1:nrow(.))
  y_vs_t_formula <- as.formula(paste(toString(formula[2]),"~ t"))
  breakpoints <- breakpoints(
    formula = y_vs_t_formula,
    data = dataset_with_t
  )$breakpoints
  first_trend_length <- max(breakpoints[1], 2 * ncol(dataset))
  first_trend <- dataset[1:first_trend_length,]
  return(first_trend)
}

calculate_least_squares_model <- function(formula, first_trend){
  first_trend_differences <- first_trend %>%
    as.matrix %>%
    diff %>%
    as.data.frame
  ls_formula <- as.formula(paste(toString(formula[2]),"~ . + 0"))
  least_squares_model <- lm(ls_formula, data = first_trend_differences)
  return(least_squares_model)
}

a_priori_betas_means <- function(least_squares_model){
  means <- ifelse(
    !is.na(least_squares_model$coefficients),
    least_squares_model$coefficients,
    0
  )
  return(means)
}

a_priori_betas_covariances <- function(least_squares_model){
  if(all(!is.na(least_squares_model$coefficients)))
    covariances <- vcov(least_squares_model)
  else{
    covariances <- nas_a_priori_covariances(least_squares_model)
  }
  return(covariances)
}

a_priori_intercept_mean <- function(formula, least_squares_model, first_trend){
  first_trend_y <- first_trend[[toString(formula[2])]]
  first_trend_xTbeta <- as.numeric(predict(least_squares_model, first_trend))
  residuals <- first_trend_y - first_trend_xTbeta
  intercept_mean <- mean(residuals)
  return(intercept_mean)
}

a_priori_intercept_sd <- function(formula, least_squares_model, first_trend){
  first_trend_y <- first_trend[[toString(formula[2])]]
  first_trend_xTbeta <- as.numeric(predict(least_squares_model, first_trend))
  residuals <- first_trend_y - first_trend_xTbeta
  intercept_sd <- sd(residuals)
  return(intercept_sd)
}

nas_a_priori_covariances <- function(least_squares_model){
  not_na_covariances <- vcov(least_squares_model)
  vars_names <- names(least_squares_model$coefficients)
  match_index <- match(vars_names, colnames(not_na_covariances))

  covariances <- matrix(
    nrow = length(vars_names),
    ncol = length(vars_names)
  )

  for(i in 1:length(vars_names)){
    for(j in 1:length(vars_names)){
      if(!is.na(match_index[i]) & !is.na(match_index[j])){
        covariance <- not_na_covariances[match_index[i],match_index[j]]
      } else if (i == j) {
        covariance <- 2 * mean(diag(not_na_covariances))
      } else {
        covariance <- 0
      }
      covariances[i,j] <- covariance
    }
  }

  colnames(covariances) <- vars_names
  rownames(covariances) <- vars_names

  return(covariances)
}