load_a_priori.BR_model <- function(model){

  model <- model %>%
    calculate_a_priori_parameters

  if(!is.null(model$meta_parameters$apriori_means)){
    model$a_priori$means <- model$meta_parameters$apriori_means
  }
  if(!is.null(model$meta_parameters$apriori_covariances)){
    model$a_priori$covariances <- model$meta_parameters$apriori_covariances
  }
  if(!is.null(model$meta_parameters$apriori_intercept_mean)){
    model$a_priori$intercept_mean <- model$meta_parameters$apriori_intercept_mean
  }
  if(!is.null(model$meta_parameters$apriori_intercept_var)){
    model$a_priori$intercept_var <- model$meta_parameters$apriori_intercept_var
  }
  if(!is.null(model$meta_parameters$apriori_intercept_upper)){
    model$a_priori$intercept_upper <- model$meta_parameters$apriori_intercept_upper
  }
  if(!is.null(model$meta_parameters$sigma_lower)){
    model$a_priori$sigma_lower <- model$meta_parameters$sigma_lower
  }

  return(model)
}

calculate_a_priori_parameters.BR_model <- function(model){

  positive_betas <- model$meta_parameters$positive_betas
  complete_data <- model$raw_data
  first_trend <- get_first_trend(complete_data)
  least_squares_model <- calculate_least_squares_model(first_trend)
  y <- as.numeric(model$model_data$original$Y)

  model$a_priori$means <- a_priori_means(least_squares_model,positive_betas)
  model$a_priori$covariances <- a_priori_covariances(least_squares_model)
  model$a_priori$intercept_mean <- a_priori_intercept_mean(least_squares_model,first_trend,y)
  model$a_priori$intercept_var <- a_priori_intercept_var(least_squares_model,complete_data)
  model$a_priori$intercept_upper <- as.numeric(quantile(y,0.05))
  model$a_priori$sigma_lower <- (1.5*sd(y))^2 # Heuristic

  return(model)
}

get_first_trend <- function(raw_data){
  breakpoints <- breakpoints(
    Y ~ t,
    data = raw_data %>% mutate(t = 1:nrow(.))
  )$breakpoints

  first_trend_length <- max(breakpoints[1],2*ncol(raw_data))
  first_trend <- raw_data[1:first_trend_length,]
  return(first_trend)
}

calculate_least_squares_model <- function(first_trend){
  first_trend_differences <- apply(first_trend,2,column_differences) %>%
    as_data_frame
  least_squares_model <- lm(Y ~ .+0,data=first_trend_differences)
  return(least_squares_model)
}

a_priori_means <- function(least_squares_model,positive_betas){
  if(positive_betas){
    means <- ifelse(
      !is.na(least_squares_model$coefficients) & least_squares_model$coefficients > 0,
      least_squares_model$coefficients,
      0
    )
  } else {
    means <- ifelse(
      !is.na(least_squares_model$coefficients),
      least_squares_model$coefficients,
      0
    )
  }
  return(means)
}

a_priori_covariances <- function(least_squares_model){
  if(all(!is.na(least_squares_model$coefficients)))
    covariances <- vcov(least_squares_model)
  else{
    covariances <- nas_a_priori_covariances(least_squares_model)
  }
  return(covariances)
}

a_priori_intercept_mean <- function(least_squares_model,first_trend,y){
  intercept_mean <- min(
    as.numeric(mean(first_trend$Y - predict(least_squares_model,first_trend))),
    min(y)
  )
  return(intercept_mean)
}

a_priori_intercept_var <- function(least_squares_model,complete_data){
  intercept_var <- as.numeric(sd(
    complete_data$Y - predict(least_squares_model,complete_data)
  ))
  return(intercept_var)
}

nas_a_priori_covariances <- function(least_squares_model){
  not_na_covariances <- vcov(least_squares_model)
  vars_names <- names(least_squares_model$coefficients)
  match_index <- match(vars_names,colnames(not_na_covariances))

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

column_differences <- function(column){
  diff_column <- vector()
  for(i in 1:(length(column)-1)){
    diff_column[i] <- round(column[i+1] - column[i],2)
  }
  return(diff_column)
}