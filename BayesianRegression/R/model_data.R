load_model_data.BR_model <- function(model){
  model <- model %>%
    load_original_X_Y %>%
    load_scaled_X_Y %>%
    load_dimensions
  return(model)
}

load_original_X_Y.BR_model <- function(model){
  model$model_data$original$X <- model$raw_data %>%
    select(-Y) %>%
    as.matrix
  model$model_data$original$Y <- model$raw_data %>%
    select(Y) %>%
    as.matrix
  return(model)
}

load_scaled_X_Y.BR_model <- function(model){
  model$model_data$scaled$X <- model$scale$scaled_data %>%
    select(-Y) %>%
    as.matrix
  model$model_data$scaled$Y <- model$scale$scaled_data %>%
    select(Y) %>%
    as.matrix
  return(model)
}

load_dimensions.BR_model <- function(model){
  model$model_data$m <- nrow(model$model_data$scaled$X)
  model$model_data$p <- ncol(model$model_data$scaled$X)
  return(model)
}