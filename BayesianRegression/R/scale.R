scale_data.BR_model <- function(model){

  scaled_data <- model$raw_data %>% scale
  stdevs <- attr(scaled_data,'scaled:scale')
  means <- attr(scaled_data,'scaled:center')

  model$scale$scaled_data <- as_data_frame(scaled_data)
  model$scale$stdevs <- stdevs
  model$scale$means <- means

  return(model)
}