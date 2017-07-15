load_model_data <- function(dlmRS){
  dlmRS <- dlmRS %>%
    load_X_Y %>%
    load_dimensions
  return(dlmRS)
}

load_X_Y <- function(dlmRS){
  formula <- delete_intercept(dlmRS$input$formula)
  datset <- dlmRS$input$dataset
  mf <- model.frame(formula = formula, data = dataset)
  dlmRS$model_data$X <- model.matrix(attr(mf, "terms"), data = mf)
  dlmRS$model_data$Y <- model.response(data = mf)
  return(dlmRS)
}

load_dimensions <- function(dlmRS){
  dlmRS$model_data$m <- nrow(dlmRS$model_data$X)
  dlmRS$model_data$p <- ncol(dlmRS$model_data$X)
  return(dlmRS)
}

delete_intercept <- function(formula) {
  non_intercept_in_formula <- grepl("+0", gsub(" ", "", toString(formula[3])))
  if(!non_intercept_in_formula) {
    formula <- as.formula(paste(
      toString(formula[2]),
      "~",
      toString(formula[3]),
      "+0"
    ))
  }
  return(formula)
}