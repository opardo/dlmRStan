get_insights <- function(dlmRS){

  dlmRS <- dlmRS %>%
    insights_model_adjustment %>%
    insights_efficiencies %>%
    insights_contributions

  return(dlmRS)
}

insights_model_adjustment <- function(dlmRS){

  if(!dlmRS$input$remove_intercept) {
    intercept <- dlmRS$fit$parameters$beta$Intercept
    y_hat <- as.numeric(dlmRS$fit$parameters$yhat)
    y_original <- as.numeric(dlmRS$data$Y)
    model_adjustment <- data_frame(
      intercept = intercept,
      y_hat = y_hat,
      y_original = y_original
    )
  } else {
    y_hat <- as.numeric(dlmRS$fit$parameters$yhat)
    y_original <- as.numeric(dlmRS$data$Y)
    model_adjustment <- data_frame(
      y_hat = y_hat,
      y_original = y_original
    )
  }

  dlmRS$insights$tables$model_adjustment <- model_adjustment

  model_adjustment <- model_adjustment %>%
    mutate(date = 1:length(y_original)) %>%
    gather(y,variable,-date)

  dlmRS$insights$plots$model_adjustment <- ggplot(
    data = model_adjustment,
    aes(x = date, y = variable, color = y)
  ) +
    geom_line()

  return(dlmRS)
}

insights_efficiencies <- function(dlmRS){

  if(!dlmRS$input$remove_intercept) {
    efficiencies <- dlmRS$fit$parameters$beta %>%
      select(-Intercept)
  } else {
    efficiencies <- dlmRS$fit$parameters$beta
  }

  dlmRS$insights$tables$efficiencies <- efficiencies

  efficiencies <- efficiencies %>%
    mutate(date = 1:nrow(.)) %>%
    gather(covariate,efficiency,-date) %>%
    mutate(covariate = factor(covariate, levels = rev(unique(covariate))))

  dlmRS$insights$plots$efficiencies <- ggplot(
    data = efficiencies,
    aes(x = date, y = efficiency, color = covariate)
  ) +
    geom_line(linetype = 1) +
    scale_color_brewer(palette = "Dark2") +
    geom_hline(yintercept = 0, color = "gray")

  return(dlmRS)
}

insights_contributions <- function(dlmRS){

  contributions <- dlmRS$fit$parameters$contribution
  dlmRS$insights$tables$contributions <- contributions

  contributions <- contributions %>%
    mutate(date = 1:nrow(.)) %>%
    gather(covariate,contribution,-date) %>%
    mutate(covariate = factor(covariate, levels = rev(unique(covariate))))

  dlmRS$insights$plots$contributions <- ggplot(
    data = contributions,
    aes(x = date, y = contribution, fill = covariate)
  ) +
    geom_area(position="stack") +
    scale_fill_brewer(palette = "Dark2")

  return(dlmRS)
}
