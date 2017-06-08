get_insights.BR_model <- function(model){

  model <- model %>%
    insights_model_adjustment %>%
    insights_efficiencies %>%
    insights_contributions

  return(model)
}

insights_model_adjustment.BR_model <- function(model){

  intercept <- model$fit$parameters$beta$Intercept
  y_hat <- as.numeric(model$fit$parameters$yhat)
  y_original <- as.numeric(model$model_data$original$Y)

  model_adjustment <- data_frame(
    date = 1:length(y_original),
    intercept = intercept,
    y_hat = y_hat,
    y_original = y_original
  )

  model$insights$tables$model_adjustment <- model_adjustment

  model_adjustment <- model_adjustment %>%
    gather(y,variable,-date)

  model$insights$plots$model_adjustment <- ggplot(
    data = model_adjustment,
    aes(x = date, y = variable, color = y)
  ) +
    geom_line()

  return(model)
}

insights_efficiencies.BR_model <- function(model){

  efficiencies <- model$fit$parameters$beta %>%
    select(-Intercept) %>%
    mutate(date = 1:nrow(.))

  model$insights$tables$efficiencies <- efficiencies

  efficiencies <- efficiencies %>%
    gather(covariate,efficiency,-date) %>%
    mutate(covariate = factor(covariate, levels = rev(unique(covariate))))

  model$insights$plots$efficiencies <- ggplot(
    data = efficiencies,
    aes(x = date, y = efficiency, color = covariate)
  ) +
    geom_line(linetype = 1) +
    scale_color_brewer(palette = "Dark2") +
    geom_hline(yintercept = 0, color = "gray")

  return(model)
}

insights_contributions.BR_model <- function(model){

  contributions <- model$fit$parameters$contribution %>%
    mutate(date = 1:nrow(.))

  model$insights$tables$contributions <- contributions

  contributions <- contributions %>%
    gather(covariate,contribution,-date) %>%
    mutate(covariate = factor(covariate, levels = rev(unique(covariate))))

  model$insights$plots$contributions <- ggplot(
    data = contributions,
    aes(x = date, y = contribution, fill = covariate)
  ) +
    geom_area(position="stack") +
    scale_fill_brewer(palette = "Dark2")

  return(model)
}
