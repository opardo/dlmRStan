# data("dlmRStan1")
# dataset <- dlmRStan1

local_path <- "C:/Users/MB75168/Documents/Projects/Personal/"
setwd(paste0(local_path,"Thesis/Applications/MarketResearch/"))
library(readr)
dataset <- read_csv("brand_awareness_investment.csv") %>%
  select(-weeks)

formula <- awareness ~ .

betas_range <- c(-0.07, 0.07)
intercept_range <- c(30,50)

apriori_betas_means <- NULL
apriori_betas_covariances <- NULL
apriori_intercept_mean <- NULL
apriori_intercept_sd <- NULL
sigma_lower <- 0.8 * sd(dataset[[toString(formula[2])]])
chains <- 4
iter <- 1e3
warmup <- 2e2
thin <- 1
parallel_fit <- TRUE

model <- dlmRStan(
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
  thin,
  parallel_fit
)

model$validation

model$insights$plots
