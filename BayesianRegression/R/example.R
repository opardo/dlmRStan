# file_name <- "./data/dataset2.csv"
#
# dataset <- read_csv(file_name)  %>%
#   na.omit %>% # Not NA's needed
#   as_data_frame
#
# dep_variable <-  "AWARENESS"
#
# parallel_fit <- TRUE
# apriori_means <- NULL
# apriori_covariances <- NULL
# apriori_intercept_mean <- NULL
# apriori_intercept_var <- 0.4
# apriori_intercept_upper <- NULL
# sigma_lower <- NULL
# positive_betas <- FALSE
#
# model <- BR_dlm(
#   dataset,
#   dep_variable,
#   parallel_fit,
#   apriori_means,
#   apriori_covariances,
#   apriori_intercept_mean,
#   apriori_intercept_var,
#   apriori_intercept_upper,
#   sigma_lower,
#   positive_betas
# )
#
# model$validation
#
# model$insights$plots
