# Load required libraries
library(caret)
library(rfCountData)
library(Metrics)

# Read in the data
data <- read.csv('data_poisson_final_male.csv')
fip <- read.csv('fips.csv')

# This function splits the main data based on given FIPS county codes.
# Essentially, for each group of FIPS in 'fip_sample', it subsets the data.
split_data <- function(data, fip_sample) {
    df_list <- lapply(fip_sample, function(fip_group) {
        subset(data, fips %in% fip_group)
    })
    return(df_list)
}

# This function trains random forests and Poisson regression models using 'rr' (radon and radnet) as predictors
# and evaluates their performance using root mean square error (RMSE) and mean absolute percentage error (MAPE).
evaluate_models_rr <- function(train_data, test_data) {
    # Extract training data columns
  x_train <- train_data[, c(4, 8, 14)]
  y_train <- train_data[, 5]
  off_train <- log(train_data[, 6])

    # Extract test data columns
  x_test <- test_data[, c(4, 8, 14)]
  y_test <- test_data[, 5]
  off_test <- log(test_data[, 6])

    # Train random forest
  run <- rfPoisson(x = x_train, offset = off_train, y = y_train)
  y_predict <- predict(run, newdata = x_test, offset = off_test)

    # Train Poisson regression model
  run2 <- glm(Count ~ cdc_radon_median + RadNet + Age_recode_with_1_year_olds, 
              data = train_data, family = "poisson", offset = log(Pop))
  y_predict2 <- predict(run2, test_data)

    # Return evaluation metrics for both models
  return(list(
    rf_rmse = rmse(y_predict, y_test),
    poi_rmse = rmse(y_predict2, y_test),
    rf_mape = mape(y_predict, y_test),
    poi_mape = mape(y_predict2, y_test)
  ))
}


# Similar function as above but now for 'rrsp' predictors (radon, radnet, smoking, pm2.5)
evaluate_models_rrsp <- function(train_data, test_data) {
  x_train <- train_data[, c(4, 7, 8, 14, 20)]
  y_train <- train_data[, 5]
  off_train <- log(train_data[, 6])
  
  x_test <- test_data[, c(4, 7, 8, 14, 20)]
  y_test <- test_data[, 5]
  off_test <- log(test_data[, 6])
  
  run <- rfPoisson(x = x_train, offset = off_train, y = y_train)
  y_predict <- predict(run, newdata = x_test, offset = off_test)
  
  run2 <- glm(Count ~ smoking_10+drinking_10+cdc_radon_median+RadNet+pm2.5+Age_recode_with_1_year_olds, 
              data = train_data, family = "poisson", offset = log(Pop))
  y_predict2 <- predict(run2, test_data)
  
  return(list(
    rf_rmse = rmse(y_predict, y_test),
    poi_rmse = rmse(y_predict2, y_test),
    rf_mape = mape(y_predict, y_test),
    poi_mape = mape(y_predict2, y_test)
  ))
}


# Similar function as above but now for 'ac' predictors. (all of the variables except for sociodemographic variables)
evaluate_models_ac <- function(train_data, test_data) {
  x_train <- train_data[, c(4,7,8,9,14,15,16,17,18,20,21)]
  y_train <- train_data[, 5]
  off_train <- log(train_data[, 6])
  
  x_test <- test_data[, c(4,7,8,9,14,15,16,17,18,20,21)]
  y_test <- test_data[, 5]
  off_test <- log(test_data[, 6])
  
  run <- rfPoisson(x = x_train, offset = off_train, y = y_train)
  y_predict <- predict(run, newdata = x_test, offset = off_test)
  
  run2 <- glm(Count ~ smoking_10+drinking_10+cdc_radon_median+RadNet+ozone+pm2.5+cdc_aq_tox_butadiene_2011+cdc_aq_tox_acetaldehyde_2011+cdc_aq_tox_formaldehyde_2011+cdc_aq_tox_benzene_2011+Age_recode_with_1_year_olds, 
              data = train_data, family = "poisson", offset = log(Pop))
  y_predict2 <- predict(run2, test_data)
  
  return(list(
    rf_rmse = rmse(y_predict, y_test),
    poi_rmse = rmse(y_predict2, y_test),
    rf_mape = mape(y_predict, y_test),
    poi_mape = mape(y_predict2, y_test)
  ))
}


# Similar function as above but now for 'all' predictors.
evaluate_models_all <- function(train_data, test_data) {
  x_train <- train_data[, c(4,7,8,9,10,11,12,13,14,15,16,17,18,20,21)]
  y_train <- train_data[, 5]
  off_train <- log(train_data[, 6])
  
  x_test <- test_data[, c(4,7,8,9,10,11,12,13,14,15,16,17,18,20,21)]
  y_test <- test_data[, 5]
  off_test <- log(test_data[, 6])
  
  run <- rfPoisson(x = x_train, offset = off_train, y = y_train)
  y_predict <- predict(run, newdata = x_test, offset = off_test)
  
  run2 <- glm(Count ~ High_school_education+Median_family_income+Unemployed+Urban+smoking_10+drinking_10+cdc_radon_median+RadNet+ozone+pm2.5+cdc_aq_tox_butadiene_2011+cdc_aq_tox_acetaldehyde_2011+cdc_aq_tox_formaldehyde_2011+cdc_aq_tox_benzene_2011+Age_recode_with_1_year_olds, 
              data = train_data, family = "poisson", offset = log(Pop))
  y_predict2 <- predict(run2, test_data)
  
  return(list(
    rf_rmse = rmse(y_predict, y_test),
    poi_rmse = rmse(y_predict2, y_test),
    rf_mape = mape(y_predict, y_test),
    poi_mape = mape(y_predict2, y_test)
  ))
}


# Initialize result storage vectors for RMSE and MAPE of random forest and Poisson models for 'rr' predictors.

results_rf_rr <- vector('double', 5 * 5)
results_poi_rr <- vector('double', 5 * 5)
results_rf_rr_2 <- vector('double', 5 * 5)
results_poi_rr_2 <- vector('double', 5 * 5)

# The following loops perform 5-fold cross-validation, repeated 5 times.
# For each iteration, the data is split into training and test sets based on FIPS codes, 
# and then models are trained and evaluated.

# Loop for 'rr' predictors
for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)

    # Loop over folds
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])

      # Evaluate models using 'rr' predictors
    results <- evaluate_models_rr(train_data, test_data)
    index <- 5 * (j - 1) + k

      # Store results
    results_rf_rr[index] <- results$rf_rmse
    results_poi_rr[index] <- results$poi_rmse
    results_rf_rr_2[index] <- results$rf_mape
    results_poi_rr_2[index] <- results$poi_mape
  }
}

# Initialize result storage vectors for RMSE and MAPE of random forest and Poisson models for 'rrsp' predictors.
results_rf_rrsp <- vector('double', 5 * 5)
results_poi_rrsp <- vector('double', 5 * 5)
results_rf_rrsp_2 <- vector('double', 5 * 5)
results_poi_rrsp_2 <- vector('double', 5 * 5)

# The following loops perform 5-fold cross-validation, repeated 5 times.
# For each iteration, the data is split into training and test sets based on FIPS codes, 
# and then models are trained and evaluated.

# Loop for 'rrsp' predictors
for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)

    # Loop over folds
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])

      # Evaluate models using 'rrsp' predictors
    results <- evaluate_models_rrsp(train_data, test_data)
    index <- 5 * (j - 1) + k

      # Store results
    results_rf_rrsp[index] <- results$rf_rmse
    results_poi_rrsp[index] <- results$poi_rmse
    results_rf_rrsp_2[index] <- results$rf_mape
    results_poi_rrsp_2[index] <- results$poi_mape
  }
}

# Initialize result storage vectors for RMSE and MAPE of random forest and Poisson models for 'ac' predictors.
results_rf_ac <- vector('double', 5 * 5)
results_poi_ac <- vector('double', 5 * 5)
results_rf_ac_2 <- vector('double', 5 * 5)
results_poi_ac_2 <- vector('double', 5 * 5)

# The following loops perform 5-fold cross-validation, repeated 5 times.
# For each iteration, the data is split into training and test sets based on FIPS codes, 
# and then models are trained and evaluated.

# Loop for 'ac' predictors
for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)

    # Loop over folds
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])

      # Evaluate models using 'ac' predictors
    results <- evaluate_models_ac(train_data, test_data)
    index <- 5 * (j - 1) + k

      # Store results
    results_rf_ac[index] <- results$rf_rmse
    results_poi_ac[index] <- results$poi_rmse
    results_rf_ac_2[index] <- results$rf_mape
    results_poi_ac_2[index] <- results$poi_mape
  }
}

# Initialize result storage vectors for RMSE and MAPE of random forest and Poisson models for 'all' predictors.
results_rf_all <- vector('double', 5 * 5)
results_poi_all <- vector('double', 5 * 5)
results_rf_all_2 <- vector('double', 5 * 5)
results_poi_all_2 <- vector('double', 5 * 5)


# The following loops perform 5-fold cross-validation, repeated 5 times.
# For each iteration, the data is split into training and test sets based on FIPS codes, 
# and then models are trained and evaluated.

# Loop for 'all' predictors
for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)

    # Loop over folds
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])

       # Evaluate models using 'all' predictors
    results <- evaluate_models_all(train_data, test_data)
    index <- 5 * (j - 1) + k
      
      # Store results
    results_rf_all[index] <- results$rf_rmse
    results_poi_all[index] <- results$poi_rmse
    results_rf_all_2[index] <- results$rf_mape
    results_poi_all_2[index] <- results$poi_mape
  }
}

# Results for RMSE using Random Forest for subset 'rr'
cat("Results for RF RMSE_rr:\n")
print(summary(results_rf_rr))
cat("SD:", sd(results_rf_rr), "\n")

# Results for RMSE using Poisson for subset 'rr'
cat("Results for Poisson RMSE_rr:\n")
print(summary(results_poi_rr))
cat("SD:", sd(results_poi_rr), "\n")

# Results for MAPE using Random Forest for subset 'rr'
cat("Results for RF MAPE_rr:\n")
print(summary(results_rf_rr_2))
cat("SD:", sd(results_rf_rr_2), "\n")

# Results for MAPE using Poisson for subset 'rr'
cat("Results for Poisson MAPE_rr:\n")
print(summary(results_poi_rr_2))
cat("SD:", sd(results_poi_rr_2), "\n")

# Results for RMSE using Random Forest for subset 'rrsp'
cat("Results for RF RMSE_rrsp:\n")
print(summary(results_rf_rrsp))
cat("SD:", sd(results_rf_rrsp), "\n")

# Results for RMSE using Poisson for subset 'rrsp'
cat("Results for Poisson RMSE_rrsp:\n")
print(summary(results_poi_rrsp))
cat("SD:", sd(results_poi_rrsp), "\n")

# Results for MAPE using Random Forest for subset 'rrsp'
cat("Results for RF MAPE_rrsp:\n")
print(summary(results_rf_rrsp_2))
cat("SD:", sd(results_rf_rrsp_2), "\n")

# Results for MAPE using Poisson for subset 'rrsp'
cat("Results for Poisson MAPE_rrsp:\n")
print(summary(results_poi_rrsp_2))
cat("SD:", sd(results_poi_rrsp_2), "\n")

# Results for RMSE using Random Forest for subset 'ac'
cat("Results for RF RMSE_ac:\n")
print(summary(results_rf_ac))
cat("SD:", sd(results_rf_ac), "\n")

# Results for RMSE using Poisson for subset 'ac'
cat("Results for Poisson RMSE_ac:\n")
print(summary(results_poi_ac))
cat("SD:", sd(results_poi_ac), "\n")

# Results for MAPE using Random Forest for subset 'ac'
cat("Results for RF MAPE_ac:\n")
print(summary(results_rf_ac_2))
cat("SD:", sd(results_rf_ac_2), "\n")

# Results for MAPE using Poisson for subset 'ac'
cat("Results for Poisson MAPE_ac:\n")
print(summary(results_poi_ac_2))
cat("SD:", sd(results_poi_ac_2), "\n")

# Results for RMSE using Random Forest for subset 'all'
cat("Results for RF RMSE_all:\n")
print(summary(results_rf_all))
cat("SD:", sd(results_rf_all), "\n")

# Results for RMSE using Poisson for subset 'all'
cat("Results for Poisson RMSE_all:\n")
print(summary(results_poi_all))
cat("SD:", sd(results_poi_all), "\n")

# Results for MAPE using Random Forest for subset 'all'
cat("Results for RF MAPE_all:\n")
print(summary(results_rf_all_2))
cat("SD:", sd(results_rf_all_2), "\n")

# Results for MAPE using Poisson for subset 'all'
cat("Results for Poisson MAPE_all:\n")
print(summary(results_poi_all_2))
cat("SD:", sd(results_poi_all_2), "\n")
