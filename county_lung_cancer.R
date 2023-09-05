library(caret)
library(rfCountData)
library(Metrics)

data <- read.csv('data_poisson_final_male.csv')
fip <- read.csv('fips.csv')

# Function to split the data based on FIPS
split_data <- function(data, fip_sample) {
    df_list <- lapply(fip_sample, function(fip_group) {
        subset(data, fips %in% fip_group)
    })
    return(df_list)
}

# Function to train the models and evaluate with rr
evaluate_models_rr <- function(train_data, test_data) {
  x_train <- train_data[, c(4, 8, 14)]
  y_train <- train_data[, 5]
  off_train <- log(train_data[, 6])
  
  x_test <- test_data[, c(4, 8, 14)]
  y_test <- test_data[, 5]
  off_test <- log(test_data[, 6])
  
  run <- rfPoisson(x = x_train, offset = off_train, y = y_train)
  y_predict <- predict(run, newdata = x_test, offset = off_test)
  
  run2 <- glm(Count ~ cdc_radon_median + RadNet + Age_recode_with_1_year_olds, 
              data = train_data, family = "poisson", offset = log(Pop))
  y_predict2 <- predict(run2, test_data)
  
  return(list(
    rf_rmse = rmse(y_predict, y_test),
    poi_rmse = rmse(y_predict2, y_test),
    rf_mape = mape(y_predict, y_test),
    poi_mape = mape(y_predict2, y_test)
  ))
}


# Function to train the models and evaluate with rrsp
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


# Function to train the models and evaluate with ac
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


# Function to train the models and evaluate with all
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



results_rf_rr <- vector('double', 5 * 5)
results_poi_rr <- vector('double', 5 * 5)
results_rf_rr_2 <- vector('double', 5 * 5)
results_poi_rr_2 <- vector('double', 5 * 5)

for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)
  
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])
    
    results <- evaluate_models_rr(train_data, test_data)
    index <- 5 * (j - 1) + k
    
    results_rf_rr[index] <- results$rf_rmse
    results_poi_rr[index] <- results$poi_rmse
    results_rf_rr_2[index] <- results$rf_mape
    results_poi_rr_2[index] <- results$poi_mape
  }
}


results_rf_rrsp <- vector('double', 5 * 5)
results_poi_rrsp <- vector('double', 5 * 5)
results_rf_rrsp_2 <- vector('double', 5 * 5)
results_poi_rrsp_2 <- vector('double', 5 * 5)

for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)
  
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])
    
    results <- evaluate_models_rrsp(train_data, test_data)
    index <- 5 * (j - 1) + k
    
    results_rf_rrsp[index] <- results$rf_rmse
    results_poi_rrsp[index] <- results$poi_rmse
    results_rf_rrsp_2[index] <- results$rf_mape
    results_poi_rrsp_2[index] <- results$poi_mape
  }
}

results_rf_ac <- vector('double', 5 * 5)
results_poi_ac <- vector('double', 5 * 5)
results_rf_ac_2 <- vector('double', 5 * 5)
results_poi_ac_2 <- vector('double', 5 * 5)

for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)
  
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])
    
    results <- evaluate_models_ac(train_data, test_data)
    index <- 5 * (j - 1) + k
    
    results_rf_ac[index] <- results$rf_rmse
    results_poi_ac[index] <- results$poi_rmse
    results_rf_ac_2[index] <- results$rf_mape
    results_poi_ac_2[index] <- results$poi_mape
  }
}

results_rf_all <- vector('double', 5 * 5)
results_poi_all <- vector('double', 5 * 5)
results_rf_all_2 <- vector('double', 5 * 5)
results_poi_all_2 <- vector('double', 5 * 5)

for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)
  
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])
    
    results <- evaluate_models_all(train_data, test_data)
    index <- 5 * (j - 1) + k
    
    results_rf_all[index] <- results$rf_rmse
    results_poi_all[index] <- results$poi_rmse
    results_rf_all_2[index] <- results$rf_mape
    results_poi_all_2[index] <- results$poi_mape
  }
}

cat("Results for RF RMSE_rr:\n")
print(summary(results_rf_rr))
cat("SD:", sd(results_rf_rr), "\n")

cat("Results for Poisson RMSE_rr:\n")
print(summary(results_poi_rr))
cat("SD:", sd(results_poi_rr), "\n")

cat("Results for RF MAPE_rr:\n")
print(summary(results_rf_rr_2))
cat("SD:", sd(results_rf_rr_2), "\n")

cat("Results for Poisson MAPE_rr:\n")
print(summary(results_poi_rr_2))
cat("SD:", sd(results_poi_rr_2), "\n")


cat("Results for RF RMSE_rrsp:\n")
print(summary(results_rf_rrsp))
cat("SD:", sd(results_rf_rrsp), "\n")

cat("Results for Poisson RMSE_rrsp:\n")
print(summary(results_poi_rrsp))
cat("SD:", sd(results_poi_rrsp), "\n")

cat("Results for RF MAPE_rrsp:\n")
print(summary(results_rf_rrsp_2))
cat("SD:", sd(results_rf_rrsp_2), "\n")

cat("Results for Poisson MAPE_rrsp:\n")
print(summary(results_poi_rrsp_2))
cat("SD:", sd(results_poi_rrsp_2), "\n")


cat("Results for RF RMSE_ac:\n")
print(summary(results_rf_ac))
cat("SD:", sd(results_rf_ac), "\n")

cat("Results for Poisson RMSE_ac:\n")
print(summary(results_poi_ac))
cat("SD:", sd(results_poi_ac), "\n")

cat("Results for RF MAPE_ac:\n")
print(summary(results_rf_ac_2))
cat("SD:", sd(results_rf_ac_2), "\n")

cat("Results for Poisson MAPE_ac:\n")
print(summary(results_poi_ac_2))
cat("SD:", sd(results_poi_ac_2), "\n")


cat("Results for RF RMSE_all:\n")
print(summary(results_rf_all))
cat("SD:", sd(results_rf_all), "\n")

cat("Results for Poisson RMSE_all:\n")
print(summary(results_poi_all))
cat("SD:", sd(results_poi_all), "\n")

cat("Results for RF MAPE_all:\n")
print(summary(results_rf_all_2))
cat("SD:", sd(results_rf_all_2), "\n")

cat("Results for Poisson MAPE_all:\n")
print(summary(results_poi_all_2))
cat("SD:", sd(results_poi_all_2), "\n")
