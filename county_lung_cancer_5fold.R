# Load required libraries
library(caret)
library(rfCountData)
library(Metrics)

# Read in the data
data <- read.csv('C:\\Users\\hlee936\\OneDrive - Georgia Institute of Technology\\project\\python\\data_final\\data_poisson_final_female.csv')
fip <- read.csv('C:\\Users\\hlee936\\OneDrive - Georgia Institute of Technology\\project\\python\\data_final\\fips.csv')

# This function splits the main data based on given FIPS county codes.
# Essentially, for each group of FIPS in 'fip_sample', it subsets the data.
split_data <- function(data, fip_sample) {
  df_list <- lapply(fip_sample, function(fip_group) {
    subset(data, fips %in% fip_group)
  })
  return(df_list)
}

# Followings are the functions for evaluating perfermance of random forests and Poisson regression models.
# Each number of columns corresponds to followings
# 4 age group (divided into five-year intervals, e.g., 30-34 years old)
# 5 incidence count 6 population 7 pm2.5 8 RadNet 9 ozone 10 High school 11 Median family income 
# 12 unemployed 13 urban 14 radon 15 butadiene 16 acetaldehyde 17 formaldehyde 18 benzene 20 smoking 21 drinking


#Function to split the data based on FIPS
split_data <- function(data, fip_sample) {
  df_list <- lapply(fip_sample, function(fip_group) {
    subset(data, fips %in% fip_group)
  })
  return(df_list)
}


# This function trains random forests and Poisson regression models using all variables without drinking, buta, acete as predictors
# and evaluates their performance using root mean square error (RMSE) and mean absolute percentage error (MAPE).

evaluate_models <- function(train_data, test_data) {
  # Extract training data columns
  x_train <- train_data[, c(4,7,8,9,10,11,12,13,14,17,18,20)]
  y_train <- train_data[, 5]
  off_train <- log(train_data[, 6])
  
  # Extract test data columns
  x_test <- test_data[, c(4,7,8,9,10,11,12,13,14,17,18,20)]
  y_test <- test_data[, 5]
  off_test <- log(test_data[, 6])
  
  # Train random forest
  run <- rfPoisson(x = x_train, offset = off_train, y = y_train)
  y_predict <- predict(run, newdata = x_test, offset = off_test)
  
  # Train Poisson regression model
  run2 <- glm(Count ~ smoking_10+drinking_10+cdc_radon_median+RadNet+ozone+pm2.5+cdc_aq_tox_formaldehyde_2011+cdc_aq_tox_benzene_2011+High_school_education+Median_family_income+Unemployed +Urban+Age_recode_with_1_year_olds, 
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


# Initialize result storage vectors for RMSE and MAPE of random forest and Poisson models for assigned predictors.
results_rf <- vector('double', 5 * 5)
results_rf_2 <- vector('double', 5 * 5)
results_poi <- vector('double', 5 * 5)
results_poi_2 <- vector('double', 5 * 5)

# The following loops perform 5-fold cross-validation, repeated 5 times.
# For each iteration, the data is split into training and test sets based on FIPS codes, 
# and then models are trained and evaluated.

#Loop for assinged predictors
for (j in 1:5) {
  dfnum <- sample(1:5, size = nrow(fip), replace = TRUE, prob = rep(0.2, 5))
  fip_samples <- split(fip$fips, dfnum)
  df_list <- split_data(data, fip_samples)
  
  # Loop over folds
  for (k in 1:5) {
    test_data <- df_list[[k]]
    train_data <- do.call(rbind, df_list[-k])
    
    # Evaluate models using predictors
    results <- evaluate_models(train_data, test_data)
    index <- 5 * (j - 1) + k
    
    # Store results
    results_rf[index] <- results$rf_rmse
    results_poi[index] <- results$poi_rmse
    results_rf_2[index] <- results$rf_mape
    results_poi_2[index] <- results$poi_mape
  }
}

cat("Results for RF RMSE:\n")
print(summary(results_rf))
cat("SD:", sd(results_rf), "\n")
cat("Results for RF MAPE:\n")
print(summary(results_rf_2))
cat("SD:", sd(results_rf_2), "\n")
cat("Results for Poisson RMSE:\n")
print(summary(results_poi))
cat("SD:", sd(results_poi), "\n")
cat("Results for Poisson MAPE:\n")
print(summary(results_poi_2))
cat("SD:", sd(results_poi_2), "\n")
