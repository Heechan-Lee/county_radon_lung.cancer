# Load required libraries
library(caret)
library(rfCountData)
library(Metrics)

# Read in the data
data <- read.csv('data_poisson_final_female.csv')
fip <- read.csv('fips.csv')

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
# 4 age group 5 incidence count 6 population 7 pm2.5 8 RadNet 9 ozone 10 High school 11 Median family income 
# 12 unemployed 13 urban 14 radon 15 butadiene 16 acetaldehyde 17 formaldehyde 18 benzene 20 smoking 21 drinking


#Models to check VIF
poi_r <- glm(Count ~ smoking_10+cdc_radon_median+RadNet+ozone+pm2.5+cdc_aq_tox_formaldehyde_2011+cdc_aq_tox_benzene_2011+High_school_education+Median_family_income+Unemployed +Urban+Age_recode_with_1_year_olds,data=data, family = "poisson", offset=log(Pop))
vif_values <- vif(poi_r)
print(vif_values)

#IRR from Poisson regression

poi_r <- glm(Count ~ smoking_10+cdc_radon_median+RadNet+ozone+pm2.5+cdc_aq_tox_formaldehyde_2011+cdc_aq_tox_benzene_2011+High_school_education+Median_family_income+Unemployed +Urban+Age_recode_with_1_year_olds,data=data, family = "poisson", offset=log(Pop))
estimates <- summary(poi_r)$coefficients[, "Estimate"]
p_values <- summary(poi_r)$coefficients[, "Pr(>|z|)"]

# Compute 95% confidence intervals for the coefficients
conf_intervals <- confint(poi_r, level = 0.95)

# Combine the statistics into a single data frame
results <- data.frame(
  Estimate = estimates,
  `Lower 95% CI` = conf_intervals[, 1],
  `Upper 95% CI` = conf_intervals[, 2],
  `P-value` = p_values
)

# Print the results
print(results)

# Exponentiate to get IRR
IRR <- exp(estimates)
IRR_lower <- exp(conf_intervals[, 1])
IRR_upper <- exp(conf_intervals[, 2])

# Combine the statistics into a single data frame
results_IRR <- data.frame(
  IRR = IRR,
  `Lower 95% CI` = IRR_lower,
  `Upper 95% CI` = IRR_upper,
  `P-value` = p_values
)

# Print the results
print(results_IRR)



# Calculating VIM

x <- data[,c(4,7,8,9,10,11,12,13,14,17,18,20)]
y <- data[,c(5)]
off_train <- log(data[,c(6)])

numeric_cols <- sapply(x, is.numeric)

# Now, split the dataframe
x_numeric <- x[, numeric_cols]
x_numeric <- scale(x_numeric)

x[, numeric_cols] <- x_numeric

x_stand<- cbind(x, y)
x_stand <- x_stand[,-ncol(x_stand)]

num_iterations <- 50

for(i in 1:num_iterations) {
  
  # Train the model using standardized data. 
  model <- rfPoisson(x=x_stand, offset=off_train, y=y)
  
  # Initialize total_importance after the first run
  if(i == 1) {
    total_importance <- matrix(0, nrow = nrow(importance(model)), ncol = ncol(importance(model)))
  }
  
  # Check dimensions before adding
  if(!all(dim(total_importance) == dim(importance(model)))) {
    stop(paste("Dimension mismatch in iteration", i))
  }
  
  # Accumulate importances
  total_importance <- total_importance + importance(model)
}

# Compute the average importance
avg_importance <- total_importance / num_iterations

# Print the results
print(avg_importance)
