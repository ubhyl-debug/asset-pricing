# Install and load necessary packages
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(readr)
library(dplyr)
library(tidyverse)


# Load data
q4_consumption <- read_csv('consumption_growth2.csv')
portfolio_returns <- read_csv('25_Portfolios_5x5.CSV', col_types = cols())
fama_french_factors <- read_csv('F-F-Annual_FF.csv')

# Filter data by relevant years
relevant_years <- 1948:2022

q4_consumption <- q4_consumption %>%
  filter(YEAR %in% relevant_years)

portfolio_returns <- portfolio_returns %>%
  filter(YEAR %in% relevant_years)

fama_french_factors <- fama_french_factors %>%
  filter(YEAR %in% relevant_years)

# Calculate Excess Returns for each year
excess_returns_list <- list()

for (year in relevant_years) {
  # Filter the risk-free rate for the current year
  rf <- fama_french_factors %>%
    filter(YEAR == year) %>%
    pull(RF)
  
  # Filter portfolio returns for the current year
  portfolio_returns_year <- portfolio_returns %>%
    filter(YEAR == year)
  
  # Subtract the risk-free rate from each portfolio return
  excess_returns_year <- portfolio_returns_year %>%
    mutate(across(-YEAR, ~ . - rf))  # Subtract rf from all columns except YEAR
  
  # Append the result to the list
  excess_returns_list[[as.character(year)]] <- excess_returns_year
}

# Combine all years into a single dataframe
excess_returns <- bind_rows(excess_returns_list)

# Check the structure of the resulting excess returns
str(excess_returns)

aligned_q4_consumption <- q4_consumption %>%
  arrange(YEAR)

aligned_excess_returns <- excess_returns %>%
  arrange(YEAR)

# Ensure that 'YEAR' column is only once in the final dataframe
combined_data <- cbind(aligned_excess_returns, c_growth = aligned_q4_consumption$c_growth)

# Check the structure of the combined data
str(combined_data)

# Add Fama-French factors to the combined data
combined_data <- combined_data %>%
  left_join(fama_french_factors, by = "YEAR")

# Check the structure of the combined data
str(combined_data)

# Extract portfolio data
portfolio_excess_returns <- combined_data %>% select(-YEAR, -c_growth, -Mkt_RF, -SMB, -HML, -RF)

# Calculate average excess returns for each portfolio
average_excess_returns <- portfolio_excess_returns %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Portfolio", values_to = "Avg_Excess_Return")

# Conduct time series regression
betas <- list()

# Perform regression for each portfolio
for (portfolio in colnames(portfolio_excess_returns)) {
  # Construct the formula for regression
  formula <- as.formula(paste0("`", portfolio, "` ~ Mkt_RF + SMB + HML"))
  
  # Perform the linear regression
  model <- lm(formula, data = combined_data)
  
  # Extract the beta coefficients for Mkt_RF, SMB, and HML
  beta_mkt_rf <- coef(model)["Mkt_RF"]
  beta_smb <- coef(model)["SMB"]
  beta_hml <- coef(model)["HML"]
  
  # Store the beta values
  betas[[portfolio]] <- c(beta_mkt_rf, beta_smb, beta_hml)
}

# Convert the list of betas to a data frame
betas_df <- do.call(rbind, betas)
betas_df <- as.data.frame(betas_df)
colnames(betas_df) <- c("Mkt_RF_Beta", "SMB_Beta", "HML_Beta")
betas_df$Portfolio <- rownames(betas_df)

# Merge betas with average excess returns
betas_df <- betas_df %>%
  left_join(average_excess_returns, by = "Portfolio")

# Perform cross-sectional regression
cross_sectional_model_fama <- lm(Avg_Excess_Return ~ Mkt_RF_Beta + SMB_Beta + HML_Beta, data = betas_df)
print(cross_sectional_model_fama)
  
# Extracting the coefficients
lambda_market_mkt <- coef(cross_sectional_model_fama)["Mkt_RF_Beta"]
lambda_market_smb <- coef(cross_sectional_model_fama)["SMB_Beta"]
lambda_market_hml <- coef(cross_sectional_model_fama)["HML_Beta"]
gamma_3 <- coef(cross_sectional_model_fama)["(Intercept)"]

lambda_market_combined = c(lambda_market_mkt,lambda_market_smb,lambda_market_hml)

# Print lambda_markets and gamma
factor_beta = c("Mkt_RF_Beta","SMB_Beta","HML_Beta")
for (factor in factor_beta) {
  print(paste(factor,":"))
  print(paste("Lambda_market (Risk Premium):", lambda_market_combined[factor]))
}
print(paste("Gamma (Intercept):", gamma_3))


  





