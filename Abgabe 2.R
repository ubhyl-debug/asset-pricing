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
fama_french_factors <- read_csv('F-F-Annual.csv')

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

# extract portfolio data
portfolio_excess_returns <- combined_data %>% select(-YEAR, -c_growth)
# 3. Conduct time series regression

str(portfolio_excess_returns)
betas <- list()

# Extract portfolio data
portfolio_excess_returns <- combined_data %>% select(-YEAR, -c_growth)

# Conduct time series regression
betas <- list()

# Perform regression for each portfolio
for(portfolio in colnames(portfolio_excess_returns)) {
  # Construct the formula for regression
  formula <- as.formula(paste("`", portfolio, "`", "~ c_growth", sep = ""))
  
  # Perform the linear regression
  model <- lm(formula, data = combined_data)
  
  # Extract the beta coefficient for c_growth
  beta <- coef(model)["c_growth"]
  
  # Store the beta value
  betas[[portfolio]] <- beta
}

# Convert the list of betas to a data frame
betas_df <- data.frame(Portfolio = names(betas), Consumption_Beta = unlist(betas))

# Print the consumption betas for the 25 portfolios
print(betas_df)
# Check summary for a specific portfolio to verify regression output

