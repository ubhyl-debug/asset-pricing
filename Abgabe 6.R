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
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Load data
q4_consumption <- read_csv('consumption_growth2.csv')
portfolio_returns <- read_csv('25_Portfolios_5x5.CSV', col_types = cols())
fama_french_factors <- read_csv('F-F-Annual_FF.csv')

# Load data Abgabe 4.R
source("Abgabe 4.R")

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
cross_sectional_model_c <- lm(Avg_Excess_Return ~ Mkt_RF_Beta + SMB_Beta + HML_Beta, data = betas_df)
print(cross_sectional_model_c)

# Extracting the coefficients
lambda_market_mkt <- coef(cross_sectional_model_c)["Mkt_RF_Beta"]
lambda_market_smb <- coef(cross_sectional_model_c)["SMB_Beta"]
lambda_market_hml <- coef(cross_sectional_model_c)["HML_Beta"]
gamma_3 <- coef(cross_sectional_model_c)["(Intercept)"]

lambda_market_combined = c(lambda_market_mkt,lambda_market_smb,lambda_market_hml)

# Predicted returns for CCAMP
betas_and_returns_c <- betas_and_returns_c %>%
  mutate(Predicted_Excess_Return_c = gamma_c + lambda_market_c * Consumption_Beta)

# Predicted returns for Fama-French
betas_df <- betas_df %>%
  mutate(Predicted_Excess_Return_fama = gamma_3  + 
           lambda_market_mkt * Mkt_RF_Beta + 
           lambda_market_smb * SMB_Beta + 
           lambda_market_hml * HML_Beta)

# Plots actual Avg. Excess Return vs. predicted Avg. Excess Return for CCAPM
plot_ccapm <- ggplot(betas_and_returns_c, aes(x = Average_Excess_Return, y = Predicted_Excess_Return_c)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual Avg. Excess Return vs. Predicted Avg. Excess Return (CCAPM)",
       x = "Avg. Excess Return",
       y = "Predicted Avg. Excess Return") +
  theme_minimal()

# Plots actual Avg. Excess Return vs. predicted Avg. Excess Return for Fama-French
plot_ff <- ggplot(betas_df, aes(x = Avg_Excess_Return, y = Predicted_Excess_Return_fama)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual Avg. Excess Return vs. Predicted Avg. Excess Return (Fama-French)",
       x = "Avg. Excess Return",
       y = "Predicted Avg. Excess Return") +
  theme_minimal()

print(plot_ccapm)
print(plot_ff)

# Save the plot to a file (optional)
ggsave("Actual Avg. Excess Return vs. Predicted Avg. Excess Return (CCAPM).png", plot_ccapm)
ggsave("Actual Avg. Excess Return vs. Predicted Avg. Excess Return (Fama-French).png", plot_ff)

# Calculate the cross-sectional R²-values
R2_ccapm <- summary(lm(Average_Excess_Return ~ Predicted_Excess_Return_c, data = betas_and_returns_c))$r.squared
R2_ff <- summary(lm(Avg_Excess_Return ~ Predicted_Excess_Return_fama, data = betas_df))$r.squared

print(paste("Cross-sectional R²-values (CCAPM):", R2_ccapm))
print(paste("Cross-sectional R²-values (Fama-French):", R2_ff))

# Interpretation
if (R2_ff > R2_ccapm) {
  print("Das Fama-French-Modell erklärt die Renditen der 25 Fama-French-Portfolios besser als das konsumbasierte CAPM-Modell.")
} else {
  print("Das konsumbasierte CAPM-Modell erklärt die Renditen der 25 Fama-French-Portfolios besser als das Fama-French-Modell.")
}
