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
q4_consumption_c <- read_csv('consumption_growth2.csv')
portfolio_returns_c <- read_csv('25_Portfolios_5x5.CSV', col_types = cols())
fama_french_factors_c <- read_csv('F-F-Annual_FF.csv')

# Filter data by relevant years
relevant_years_c <- c(1948:2022)

q4_consumption_c <- q4_consumption_c %>%
  filter(YEAR %in% relevant_years_c)

portfolio_returns_c <- portfolio_returns_c %>%
  filter(YEAR %in% relevant_years_c)

fama_french_factors_c <- fama_french_factors_c %>%
  filter(YEAR %in% relevant_years_c)

# Calculate Excess Returns for each year
excess_returns_list_c <- list()

for (year in relevant_years_c) {
  rf_c <- fama_french_factors_c %>%
    filter(YEAR == year) %>%
    pull(RF)
  
  portfolio_returns_year_c <- portfolio_returns_c %>%
    filter(YEAR == year)
  
  excess_returns_year_c <- portfolio_returns_year_c %>%
    mutate(across(-YEAR, ~ . - rf_c))  # Subtract rf from all columns except YEAR
  
  excess_returns_list_c[[as.character(year)]] <- excess_returns_year_c
}

excess_returns_c <- bind_rows(excess_returns_list_c)

# Align the data
aligned_q4_consumption_c <- q4_consumption_c %>%
  arrange(YEAR)

aligned_excess_returns_c <- excess_returns_c %>%
  arrange(YEAR)

combined_data_c <- cbind(aligned_excess_returns_c, c_growth = aligned_q4_consumption_c$c_growth)

portfolio_excess_returns_c <- combined_data_c %>% select(-YEAR, -c_growth)

# Calculate consumption growth betas for each portfolio
betas_c <- list()

for(portfolio in colnames(portfolio_excess_returns_c)) {
  formula <- as.formula(paste("`", portfolio, "`", "~ c_growth", sep = ""))
  model_c <- lm(formula, data = combined_data_c)
  beta_c <- coef(model_c)["c_growth"]
  betas_c[[portfolio]] <- beta_c
}

betas_c_df <- data.frame(Portfolio = names(betas_c), Consumption_Beta = unlist(betas_c))

# Calculate average excess returns for each portfolio
average_excess_returns_c <- portfolio_excess_returns_c %>% 
  summarise(across(everything(), mean))

average_excess_returns_c_df <- pivot_longer(average_excess_returns_c, 
                                          cols = everything(), 
                                          names_to = "Portfolio", 
                                          values_to = "Average_Excess_Return")
print(average_excess_returns_c_df)

betas_and_returns_c <- betas_c_df %>%
  left_join(average_excess_returns_c_df, by = "Portfolio")

# Perform cross-sectional regression
cross_sectional_model_c <- lm(Average_Excess_Return ~ Consumption_Beta, data = betas_and_returns_c)
summary(cross_sectional_model_c)
print(cross_sectional_model_c)
# Extracting the coefficients
lambda_market_c <- coef(cross_sectional_model_c)["Consumption_Beta"]
gamma_c <- coef(cross_sectional_model_c)["(Intercept)"]

# Print lambda_market and gamma
print(paste("Lambda_market (Risk Premium):", lambda_market_c))
print(paste("Gamma (Intercept):", gamma_c))

# Plot the relationship
plot <- ggplot(betas_and_returns_c, aes(x = Consumption_Beta, y = Average_Excess_Return)) +
  geom_point(color = "blue", size = 3) +  # Points for the relationship
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(title = "Average Excess Returns vs. Consumption Growth Beta per Portfolio [1948-2022]",
       x = "Consumption Growth Beta",
       y = "Average Excess Return") +
  theme_minimal()

print(plot)

# Save the plot to a file (optional)
ggsave("average_excess_returns_vs_consumption_growth_beta_scatter_plot.png", plot)

# Display the betas and average returns along with the regression results
betas_and_returns_c
