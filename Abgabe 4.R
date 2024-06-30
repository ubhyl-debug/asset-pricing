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
q4_consumption <- read_csv('consumption_growth.csv')
portfolio_returns <- read_csv('25_Portfolios_5x5.CSV', col_types = cols())
fama_french_factors <- read_csv('F-F-Annual.csv')

# Filter data by relevant years
relevant_years <- c(1948, 1949, 1950, 1951, 1952)

q4_consumption <- q4_consumption %>%
  filter(YEAR %in% relevant_years)

portfolio_returns <- portfolio_returns %>%
  filter(YEAR %in% relevant_years)

fama_french_factors <- fama_french_factors %>%
  filter(YEAR %in% relevant_years)

# Calculate Excess Returns for each year
excess_returns_list <- list()

for (year in relevant_years) {
  rf <- fama_french_factors %>%
    filter(YEAR == year) %>%
    pull(RF)
  
  portfolio_returns_year <- portfolio_returns %>%
    filter(YEAR == year)
  
  excess_returns_year <- portfolio_returns_year %>%
    mutate(across(-YEAR, ~ . - rf))  # Subtract rf from all columns except YEAR
  
  excess_returns_list[[as.character(year)]] <- excess_returns_year
}

excess_returns <- bind_rows(excess_returns_list)

# Align the data
aligned_q4_consumption <- q4_consumption %>%
  arrange(YEAR)

aligned_excess_returns <- excess_returns %>%
  arrange(YEAR)

combined_data <- cbind(aligned_excess_returns, c_growth = aligned_q4_consumption$c_growth)

portfolio_excess_returns <- combined_data %>% select(-YEAR, -c_growth)

# Calculate consumption growth betas for each portfolio
betas <- list()

for(portfolio in colnames(portfolio_excess_returns)) {
  formula <- as.formula(paste("`", portfolio, "`", "~ c_growth", sep = ""))
  model <- lm(formula, data = combined_data)
  beta <- coef(model)["c_growth"]
  betas[[portfolio]] <- beta
}

betas_df <- data.frame(Portfolio = names(betas), Consumption_Beta = unlist(betas))

# Calculate average excess returns for each portfolio
average_excess_returns <- portfolio_excess_returns %>% 
  summarise(across(everything(), mean))

average_excess_returns_df <- pivot_longer(average_excess_returns, 
                                          cols = everything(), 
                                          names_to = "Portfolio", 
                                          values_to = "Average_Excess_Return")
print(average_excess_returns_df)

betas_and_returns <- betas_df %>%
  left_join(average_excess_returns_df, by = "Portfolio")

# Perform cross-sectional regression
cross_sectional_model <- lm(Average_Excess_Return ~ Consumption_Beta, data = betas_and_returns)
summary(cross_sectional_model)
print(cross_sectional_model)
# Extracting the coefficients
lambda_market <- coef(cross_sectional_model)["Consumption_Beta"]
gamma <- coef(cross_sectional_model)["(Intercept)"]

# Print lambda_market and gamma
print(paste("Lambda_market (Risk Premium):", lambda_market))
print(paste("Gamma (Intercept):", gamma))

# Plot the relationship
plot <- ggplot(betas_and_returns, aes(x = Consumption_Beta, y = Average_Excess_Return)) +
  geom_point(color = "blue", size = 3) +  # Points for the relationship
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(title = "Average Excess Returns vs. Consumption Growth Beta per Portfolio [1948-1952]",
       x = "Consumption Growth Beta",
       y = "Average Excess Return") +
  theme_minimal()

print(plot)

# Save the plot to a file (optional)
ggsave("average_excess_returns_vs_consumption_growth_beta_scatter_plot.png", plot)

# Display the betas and average returns along with the regression results
betas_and_returns