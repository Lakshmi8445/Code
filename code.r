

# Install necessary packages (run this once)
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("corrr")
install.packages("broom")

# Load required libraries
library(tidyverse)
library(ggplot2)

library(corrr)
library(broom)
library(readr)

cat("Files in current directory:\n")
print(list.files())

cat("\nLooking for CSV files...\n")
csv_files <- list.files(pattern = "\\.csv$")
print(csv_files)

filename <- "Cars Datasets 2025.csv"  
car_data <- read_csv(filename)

# Display first few rows
cat("First 10 rows of the dataset:\n")
print(head(car_data, 10))

# Display column names
cat("\nColumn names:\n")
print(names(car_data))

# Display data structure
cat("\nData structure:\n")
print(str(car_data))

# Summary statistics
cat("\nSummary statistics:\n")
print(summary(car_data))

# Clean the HorsePower column - e
car_data_clean <- car_data %>%
  # Extract the first number from HorsePower 
  mutate(
    horsepower_clean = as.numeric(str_extract(HorsePower, "\\d+")), 

    # Clean price - remove $ and commas using 
    price_clean = as.numeric(str_replace_all(`Cars Prices`, "[$,]", ""))
  ) %>%
  # Remove rows with missing values
  filter(!is.na(horsepower_clean) & !is.na(price_clean)) %>%
  # Keep original columns plus cleaned ones
  select(`Company Names`, `Cars Names`, HorsePower, horsepower_clean,
         `Cars Prices`, price_clean, everything())

# Show cleaned data
cat("CLEANED DATA SAMPLE:\n")
print(head(car_data_clean[, c("Company Names", "Cars Names",
                              "HorsePower", "horsepower_clean",
                              "Cars Prices", "price_clean")]))

cat("\nDATA SUMMARY:\n")
cat("Number of cars:", nrow(car_data_clean), "\n")
cat("Horsepower range:", range(car_data_clean$horsepower_clean), "hp\n")
cat("Price range: $", format(range(car_data_clean$price_clean), big.mark = ","), "\n")
cat("Mean horsepower:", round(mean(car_data_clean$horsepower_clean), 1), "hp\n")
cat("Mean price: $", format(round(mean(car_data_clean$price_clean)), big.mark = ","), "\n")

# Create scatter plot with proper formatting
scatter_plot <- ggplot(car_data_clean, aes(x = horsepower_clean, y = price_clean)) +
  geom_point(aes(color = `Company Names`), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "solid") +
  labs(
    title = "Correlation between Horsepower and Car Price",
    subtitle = "Research Question: Is there a correlation between car horsepower and car prices?",
    x = "Horsepower (hp)",
    y = "Price (USD)",
    color = "Brand"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = 8)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8)
  ) +
  # Add value labels for extreme points
  geom_text(
    data = car_data_clean %>%
      filter(price_clean > 800000 | horsepower_clean > 900 | price_clean < 20000),
    aes(label = paste(`Company Names`, `Cars Names`)),
    hjust = -0.1, vjust = 0.5, size = 3
  )

print(scatter_plot)

# FIXED CODE for better histograms
library(ggplot2)
library(gridExtra)
library(scales)

# Calculate means
mean_hp <- mean(car_data_clean$horsepower_clean)
mean_price <- mean(car_data_clean$price_clean)

# Log scale for price (
hist_hp <- ggplot(car_data_clean, aes(x = horsepower_clean)) +
  geom_histogram(
    binwidth = 30,
    fill = "steelblue",
    color = "white",
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = mean_hp,
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  annotate(
    "text",
    x = mean_hp + 40,
    y = Inf,
    label = paste("Mean:", round(mean_hp, 1), "hp"),
    color = "red",
    size = 3.5,
    vjust = 2,
    hjust = 0
  ) +
  labs(
    title = "Horsepower Distribution",
    x = "Horsepower (hp)",
    y = "Number of Cars"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# PRICE HISTOGRAM WITH LOG SCALE
hist_price_log <- ggplot(car_data_clean, aes(x = price_clean)) +
  geom_histogram(
    bins = 30,  # Use bins instead of binwidth for better automatic sizing
    fill = "darkgreen",
    color = "white",
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = mean_price,
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  annotate(
    "text",
    x = mean_price * 1.5,
    y = Inf,
    label = paste("Mean: $", format(round(mean_price), big.mark = ",")),
    color = "red",
    size = 3.5,
    vjust = 2,
    hjust = 0
  ) +
  labs(
    title = "Price Distribution (Log Scale)",
    x = "Price (USD)",
    y = "Number of Cars"
  ) +
  scale_x_log10(  # LOG SCALE to handle extreme values
    labels = dollar_format(),
    breaks = c(10000, 50000, 100000, 500000, 1000000, 5000000, 10000000)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

combined_figure <- grid.arrange(hist_hp, hist_price_log, ncol = 2)
print(combined_figure)

# Calculate Pearson correlation
correlation <- cor(car_data_clean$horsepower_clean, car_data_clean$price_clean)
cat("CORRELATION ANALYSIS\n")
cat(strrep("=", 50), "\n")
cat("Pearson Correlation Coefficient (r):", round(correlation, 3), "\n\n")

# Test for statistical significance
cor_test <- cor.test(car_data_clean$horsepower_clean, car_data_clean$price_clean)
cat("Hypothesis Test Results:\n")
cat("Null Hypothesis (H0): No correlation (ρ = 0)\n")
cat("Alternative Hypothesis (H1): Correlation exists (ρ ≠ 0)\n\n")
cat("Test Statistic (t):", round(cor_test$statistic, 3), "\n")
cat("P-value:", format.pval(cor_test$p.value, digits = 3), "\n")
cat("95% Confidence Interval: [",
    round(cor_test$conf.int[1], 3), ",",
    round(cor_test$conf.int[2], 3), "]\n\n")

# Interpret the correlation
r_value <- abs(correlation)
if(r_value >= 0.7) {
  strength <- "strong"
} else if(r_value >= 0.5) {
  strength <- "moderate"
} else if(r_value >= 0.3) {
  strength <- "weak"
} else {
  strength <- "very weak"
}

cat("INTERPRETATION:\n")
cat("There is a", strength, if(correlation > 0) "positive" else "negative",
    "correlation between horsepower and price.\n")
cat("Correlation coefficient (r) =", round(correlation, 3), "\n")

if(cor_test$p.value < 0.05) {
  cat("\nSTATISTICAL CONCLUSION: Reject H0. The correlation is statistically significant (p < 0.05).\n")
} else {
  cat("\nSTATISTICAL CONCLUSION: Fail to reject H0. The correlation is not statistically significant.\n")
}