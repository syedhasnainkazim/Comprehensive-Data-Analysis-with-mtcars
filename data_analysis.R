# data_analysis.R
# =============================================================================
# Project Title: Comprehensive Data Analysis with mtcars
# Description: This script demonstrates a complete data analysis workflow,
#              including data exploration, cleaning, visualization, correlation
#              analysis, modeling, and hypothesis testing using the mtcars dataset.
# Author: Syed Kazim
# Date: [Insert Date]
# =============================================================================

# -----------------------------------------------------------------------------
# Section 1: Setup
# -----------------------------------------------------------------------------
# Load required packages. If not installed, install them first.
required_packages <- c("dplyr", "ggplot2", "readr", "corrplot")
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
invisible(lapply(required_packages, install_if_missing))

# -----------------------------------------------------------------------------
# Section 2: Data Loading and Exploration
# -----------------------------------------------------------------------------
# Load the built-in mtcars dataset.
data("mtcars")

# Print basic information about the dataset.
cat("Summary of mtcars dataset:\n")
print(summary(mtcars))
cat("\nStructure of mtcars dataset:\n")
print(str(mtcars))

# -----------------------------------------------------------------------------
# Section 3: Data Cleaning and Preparation
# -----------------------------------------------------------------------------
# Convert several numeric variables into factors for categorical analysis.
mtcars <- mtcars %>%
  mutate(
    cyl  = factor(cyl),
    vs   = factor(vs, labels = c("V-shaped", "Straight")),
    am   = factor(am, labels = c("Automatic", "Manual")),
    gear = factor(gear),
    carb = factor(carb)
  )

# -----------------------------------------------------------------------------
# Section 4: Exploratory Data Analysis (EDA)
# -----------------------------------------------------------------------------
# 4.1: Descriptive Statistics by Cylinder Count
summary_by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    count    = n(),
    avg_mpg  = round(mean(mpg), 2),
    sd_mpg   = round(sd(mpg), 2),
    avg_hp   = round(mean(hp), 2),
    sd_hp    = round(sd(hp), 2)
  )
cat("\nDescriptive Statistics by Cylinder Count:\n")
print(summary_by_cyl)

# 4.2: Correlation Analysis among Numeric Variables
numeric_vars <- mtcars %>% select_if(is.numeric)
cor_matrix <- round(cor(numeric_vars), 2)
cat("\nCorrelation Matrix:\n")
print(cor_matrix)

# Visualize the correlation matrix.
corrplot::corrplot(cor_matrix, method = "number", type = "upper")

# -----------------------------------------------------------------------------
# Section 5: Data Visualization
# -----------------------------------------------------------------------------
# 5.1: Bar Plot - Average MPG by Cylinder Count
plot_avg_mpg <- ggplot(summary_by_cyl, aes(x = cyl, y = avg_mpg)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average MPG by Cylinder Count",
    x = "Number of Cylinders",
    y = "Average MPG"
  ) +
  theme_minimal()
print(plot_avg_mpg)
ggsave("avg_mpg_by_cylinders.png", plot = plot_avg_mpg, width = 6, height = 4)

# 5.2: Boxplot - MPG Distribution by Transmission Type
plot_mpg_am <- ggplot(mtcars, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() +
  labs(
    title = "MPG Distribution by Transmission Type",
    x = "Transmission",
    y = "Miles Per Gallon (MPG)"
  ) +
  theme_minimal()
print(plot_mpg_am)
ggsave("mpg_by_transmission.png", plot = plot_mpg_am, width = 6, height = 4)

# 5.3: Scatter Plot - Horsepower vs MPG with Regression Line
plot_hp_mpg <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "tomato", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(
    title = "Scatter Plot of Horsepower vs MPG",
    x = "Horsepower (HP)",
    y = "Miles Per Gallon (MPG)"
  ) +
  theme_minimal()
print(plot_hp_mpg)
ggsave("hp_vs_mpg.png", plot = plot_hp_mpg, width = 6, height = 4)

# -----------------------------------------------------------------------------
# Section 6: Modeling and Prediction
# -----------------------------------------------------------------------------
# 6.1: Linear Regression Model to Predict MPG
model <- lm(mpg ~ wt + hp, data = mtcars)
cat("\nLinear Regression Model Summary:\n")
print(summary(model))

# 6.2: Add Predicted MPG Values to the Dataset
mtcars <- mtcars %>% mutate(predicted_mpg = predict(model, newdata = mtcars))

# 6.3: Scatter Plot - Actual vs. Predicted MPG
plot_actual_pred <- ggplot(mtcars, aes(x = mpg, y = predicted_mpg)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Actual vs. Predicted MPG",
    x = "Actual MPG",
    y = "Predicted MPG"
  ) +
  theme_minimal()
print(plot_actual_pred)
ggsave("actual_vs_predicted_mpg.png", plot = plot_actual_pred, width = 6, height = 4)

# -----------------------------------------------------------------------------
# Section 7: Additional Statistical Analysis
# -----------------------------------------------------------------------------
# 7.1: T-test - Comparing MPG Between Transmission Types
t_test_result <- t.test(mpg ~ am, data = mtcars)
cat("\nT-test Result Comparing MPG by Transmission Type:\n")
print(t_test_result)

# 7.2: Correlation Test - Weight vs. MPG
cor_test <- cor.test(mtcars$wt, mtcars$mpg)
cat("\nCorrelation Test between Weight and MPG:\n")
print(cor_test)

# -----------------------------------------------------------------------------
# Section 8: Save the Cleaned and Enhanced Dataset
# -----------------------------------------------------------------------------
# Optionally, save the modified dataset with predictions to a CSV file.
write.csv(mtcars, "mtcars_enhanced.csv", row.names = FALSE)
cat("\nEnhanced dataset saved as 'mtcars_enhanced.csv'.\n")

# =============================================================================
# End of Script
# =============================================================================
cat("\nData analysis complete. All plots have been saved and outputs printed to console.\n")
