# Load required libraries
library(dplyr)
library(caret)

# Remove missing values
complete_data <- na.omit(merged_data)

# Set a random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
train_index <- sample(1:nrow(complete_data), 0.7 * nrow(complete_data))
train_data <- complete_data[train_index, ]
test_data <- complete_data[-train_index, ]

# Define the full model with all predictors
full_model <- lm(Likes ~ Total_Likes + Comments + Shares + Saves + Following + Followers + Length, data = train_data)

# Perform backward stepwise regression
backward_model <- step(full_model, direction = "backward")

# Extract the selected variables from the backward stepwise model
selected_vars <- all.vars(formula(backward_model))[-1]

# Make predictions using the backward stepwise model on the test_data
backward_predictions <- predict(backward_model, newdata = test_data[, c(selected_vars, "Likes")])

# Calculate evaluation metrics for the backward stepwise model
backward_mae <- mean(abs(test_data$Likes - backward_predictions), na.rm = TRUE)
backward_mse <- mean((test_data$Likes - backward_predictions)^2, na.rm = TRUE)
backward_r_squared <- 1 - sum((test_data$Likes - backward_predictions)^2, na.rm = TRUE) / sum((test_data$Likes - mean(test_data$Likes, na.rm = TRUE))^2, na.rm = TRUE)

# Print the evaluation metrics
cat("Backward Stepwise Model - MAE:", backward_mae, "MSE:", backward_mse, "R-squared:", backward_r_squared, "\n")

# Print the coefficients of the backward stepwise model
print(summary(backward_model))

# Create an enhanced scatter plot of actual vs. predicted likes with log base scaling
da_plot <- ggplot(test_data, aes(x = Likes, y = backward_predictions)) +
  geom_point(alpha = 0.6, size = 2, shape = 16, color = "#375A80") +  # Use solid circle shape (shape = 16)
  geom_smooth(method = "lm", se = FALSE, color = "#B14044", size = 1) +
  labs(
    title = "Actual vs. Predicted Likes",
    subtitle = "Backward Stepwise Model",
    x = "Actual Likes (Log Scale)",
    y = "Predicted Likes (Log Scale)",
    caption = paste0("Figure 2. Comparison of actual and predicted likes using a Backward Stepwise Model.\nBest Fit Function: ", expression(Likes^hat == "8.28 *Comments + 9.36 *Shares + 1744.56 *Length - 51082.56")) # Add caption
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 12, margin = ggplot2::margin(t = 10))
  ) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma)

# Extract the coefficients of the best model found by forward stepwise selection
best_model_coefficients <- coef(backward_model)

# Create a string representation of the best model equation
best_model_equation <- paste("Likes =",
                             paste(sprintf("%.2f * %s", best_model_coefficients[-1], names(best_model_coefficients)[-1]), collapse = " + "),
                             "+", sprintf("%.2f", best_model_coefficients[1]))

best_model_equation

# Save the scatter plot (stepwise model) as a high-resolution TIFF image
ggsave("back_stepwise_model_plot.tiff", plot = da_plot, device = "tiff", width = 18, height = 7, units = "in", dpi = 300)

