library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(flextable)
library(webshot2)

# Load the cleaned dataset
load("cleaned_data.rdata")

# Set a random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
train_index <- sample(1:nrow(merged_data), 0.7 * nrow(merged_data))
train_data <- merged_data[train_index, ]
test_data <- merged_data[-train_index, ]

# Fit the initial model
initial_model <- lm(Likes ~ 1, data = train_data)

# Perform forward stepwise regression manually
forward_model <- initial_model
predictors <- c("Total_Likes", "Comments", "Shares", "Saves", "Following", "Followers", "Length_seconds")
for (var in predictors) {
  formula <- as.formula(paste("Likes ~", paste(c(".", var), collapse = " + ")))
  new_model <- update(forward_model, formula, data = train_data, na.action = na.exclude)
  if (AIC(new_model) < AIC(forward_model)) {
    forward_model <- new_model
  }
}

# Extract the selected variables from the forward stepwise model
selected_vars <- all.vars(formula(forward_model))

# Subset the test_data to include only the selected variables and the response variable
test_data_subset <- test_data[, c(selected_vars, "Likes")]

# Make predictions using the forward stepwise model on the subsetted test_data
forward_predictions <- predict(forward_model, newdata = test_data_subset)

# Calculate evaluation metrics for the forward stepwise model
forward_mae <- mean(abs(test_data_subset$Likes - forward_predictions), na.rm = TRUE)
forward_mse <- mean((test_data_subset$Likes - forward_predictions)^2, na.rm = TRUE)
forward_r_squared <- 1 - sum((test_data_subset$Likes - forward_predictions)^2, na.rm = TRUE) / sum((test_data_subset$Likes - mean(test_data_subset$Likes, na.rm = TRUE))^2, na.rm = TRUE)

# Print the evaluation metrics
cat("Forward Stepwise Model - MAE:", forward_mae, "MSE:", forward_mse, "R-squared:", forward_r_squared, "\n")

# Create an enhanced scatter plot of actual vs. predicted likes with log base scaling
ggplot(test_data_subset, aes(x = Likes, y = forward_predictions)) +
  geom_point(alpha = 0.6, size = 2, color = "#4E79A7") +
  geom_smooth(method = "lm", se = FALSE, color = "#E15759", size = 1) +
  labs(title = "Actual vs. Predicted Likes",
       subtitle = "Forward Stepwise Model",
       x = "Actual Likes (Log Scale)",
       y = "Predicted Likes (Log Scale)") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "gray90", size = 0.3),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 12, margin = ggplot2::margin(t = 10))) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  annotate("text", x = min(test_data_subset$Likes), y = max(forward_predictions),
           label = "Perfect Prediction", color = "#F28E2B", size = 4, hjust = 0, vjust = 1)

# Extract the coefficients of the best model found by forward stepwise selection
best_model_coefficients <- coef(forward_model)

# Create a string representation of the best model equation
best_model_equation <- paste("Likes =",
                             paste(sprintf("%.2f * %s", best_model_coefficients[-1], names(best_model_coefficients)[-1]), collapse = " + "),
                             "+", sprintf("%.2f", best_model_coefficients[1]))
best_model_equation
# Save the scatter plot (stepwise model) as a high-resolution TIFF image
ggsave("stepwise_model_plot.tiff", plot = last_plot(), device = "tiff", width = 10, height = 8, units = "in", dpi = 300)

