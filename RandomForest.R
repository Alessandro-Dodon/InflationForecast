################################################################################
# Recursive Rolling RF using only the past values as predictors 500 trees
################################################################################

# Initial shifting for training data
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]
Y_train <- CPIULFSL_train[-1]  # Future values for training set

train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Convert to numeric vectors
Y_train_vec <- as.numeric(Y_train)
X_train_vec <- as.numeric(X_train_matrix[, "CPIULFSL"])  # Correctly use CPIULFSL column
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set
X_test_vec <- as.numeric(CPIULFSL_test[-length(CPIULFSL_test)])  # Past values for test set

# Number of test observations
n_test <- 239

# Create vectors to store predictions
rf_predictions <- numeric(n_test)

# Perform recursive rolling RF and predict the next value
for (i in 1:n_test) {
  # Re-prepare the training data (RF does not need standardization)
  X_train_rf <- as.matrix(X_train_vec)
  Y_train_rf <- Y_train_vec
  
  # Fit the Random Forest model using current training data
  rf_model <- randomForest(X_train_rf, Y_train_rf, ntree = 500)
  
  # Predict the next value using the corresponding value from the test set
  new_X <- as.matrix(X_test_vec[i])  # Single-row matrix
  rf_predictions[i] <- predict(rf_model, newdata = new_X)
  
  # Append the actual test value to the training data
  actual_value <- Y_test[i]
  
  # Append the actual test value to the training vectors
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_vec <- c(X_train_vec, X_test_vec[i])
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_vec:", length(X_train_vec), "\n")
}

################################################################################
# Confront recursive rolling RF predictions with actual test values
# Extract the actual values from the test data
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors for the test set
test_errors <- actual_values - rf_predictions

# Compute error metrics for the test set
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display the test errors and error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = rf_predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("RF with one variable: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("RF with one variable: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[1:n_test],
                      Actual = actual_values,
                      Predicted = rf_predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for Recursive RF Model using Past Value and 500 Trees",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_recursive_rf.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Recursive Rolling RF using only the past values as predictors 1 tree
################################################################################

# Initial shifting for training data
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]
Y_train <- CPIULFSL_train[-1]  # Future values for training set

train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Convert to numeric vectors
Y_train_vec <- as.numeric(Y_train)
X_train_vec <- as.numeric(X_train_matrix[, "CPIULFSL"])  # Correctly use CPIULFSL column
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set
X_test_vec <- as.numeric(CPIULFSL_test[-length(CPIULFSL_test)])  # Past values for test set

# Number of test observations
n_test <- 239

# Create vectors to store predictions
rf_predictions <- numeric(n_test)

# Perform recursive rolling RF and predict the next value
for (i in 1:n_test) {
  # Re-prepare the training data (RF does not need standardization)
  X_train_rf <- as.matrix(X_train_vec)
  Y_train_rf <- Y_train_vec
  
  # Fit the Random Forest model using current training data
  rf_model <- randomForest(X_train_rf, Y_train_rf, ntree = 1)
  
  # Predict the next value using the corresponding value from the test set
  new_X <- as.matrix(X_test_vec[i])  # Single-row matrix
  rf_predictions[i] <- predict(rf_model, newdata = new_X)
  
  # Append the actual test value to the training data
  actual_value <- Y_test[i]
  
  # Append the actual test value to the training vectors
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_vec <- c(X_train_vec, X_test_vec[i])
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_vec:", length(X_train_vec), "\n")
}

################################################################################
# Confront recursive rolling RF predictions with actual test values
# Extract the actual values from the test data
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors for the test set
test_errors <- actual_values - rf_predictions

# Compute error metrics for the test set
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display the test errors and error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = rf_predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("RF with one variable: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("RF with one variable: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[1:n_test],
                      Actual = actual_values,
                      Predicted = rf_predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for Recursive RF Model using Past Value and 1 Tree",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_recursive_rf1.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Recursive Rolling RF using all available predictors 500 trees
################################################################################

# Initial shifting for training data
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]
Y_train <- CPIULFSL_train[-1]  # Future values for training set

train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Convert to numeric and matrix format
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239

# Create vectors to store predictions
rf_predictions <- numeric(n_test)

# Perform recursive rolling RF and predict the next value
for (i in 1:n_test) {
  # Re-prepare the training data
  X_train_rf <- X_train_matrix
  Y_train_rf <- Y_train_vec
  
  # Fit the Random Forest model using current training data
  rf_model <- randomForest(X_train_rf, Y_train_rf, ntree = 500)
  
  # Predict the next value using the corresponding row from the test set
  new_X <- matrix(X_test_matrix[i, ], nrow = 1)  # Single-row matrix with all predictors
  rf_predictions[i] <- predict(rf_model, newdata = new_X)
  
  # Append the actual test value to the training data
  actual_value <- Y_test[i]
  
  # Append the actual test value and predictors to the training vectors
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_matrix:", nrow(X_train_matrix), "\n")
}

################################################################################
# Confront recursive rolling RF predictions with actual test values
# Extract the actual values from the test data
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors for the test set
test_errors <- actual_values - rf_predictions

# Compute error metrics for the test set
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display the test errors and error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = rf_predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("RF with all predictors: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("RF with all predictors: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[1:n_test],
                      Actual = actual_values,
                      Predicted = rf_predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for RF Model with All Predictors and 500 Trees",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_recursive_rf_all_predictors.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Recursive Rolling RF using all available predictors 1 tree
################################################################################

# Initial shifting for training data
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]
Y_train <- CPIULFSL_train[-1]  # Future values for training set

train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Convert to numeric and matrix format
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239

# Create vectors to store predictions
rf_predictions <- numeric(n_test)

# Perform recursive rolling RF and predict the next value
for (i in 1:n_test) {
  # Re-prepare the training data
  X_train_rf <- X_train_matrix
  Y_train_rf <- Y_train_vec
  
  # Fit the Random Forest model using current training data
  rf_model <- randomForest(X_train_rf, Y_train_rf, ntree = 1)
  
  # Predict the next value using the corresponding row from the test set
  new_X <- matrix(X_test_matrix[i, ], nrow = 1)  # Single-row matrix with all predictors
  rf_predictions[i] <- predict(rf_model, newdata = new_X)
  
  # Append the actual test value to the training data
  actual_value <- Y_test[i]
  
  # Append the actual test value and predictors to the training vectors
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_matrix:", nrow(X_train_matrix), "\n")
}

################################################################################
# Confront recursive rolling RF predictions with actual test values
# Extract the actual values from the test data
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors for the test set
test_errors <- actual_values - rf_predictions

# Compute error metrics for the test set
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display the test errors and error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = rf_predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("RF with all predictors: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("RF with all predictors: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[1:n_test],
                      Actual = actual_values,
                      Predicted = rf_predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for Recursive RF Model (All Predictors)",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_recursive_rf_all_predictors1.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

