################################################################################
# Lag Selection
################################################################################

# Select optimal lag using information criteria
lag_selection <- VARselect(current_train_matrix, lag.max = 10, type = "const")  # Test up to 10 lags
print(lag_selection$criteria)  # View AIC, BIC, etc.

# Use the lag with the lowest AIC or BIC
optimal_lag <- lag_selection$selection["AIC(n)"]  # Replace "AIC(n)" with "BIC(n)" if you prefer BIC
cat("Optimal lag selected:", optimal_lag, "\n")

################################################################################
# Recursive Rolling VAR using all variables
################################################################################

# Initial shifting for training data
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]
Y_train <- CPIULFSL_train[-1]  # Future values for training set

train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Combine target and predictors into a matrix
Y_train_vec <- as.numeric(Y_train)  # Initialize target variable

# Prepare test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

# Number of test observations
n_test <- nrow(X_test_matrix)  # Adjusted to match the number of test predictions

# Create vectors to store predictions
predictions <- numeric(n_test)

# Perform recursive rolling VAR and predict the next value
for (i in 1:n_test) {
  # Combine current training data (target and predictors)
  current_train_matrix <- cbind(Y_train_vec, X_train_matrix)
  colnames(current_train_matrix) <- c("Y_train_vec", colnames(X_train_matrix))
  
  # Fit the VAR model on the current training data
  var_model <- VAR(current_train_matrix, p = optimal_lag, type = "const")  # select p
  
  # Use the VAR model to forecast the next value
  forecast_result <- predict(var_model, n.ahead = 1)
  prediction <- forecast_result$fcst$Y_train_vec[1, "fcst"]  # Extract forecasted value
  
  # Store the prediction
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test[i]
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_matrix:", nrow(X_train_matrix), "\n")
}

################################################################################
# Evaluate VAR predictions against actual values
# Extract the actual values from the test data
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors for the test set
test_errors <- actual_values - predictions

# Compute error metrics for the test set
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display the test errors and error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

# Create a data frame for comparison
df_test <- data.frame(Actual = actual_values, Predicted = predictions)

# Plot actual vs predicted values for the test set
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR Model: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("VAR Model: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR Model",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_var_model.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# VAR with Recursive Rolling and PCA
################################################################################

# Step 1: Perform PCA on the training data
pca_model <- prcomp(X_train_matrix, center = TRUE, scale. = TRUE)

# Retain the first 30 PCs
num_pcs <- 30
X_train_pcs <- pca_model$x[, 1:num_pcs]  # Training data in the PC space
X_test_pcs <- as.matrix(X_test_matrix) %*% pca_model$rotation[, 1:num_pcs]  # Test data in PC space

# Initialize the training data matrix
current_train_matrix <- cbind(Y_train_vec, X_train_pcs)
colnames(current_train_matrix) <- c("Y_train_vec", paste0("PC", 1:num_pcs))

# Create vector to store predictions
predictions <- numeric(n_test)

################################################################################
# Step 2: Recursive Rolling VAR
for (i in 1:n_test) {
  # Fit VAR model on the current training matrix
  var_model <- VAR(current_train_matrix, p = optimal_lag, type = "const")  # Use optimal lag
  
  # Forecast the next value
  forecast_result <- predict(var_model, n.ahead = 1)
  prediction <- forecast_result$fcst$Y_train_vec[1, "fcst"]  # Extract forecasted value
  
  # Store the prediction
  predictions[i] <- prediction
  
  # Append the actual test observation to the training set
  actual_value <- Y_test[i]  # Dependent variable
  Y_train_vec <- c(Y_train_vec, actual_value)  # Update Y vector
  X_train_pcs <- rbind(X_train_pcs, X_test_pcs[i, ])  # Update PC matrix
  
  # Update the training matrix for the next iteration
  current_train_matrix <- cbind(Y_train_vec, X_train_pcs)
  colnames(current_train_matrix) <- c("Y_train_vec", paste0("PC", 1:num_pcs))
  
  # Debugging: Check sizes
  cat("Iteration:", i, "\n")
  cat("Training data size:", nrow(current_train_matrix), "\n")
}

################################################################################
# Step 3: Evaluate VAR Predictions
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors
test_errors <- actual_values - predictions
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

################################################################################
# Step 4: Visualizations

# Create a data frame for comparison
df_test <- data.frame(Actual = actual_values, Predicted = predictions)

# Plot actual vs predicted values for the test set
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR with PCA: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("VAR with PCA: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR with PCA",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_var_pca_model.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

