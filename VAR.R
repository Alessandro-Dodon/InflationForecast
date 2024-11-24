################################################################################
# VAR with all variables
################################################################################

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1] # Y for VAR 

# Modify the matrix for training data (removing date and CPIULFSL columns)
train_data_holdout_no_date <- train_data_holdout[,-1]  # Remove date column
X_train_matrix <- train_data_holdout_no_date[,-which(colnames(train_data_holdout_no_date) == "CPIULFSL")]  # Remove CPIULFSL column
X_train_matrix <- head(X_train_matrix, -1)  # Adjust for shifting

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- test_data_holdout_no_date[,-which(colnames(test_data_holdout_no_date) == "CPIULFSL")]
X_test_matrix <- head(X_test_matrix, -1)

# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Initialize the vector to store predictions
predictions <- numeric(n_test)

# Perform recursive rolling VAR and predict the next value
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Combine Y and X into a single data frame for VAR model
  train_data_standardized <- data.frame(Y = Y_train_standardized, X_train_standardized)
  
  # Fit the VAR model using current standardized training data
  var_model <- VAR(train_data_standardized, p = 1)  # Adjust p (lag order) as needed
  
  # Forecast the next step using the VAR model
  var_forecast <- predict(var_model, n.ahead = 1)
  prediction_standardized <- var_forecast$fcst$Y[1, "fcst"]  # Forecasted value for Y
  
  # De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test_vec[i]
  
  # Append the actual test value to the training vectors
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_matrix <- rbind(X_train_matrix, as.numeric(X_test_matrix[i, ]))
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_matrix:", nrow(X_train_matrix), "\n")
}

################################################################################
# Confront recursive rolling VAR predictions with actual test values
# Extract the actual values from the test data
actual_values <- Y_test_vec[1:n_test]  # Adjusted to match the number of predictions

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

# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR with multiple variables: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("VAR with multiple variables: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR Model with All Predictors",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_var_all_predictors.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")


################################################################################
# Determine the Optimal Lag Order for VAR
################################################################################

# Combine Y_train_vec and X_train_matrix into a single data frame
train_data_combined <- data.frame(Y = Y_train_vec, X_train_matrix)

# Perform lag selection
lag_selection <- VARselect(train_data_combined, lag.max = 12, type = "const")  # Adjust lag.max if needed

# Display the results of lag selection
cat("Optimal lags based on AIC:\n")
cat(lag_selection$selection["AIC(n)"], "\n")

cat("Optimal lags based on BIC:\n")
cat(lag_selection$selection["SC(n)"], "\n")

cat("Optimal lags based on HQC:\n")
cat(lag_selection$selection["HQ(n)"], "\n")

################################################################################
# VAR with PCA (First 30 PCs)
################################################################################

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1] # Y for VAR 

# Modify also the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Apply PCA to the training data (assuming 30 components)
train_pca <- prcomp(X_train_matrix, center = TRUE, scale. = TRUE)
train_pca_matrix <- train_pca$x[, 1:30]  # Select first 30 PCs

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

# Apply PCA to the test data (assuming 30 components)
test_pca <- predict(train_pca, newdata = X_test_matrix)[, 1:30]  # Transform test data

# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Initialize the vector to store predictions
predictions <- numeric(n_test)

# Perform recursive rolling VAR and predict the next value
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_train_pca <- colMeans(train_pca_matrix)
  sd_train_pca <- apply(train_pca_matrix, 2, sd)
  train_pca_standardized <- scale(train_pca_matrix, center = mean_train_pca, scale = sd_train_pca)
  
  # Check dimensions before combining
  if (length(Y_train_standardized) != nrow(train_pca_standardized)) {
    stop("Mismatch in dimensions between Y_train_standardized and train_pca_standardized")
  }
  
  # Combine Y and standardized PCA-transformed X for VAR model
  train_data_standardized <- data.frame(Y = Y_train_standardized, train_pca_standardized)
  
  # Fit the VAR model using current standardized training data
  var_model <- VAR(train_data_standardized, p = 1)  # Adjust p (lag order) as needed
  
  # Forecast the next step using the VAR model
  current_test_pca <- test_pca[i, , drop = FALSE]
  test_pca_standardized <- scale(current_test_pca, center = mean_train_pca, scale = sd_train_pca)
  var_forecast <- predict(var_model, n.ahead = 1)
  prediction_standardized <- var_forecast$fcst$Y[1, "fcst"]  # Forecasted value for Y
  
  # De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test_vec[i]
  
  # Append the actual test value to the training vectors
  Y_train_vec <- c(Y_train_vec, actual_value)
  train_pca_matrix <- rbind(train_pca_matrix, test_pca[i, , drop = FALSE])  # Append PCA-transformed test observation
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of train_pca_matrix:", nrow(train_pca_matrix), "\n")
}

################################################################################
# Confront recursive rolling VAR predictions with actual test values
# Extract the actual values from the test data
actual_values <- Y_test_vec[1:n_test]  # Adjusted to match the number of predictions

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

# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR with 30 PCs: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("VAR with 30 PCs: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR Model with 30 PCs",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_var_30pcs.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Determine the Optimal Lag Order for VAR
################################################################################

# Combine Y_train_vec and X_train_matrix into a single data frame
train_data_combined <- data.frame(Y = Y_train_vec, train_pca_matrix)

# Perform lag selection
lag_selection <- VARselect(train_data_combined, lag.max = 12, type = "const")  # Adjust lag.max if needed

# Display the results of lag selection
cat("Optimal lags based on AIC:\n")
cat(lag_selection$selection["AIC(n)"], "\n")

cat("Optimal lags based on BIC:\n")
cat(lag_selection$selection["SC(n)"], "\n")

cat("Optimal lags based on HQC:\n")
cat(lag_selection$selection["HQ(n)"], "\n")

