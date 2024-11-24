################################################################################
# Lasso with lambda close to zero
################################################################################

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1] # Y for Lasso

# Modify also the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Create vectors to store predictions
predictions <- numeric(n_test)

# Define the lambda for Lasso regression
lambda <- 0.0001

# Perform recursive rolling Lasso regression and predict the next value
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Fit the Lasso regression model using current standardized training data
  model <- glmnet(X_train_standardized, Y_train_standardized, alpha = 1, lambda = lambda, standardize = FALSE)
  
  # Use the last row of X_train_matrix for prediction
  new_X <- scale(matrix(as.numeric(X_test_matrix[i, ]), nrow=1), center = mean_X_train, scale = sd_X_train)
  prediction_standardized <- predict(model, newx = new_X)
  
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
# Confront recursive rolling Lasso predictions with actual test values
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
  ggtitle("Lasso Regression with multiple variables: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Lasso Regression with multiple variables: Test Residuals")

# Plotting actual vs predicted values for the test set
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for Lasso Regression with Lambda = 0.0001",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_lasso_lambda_0001.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Lasso with lambda close to infinity
################################################################################

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1] # Y for Lasso

# Modify also the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Create vectors to store predictions
predictions <- numeric(n_test)

# Define the lambda for Lasso regression
lambda <- 1000000

# Perform recursive rolling Lasso regression and predict the next value
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Fit the Lasso regression model using current standardized training data
  model <- glmnet(X_train_standardized, Y_train_standardized, alpha = 1, lambda = lambda, standardize = FALSE)
  
  # Use the last row of X_train_matrix for prediction
  new_X <- scale(matrix(as.numeric(X_test_matrix[i, ]), nrow=1), center = mean_X_train, scale = sd_X_train)
  prediction_standardized <- predict(model, newx = new_X)
  
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
# Confront recursive rolling Lasso predictions with actual test values
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
  ggtitle("Lasso Regression with multiple variables: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Lasso Regression with multiple variables: Test Residuals")

# Plotting actual vs predicted values for the test set
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for Lasso Regression with Lambda = 1,000,000",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_lasso_lambda_1000000.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Lasso with lambda set around optimal theoretical value
################################################################################

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1] # Y for Lasso

# Modify also the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Create vectors to store predictions
predictions <- numeric(n_test)

# Define the lambda for Lasso regression
lambda <- 121 / sqrt(479)

# Perform recursive rolling Lasso regression and predict the next value
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Fit the Lasso regression model using current standardized training data
  model <- glmnet(X_train_standardized, Y_train_standardized, alpha = 1, lambda = lambda, standardize = FALSE)
  
  # Use the last row of X_train_matrix for prediction
  new_X <- scale(matrix(as.numeric(X_test_matrix[i, ]), nrow=1), center = mean_X_train, scale = sd_X_train)
  prediction_standardized <- predict(model, newx = new_X)
  
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
# Confront recursive rolling Lasso predictions with actual test values
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
  ggtitle("Lasso Regression with multiple variables: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Lasso Regression with multiple variables: Test Residuals")

# Plotting actual vs predicted values for the test set
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for Lasso Regression with Optimal Estimated Lambda",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_lasso_optimal_lambda.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Lasso with lambda set around optimal theoretical value (second guess)
################################################################################

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1] # Y for Lasso

# Modify also the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Create vectors to store predictions
predictions <- numeric(n_test)

# Define the lambda for Lasso regression
lambda <- 0.01

# Perform recursive rolling Lasso regression and predict the next value
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Fit the Lasso regression model using current standardized training data
  model <- glmnet(X_train_standardized, Y_train_standardized, alpha = 1, lambda = lambda, standardize = FALSE)
  
  # Use the last row of X_train_matrix for prediction
  new_X <- scale(matrix(as.numeric(X_test_matrix[i, ]), nrow=1), center = mean_X_train, scale = sd_X_train)
  prediction_standardized <- predict(model, newx = new_X)
  
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
# Confront recursive rolling Lasso predictions with actual test values
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
  ggtitle("Lasso Regression with multiple variables: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Lasso Regression with multiple variables: Test Residuals")

# Plotting actual vs predicted values for the test set
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for Lasso Model with Lambda = 0.01",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_lasso_lambda_001.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

