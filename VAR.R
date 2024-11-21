################################################################################
<<<<<<< HEAD
# VAR with all variables
################################################################################

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1] # Y for VAR 

# Modify also the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
=======
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
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[,-1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

<<<<<<< HEAD
# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Initialize the vector to store predictions
=======
# Number of test observations
n_test <- nrow(X_test_matrix)  # Adjusted to match the number of test predictions

# Create vectors to store predictions
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
predictions <- numeric(n_test)

# Perform recursive rolling VAR and predict the next value
for (i in 1:n_test) {
<<<<<<< HEAD
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
=======
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
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_matrix:", nrow(X_train_matrix), "\n")
}

################################################################################
<<<<<<< HEAD
# Confront recursive rolling VAR predictions with actual test values
# Extract the actual values from the test data
actual_values <- Y_test_vec[1:n_test]  # Adjusted to match the number of predictions
=======
# Evaluate VAR predictions against actual values
# Extract the actual values from the test data
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620

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

<<<<<<< HEAD
# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR with multiple variables: Test Actual vs. Predicted Values")
=======
# Create a data frame for comparison
df_test <- data.frame(Actual = actual_values, Predicted = predictions)

# Plot actual vs predicted values for the test set
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR Model: Test Actual vs. Predicted Values")
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
<<<<<<< HEAD
  ggtitle("VAR with multiple variables: Test Residuals")
=======
  ggtitle("VAR Model: Test Residuals")
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

<<<<<<< HEAD
# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR Model with All Predictors",
=======
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR Model",
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
<<<<<<< HEAD
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
=======
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
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

<<<<<<< HEAD
# Display the test errors and error metrics
=======
# Display error metrics
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

<<<<<<< HEAD
# Plot actual vs predicted values for the test set
df_test <- data.frame(Actual = actual_values, Predicted = predictions)
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR with 30 PCs: Test Actual vs. Predicted Values")
=======
################################################################################
# Step 4: Visualizations

# Create a data frame for comparison
df_test <- data.frame(Actual = actual_values, Predicted = predictions)

# Plot actual vs predicted values for the test set
ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("VAR with PCA: Test Actual vs. Predicted Values")
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
<<<<<<< HEAD
  ggtitle("VAR with 30 PCs: Test Residuals")
=======
  ggtitle("VAR with PCA: Test Residuals")
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

<<<<<<< HEAD
# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR Model with 30 PCs",
=======
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for VAR with PCA",
>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
<<<<<<< HEAD
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
=======
ggsave("actual_vs_predicted_values_var_pca_model.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

>>>>>>> c28629b9bc84633072e852470d8e9f08fff22620
