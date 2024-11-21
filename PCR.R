################################################################################
# PCA
################################################################################

# Step 1: Perform PCA
pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)

# Step 2: Extract the standard deviations of the principal components
std_devs <- pca_model$sdev

# Step 3: Calculate the variance explained by each principal component
variances <- std_devs^2
proportion_variance <- variances / sum(variances)

# Step 4: Calculate cumulative variance explained
cumulative_variance <- cumsum(proportion_variance)

# Print the proportion of variance explained by each component
cat("Proportion of Variance Explained by Each Principal Component:\n")
print(proportion_variance)

# Print the cumulative variance explained
cat("\nCumulative Variance Explained by Principal Components:\n")
print(cumulative_variance)

################################################################################
# PCR with only the first PC
################################################################################

# Re-prepare the data with the correct shifts
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take the first value out to shift every observation
Y_train <- CPIULFSL_train[-1]  # Y for PCR

# Modify the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
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

# Perform recursive rolling PCR and predict the next value
for (i in 1:n_test) {
  # Step 1: Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Step 2: Apply PCA to the standardized training data
  pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
  X_train_pca <- pca_model$x[, 1]  # Use first principal component
  
  # Step 3: Fit the PCR model using the first principal component
  pcr_model <- lm(Y_train_standardized ~ X_train_pca)
  
  # Step 4: Predict using the PCR model
  new_X <- as.numeric(X_test_matrix[i, ])
  new_X_standardized <- (new_X - mean_X_train) / sd_X_train
  new_X_standardized <- matrix(new_X_standardized, nrow = 1)  # Ensure it has two dimensions
  colnames(new_X_standardized) <- colnames(X_train_matrix)  # Ensure column names match
  new_X_pca <- predict(pca_model, newdata = as.data.frame(new_X_standardized))[, 1]
  prediction_standardized <- predict(pcr_model, newdata = data.frame(new_X_pca))
  
  # Step 5: De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Step 6: Append the actual test value to the training data after prediction
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
# Confront recursive rolling PCR predictions with actual test values
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
  ggtitle("PCR with 1 Principal Component: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("PCR with 1 Principal Component: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for PCR with First Principal Component",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_pcr_first_pc.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# PCR with the first 2 PCs
################################################################################

# Re-prepare the data with the correct shifts
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take the first value out to shift every observation
Y_train <- CPIULFSL_train[-1]  # Y for PCR

# Modify the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
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

# Perform recursive rolling PCR and predict the next value
for (i in 1:n_test) {
  # Step 1: Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Step 2: Apply PCA to the standardized training data
  pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
  X_train_pca <- pca_model$x[, 1:2]  # Use first 2 principal components
  
  # Step 3: Fit the PCR model using the first 2 principal components
  pcr_model <- lm(Y_train_standardized ~ X_train_pca)
  
  # Step 4: Predict using the PCR model
  new_X <- as.numeric(X_test_matrix[i, ])
  new_X_standardized <- (new_X - mean_X_train) / sd_X_train
  new_X_standardized <- matrix(new_X_standardized, nrow = 1)  # Ensure it has two dimensions
  colnames(new_X_standardized) <- colnames(X_train_matrix)  # Ensure column names match
  new_X_pca <- predict(pca_model, newdata = as.data.frame(new_X_standardized))[, 1:2]
  prediction_standardized <- predict(pcr_model, newdata = data.frame(new_X_pca))
  
  # Step 5: De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Step 6: Append the actual test value to the training data after prediction
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
# Confront recursive rolling PCR predictions with actual test values
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
  ggtitle("PCR with 2 Principal Components: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("PCR with 2 Principal Components: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for PCR with First Two Principal Components",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_pcr_two_pcs.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# PCR with first 5 PCs
################################################################################

# Re-prepare the data with the correct shifts
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take the first value out to shift every observation
Y_train <- CPIULFSL_train[-1]  # Y for PCR

# Modify the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
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

# Perform recursive rolling PCR and predict the next value
for (i in 1:n_test) {
  # Step 1: Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Step 2: Apply PCA to the standardized training data
  pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
  X_train_pca <- pca_model$x[, 1:5]  # Use first 5 principal components
  
  # Step 3: Fit the PCR model using the first 5 principal components
  pcr_model <- lm(Y_train_standardized ~ X_train_pca)
  
  # Step 4: Predict using the PCR model
  new_X <- as.numeric(X_test_matrix[i, ])
  new_X_standardized <- (new_X - mean_X_train) / sd_X_train
  new_X_standardized <- matrix(new_X_standardized, nrow = 1)  # Ensure it has two dimensions
  colnames(new_X_standardized) <- colnames(X_train_matrix)  # Ensure column names match
  new_X_pca <- predict(pca_model, newdata = as.data.frame(new_X_standardized))[, 1:5]
  prediction_standardized <- predict(pcr_model, newdata = data.frame(new_X_pca))
  
  # Step 5: De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Step 6: Append the actual test value to the training data after prediction
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
# Confront recursive rolling PCR predictions with actual test values
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
  ggtitle("PCR with 5 Principal Components: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("PCR with 5 Principal Components: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for PCR with First Five Principal Components",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_pcr_five_pcs.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# PCR with all PCs (equal to OLS with all variables)
################################################################################

# Re-prepare the data with the correct shifts
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take the first value out to shift every observation
Y_train <- CPIULFSL_train[-1]  # Y for PCR

# Modify the matrix for the training data (this took out the last value, so it is symmetric and represents the "past" values)
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

# Perform recursive rolling PCR and predict the next value
for (i in 1:n_test) {
  # Step 1: Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Step 2: Apply PCA to the standardized training data
  pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
  X_train_pca <- pca_model$x[, 1:ncol(X_train_matrix)]  # Use all principal components
  
  # Step 3: Fit the PCR model using all principal components
  pcr_model <- lm(Y_train_standardized ~ X_train_pca)
  
  # Step 4: Predict using the PCR model
  new_X <- as.numeric(X_test_matrix[i, ])
  new_X_standardized <- (new_X - mean_X_train) / sd_X_train
  new_X_standardized <- matrix(new_X_standardized, nrow = 1)  # Ensure it has two dimensions
  colnames(new_X_standardized) <- colnames(X_train_matrix)  # Ensure column names match
  new_X_pca <- predict(pca_model, newdata = as.data.frame(new_X_standardized))[, 1:ncol(X_train_matrix)]
  prediction_standardized <- predict(pcr_model, newdata = data.frame(new_X_pca))
  
  # Step 5: De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Step 6: Append the actual test value to the training data after prediction
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
# Confront recursive rolling PCR predictions with actual test values
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
  ggtitle("PCR with multiple variables: Test Actual vs. Predicted Values")

# Residual plot for the test set
ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("PCR with multiple variables: Test Residuals")

# Plot actual vs predicted values as time series
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Create the time series plot
actual_vs_predicted_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values for PCR with All Principal Components",
       x = "Date",
       y = "CPIULFSL",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as a high-resolution PDF
ggsave("actual_vs_predicted_values_pcr_all_pcs.pdf", plot = actual_vs_predicted_plot, width = 10, height = 8, dpi = 300, units = "in")

