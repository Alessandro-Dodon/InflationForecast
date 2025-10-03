################################################################################
#                           Pre-processing Script                              
################################################################################

# Script Name:        pre_processing.R
# Author:             Alessandro Dodon
# Last Modified:      2025-05-07
#
# Description:        Pre-processing required for the FRED-MD dataset before running
#                     any forecasting model. This includes data cleaning, transformation,
#                     visualization, and basic PCA diagnostics.
#
# Input Data:         'current.csv' (FRED-MD monthly dataset)
# Output:             Summary tables printed to console; plots saved as PDFs
#
# Usage:              Run the script sequentially. All results will be printed to the 
#                     console and saved in the relative directory as PDF files.
#
# Dependencies:       All required packages are listed and loaded in the first section.
#
# Further Reference:  See the associated presentation slides and user guide in the GitHub repo.

################################################################################
# Prepare packages
################################################################################

#install.packages("stats")
#install.packages("zoo")
#install.packages("readr")
#install.packages("pracma")
#install.packages("reshape2")
#install.packages("utils")
#install.packages("viridis")
#install.packages("devtools")
#install.packages("imputeTS")
#install.packages("forecast")
#install.packages("dplyr")
#install.packages("glmnet")
#install.packages("randomForest")
#install.packages("vars")
#install.packages("pls")
#install.packages("pheatmap")
#install.packages("tidyverse")

# Install fbi package from github
#devtools::install_github("cykbennie/fbi") 

# Load packages quietly
suppressWarnings(suppressMessages({
  
  library(pls)
  library(stats)
  library(glmnet)
  library(dplyr)
  library(forecast)
  library(tidyverse)
  library(readr)
  library(lubridate)
  library(zoo)
  library(ggplot2)
  library(reshape2)
  library(viridis)
  library(fbi) # <----- fbi package from github
  library(utils)
  library(imputeTS)
  library(randomForest)
  library(vars)
  library(pheatmap)
  
}))

################################################################################
# Start by loading the monthly data 
################################################################################

# Set wd and load dataset
file_path <- "./current.csv"
dataset <- read.csv(file_path)

# Load transformed data
transformed_data <- fredmd(file = file_path, transform = TRUE)

print("Transformed Data:")
print(head(transformed_data))

# Load original (non-transformed) data
original_data <- fredmd(file = file_path, transform = FALSE)
print("Original Data:")
print(head(original_data))

# Check the structure and summary of transformed_data
str(transformed_data)
summary(transformed_data)

# Check the structure and summary of original_data
str(original_data)
summary(original_data)

################################################################################
# Describing variables in the dataset
################################################################################

data(fredmd_description) 
print(fredmd_description)

################################################################################
# Original data inspection (checking for missing values)
################################################################################

# Identify rows with NA values
na_rows_original_data <- which(rowSums(is.na(original_data)) > 0)

# Extract the dates corresponding to those rows
dates_with_na_original_data <- original_data$date[na_rows_original_data]

# Print the dates with NA values
print("Dates with NA values for original data:")
print(dates_with_na_original_data)

# Transformed data inspection (check for missing values)
# Identify rows with NA values
na_rows_transformed_data <- which(rowSums(is.na(transformed_data)) > 0)

# Extract the dates corresponding to those rows
dates_with_na_transformed_data <- transformed_data$date[na_rows_transformed_data]

# Print the dates with NA values
print("Dates with NA values for transformed data:")
print(dates_with_na_transformed_data)

################################################################################
# Counting missing values 
################################################################################

# Function to count and locate NA values in a dataset
count_and_locate_nas <- function(data) {
  # Get positions of NAs
  na_positions <- which(is.na(data), arr.ind = TRUE)
  
  # Extract column names and row names (dates) with NAs
  variables_with_nas <- colnames(data)[na_positions[, 2]]
  dates_with_nas <- data$date[na_positions[, 1]]
  
  # Create a data frame with the results
  na_details <- data.frame(Date = dates_with_nas, Variable = variables_with_nas)
  
  return(na_details)
}

# Check for 'date' column and ensure it doesn't contain NAs
if (!("date" %in% names(original_data)) || any(is.na(original_data$date))) {
  stop("The 'date' column is missing or contains NA values in the original data.")
}
if (!("date" %in% names(transformed_data)) || any(is.na(transformed_data$date))) {
  stop("The 'date' column is missing or contains NA values in the transformed data.")
}

# Count and locate NAs in original data
na_details_original_data <- count_and_locate_nas(original_data)

# Count and locate NAs in transformed data
na_details_transformed_data <- count_and_locate_nas(transformed_data)

# Print the results
print("Details of NAs in original data:")
print(na_details_original_data)

print("Details of NAs in transformed data:")
print(na_details_transformed_data)

################################################################################
# Cleaning dataset 
################################################################################

# Convert the fredmd data frame to a regular data frame if not already done
transformed_data_df <- as.data.frame(transformed_data)

# Remove rows where date is before 1960-01-01
transformed_data_cleaned <- transformed_data_df[as.Date(transformed_data_df$date) >= as.Date("1960-01-01"), ]

# Remove last row with NAs
transformed_data_cleaned <- transformed_data_cleaned[as.Date(transformed_data_cleaned$date) <= as.Date("2024-01-01"), ]

# Remove specific columns with too many NAs
columns_to_remove <- c("ACOGNO", "ANDENOx", "TWEXAFEGSMTHx", "UMCSENTx", "VIXCLSx")
transformed_data_cleaned <- transformed_data_cleaned[, !names(transformed_data_cleaned) %in% columns_to_remove]

# Display the first few rows of the cleaned data
head(transformed_data_cleaned)

# NOTE:
# Start from 1960-01-01, end in 2024-01-01 and take out variables that have too 
# many missing values in a row.

################################################################################
# Missing values for data transformed cleaned
# Count and locate NAs in transformed data cleaned
na_details_transformed_data_cleaned <- count_and_locate_nas(transformed_data_cleaned)

print("Details of NAs in transformed data cleaned:")
print(na_details_transformed_data_cleaned)

################################################################################
# Cleaning dataset from the last NAs (use SMA for the remaining NAs values)
# Ensure transformed_data_cleaned is a dataframe
transformed_data_cleaned <- as.data.frame(transformed_data_cleaned)

# Define the function to replace NA values with simple moving average
sma_replace_na <- function(x, n = 3) {
  # Ensure x is a numeric vector
  x <- as.numeric(x)
  
  # Compute the moving average with a window size of 'n', ignoring NA values
  sma <- rollapply(x, width = n, FUN = function(y) mean(y, na.rm = TRUE), fill = NA, align = "right")
  
  # Replace the NA values in the original series with the computed moving average values
  x[is.na(x)] <- sma[is.na(x)]
  
  return(x)
}

# Apply the function to each column except the date column
for (col in names(transformed_data_cleaned)) {
  if (col != "date") {
    transformed_data_cleaned[[col]] <- sma_replace_na(transformed_data_cleaned[[col]])
  }
}

# Display the cleaned dataframe
print(transformed_data_cleaned)

################################################################################
# Re-check missing values for data transformed cleaned
# Count and locate NAs in transformed data cleaned
na_details_transformed_data_cleaned <- count_and_locate_nas(transformed_data_cleaned)

print("Details of NAs in transformed data cleaned:")
print(na_details_transformed_data_cleaned)

################################################################################
# Take COVID-19 years out
################################################################################

# Remove the years
transformed_data_cleaned_no_COVID <- transformed_data_cleaned[as.Date(transformed_data_cleaned$date) <= as.Date("2019-12-01"), ]

# Display the first few rows of the cleaned data
head(transformed_data_cleaned_no_COVID)

################################################################################
# Lagging and Splitting the Data
################################################################################

# Define X (predictors, past values) and Y (target, future values) with the date column retained
X_before_splitting <- transformed_data_cleaned_no_COVID[-nrow(transformed_data_cleaned_no_COVID), ]  
Y_before_splitting <- transformed_data_cleaned_no_COVID[-1, c("date", "CPIULFSL")] # <----------- Change here

################################################################################
# Holdout Method for Splitting Data
################################################################################

# Define the split date (around 70%)
split_date <- as.Date("2000-01-01")

# Split the data based on the date
train_indices <- X_before_splitting$date < split_date
test_indices <- X_before_splitting$date >= split_date

# Split X and Y into train and test sets (date column still included for both X and Y)
X_train_with_date <- X_before_splitting[train_indices, ]
Y_train_with_date <- Y_before_splitting[train_indices, ]

X_test_with_date <- X_before_splitting[test_indices, ]
Y_test_with_date <- Y_before_splitting[test_indices, ]

# Remove the date column after splitting for X and Y
X_train <- X_train_with_date[, -1]  # Remove the date column
Y_train <- Y_train_with_date[, "CPIULFSL"] # <----------- Change here

X_test <- X_test_with_date[, -1]  # Remove the date column
Y_test <- Y_test_with_date[, "CPIULFSL"] # <----------- Change here

################################################################################
# Verify the split
################################################################################

# Check the dimensions of the splits (X) and (Y)
cat("Training Data Date Range (X):\n")
print(range(X_train_with_date$date))

cat("\nTest Data Date Range (X):\n")
print(range(X_test_with_date$date))

cat("Training Data Date Range (Y):\n")
print(range(Y_train_with_date$date))

cat("\nTest Data Date Range (Y):\n")
print(range(Y_test_with_date$date))

# Check the dimensions of the splits (X) and (Y)
cat("\nTraining Data Dimensions:\n")
print(dim(X_train))
cat(length(Y_train), "\n")  # Check length of Y_train

cat("\nTest Data Dimensions:\n")
print(dim(X_test))
cat(length(Y_test), "\n")  # Check length of Y_test

# NOTE:  
# To analyze a different target variable, use the Find and Replace function in R.  
# Click on the magnifying glass icon, type CPIULFSL in "Find", enter the desired 
# variable in "Replace" (e.g., INDPRO for industrial production), and click "All".
# Then run any desired model.

################################################################################
# View variable of interest (CPIULFSL)
################################################################################

# Extract the full column for CPIULFSL
cpi_column <- dataset$CPIULFSL

# View the first few values to verify
head(cpi_column)

# Print details 
print(fredmd_description[120,])

################################################################################
# Function to create a plot for variable of interest (CPIULFSL) 
################################################################################

plot_variable <- function(data, var_name, title_suffix) { 
  # Use backticks to handle special characters in column names
  ggplot(data, aes_string(x = "date", y = paste0("`", var_name, "`"))) +
    geom_line() +
    labs(title = paste("Plot of", var_name, title_suffix), x = "Date", y = var_name) +
    theme_light() +
    theme(panel.border = element_blank(),  # Remove outside borders
          plot.background = element_blank(),  # Remove grey background
          panel.background = element_blank())
}

# Function to create an autocorrelation plot for a single variable
plot_autocorrelation <- function(data, var_name, title_suffix, remove_first_lag = FALSE) {
  # Extract the variable data and remove NA values
  var_data <- na.omit(data[[var_name]])
  
  # Create an autocorrelation plot
  autocorr <- acf(var_data, lag.max = 30, plot = FALSE)  # Use first 30 lags
  if (remove_first_lag) {
    acf_df <- data.frame(Lag = autocorr$lag[-1], Autocorrelation = autocorr$acf[-1])  # Remove first lag
    title_suffix <- paste(title_suffix, "(First Lag Removed)")  # Proper title formatting
  } else {
    acf_df <- data.frame(Lag = autocorr$lag, Autocorrelation = autocorr$acf)
  }
  
  # Calculate the confidence interval
  ci <- qnorm((1 + 0.95) / 2) / sqrt(length(var_data))
  
  # Plot using ggplot2
  ggplot(acf_df, aes(x = Lag, y = Autocorrelation)) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "red") +
    geom_segment(aes(xend = Lag, yend = 0), color = "black") +
    geom_point(color = "black") +
    labs(title = paste("Autocorrelation of", var_name, title_suffix), x = "Lag", y = "Autocorrelation") +
    theme_light() +
    theme(panel.border = element_blank(),  # Remove outside borders
          plot.background = element_blank(),  # Remove grey background
          panel.background = element_blank())
}

################################################################################
# Plotting for variable of interest (CPIULFSL)
################################################################################

output_pdf <- "inflation_plots.pdf"
pdf(output_pdf, width = 10, height = 8)

suppressWarnings({
  # Plot time series for CPIULFSL (original data)
  original_time_series <- plot_variable(original_data, "CPIULFSL", "Original Data")
  print(original_time_series)
  
  # Plot autocorrelation for CPIULFSL (original data)
  original_autocorrelation <- plot_autocorrelation(original_data, "CPIULFSL", "Original Data", remove_first_lag = FALSE)
  print(original_autocorrelation)
  
  # Plot time series for CPIULFSL (transformed data)
  transformed_time_series <- plot_variable(transformed_data, "CPIULFSL", "Transformed Data")
  print(transformed_time_series)
  
  # Plot autocorrelation for CPIULFSL (transformed data) with first lag removed
  transformed_autocorrelation <- plot_autocorrelation(transformed_data, "CPIULFSL", "Transformed Data", remove_first_lag = TRUE)
  print(transformed_autocorrelation)
})

dev.off()  # Close the PDF device

################################################################################
# Plotting before and after ALL time series into a single PDF
################################################################################

# Function to plot all variables into a single PDF
plot_all_variables_pdf <- function(data, title_suffix, output_pdf) {
  pdf(output_pdf, width = 10, height = 8)
  vars <- names(data)[-1]  # Exclude the first column (date)
  
  for (var in vars) {
    suppressWarnings({
      p <- plot_variable(data, var, title_suffix)
      print(p)  # Print each plot to the PDF
    })
  }
  dev.off()  # Close the PDF device
}

# Set your output directory
output_dir <- "time_series_plots"
dir.create(output_dir, showWarnings = FALSE)

# Plot and save all variables into a single PDF for original data
plot_all_variables_pdf(original_data, "Original Data", file.path(output_dir, "time_series_original.pdf"))

# Plot and save all variables into a single PDF for transformed data
plot_all_variables_pdf(transformed_data, "Transformed Data", file.path(output_dir, "time_series_transformed.pdf"))

################################################################################
# Plotting before and after the autocorrelation of ALL time series into a single PDF
################################################################################

# Function to plot all autocorrelations into a single PDF
plot_all_autocorrelations_pdf <- function(data, title_suffix, output_pdf, remove_first_lag = FALSE) {
  pdf(output_pdf, width = 10, height = 8)
  vars <- names(data)[-1]  # Exclude the first column (date)
  
  for (var in vars) {
    suppressWarnings({
      p <- plot_autocorrelation(data, var, title_suffix, remove_first_lag)
      print(p)  # Print each plot to the PDF
    })
  }
  dev.off()  # Close the PDF device
}

# Set your output directory
output_dir <- "autocorrelation_plots"
dir.create(output_dir, showWarnings = FALSE)

# Plot and save all autocorrelation plots into a single PDF for original data
plot_all_autocorrelations_pdf(original_data, "Original Data", file.path(output_dir, "autocorrelation_original.pdf"), remove_first_lag = FALSE)

# Plot and save all autocorrelation plots into a single PDF for transformed data
plot_all_autocorrelations_pdf(transformed_data, "Transformed Data", file.path(output_dir, "autocorrelation_transformed.pdf"), remove_first_lag = TRUE)

################################################################################
# Correlation Matrix 
################################################################################

# Compute the correlation matrix 
correlation_matrix <- cor(transformed_data_cleaned_no_COVID[-1], use = "complete.obs")

# Create the heatmap directly from the correlation matrix
pheatmap(correlation_matrix,
         cluster_rows = FALSE,  # Disable clustering for rows
         cluster_cols = FALSE,  # Disable clustering for columns
         display_numbers = FALSE,  # Do not display correlation values
         color = viridis::plasma(100, direction = -1),  # Use plasma color palette
         main = "",  # Empty title (no bold styling)
         labels_row = rep("", nrow(correlation_matrix)),  # Empty strings for rows
         labels_col = rep("", ncol(correlation_matrix)),  # Empty strings for columns
         border_color = NA)  # Remove grey borders around cells

# Save the heatmap as a high-resolution PDF
pdf("correlation_matrix_heatmap.pdf", width = 10, height = 8)
heatmap_plot <- pheatmap(correlation_matrix,
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         display_numbers = FALSE,
                         color = viridis::plasma(100, direction = -1),
                         main = "",
                         labels_row = rep("", nrow(correlation_matrix)),
                         labels_col = rep("", ncol(correlation_matrix)),
                         border_color = NA)
print(heatmap_plot)
dev.off()

################################################################################
# Eigenvalues and Eigenvectors for Correlation Matrix
################################################################################

# Compute eigenvalues and eigenvectors for the correlation matrix
eigen_corr <- eigen(correlation_matrix)
eigenvalues_corr <- eigen_corr$values
eigenvectors_corr <- eigen_corr$vectors

# Print the eigenvalues and eigenvectors for reference
print("Eigenvalues of the Correlation Matrix:")
print(eigenvalues_corr)
print("Eigenvectors of the Correlation Matrix:")
print(eigenvectors_corr)

################################################################################
# Scree Plot of Eigenvalues
################################################################################

# Create the scree plot for eigenvalues
scree_plot_eigenvalues <- ggplot(data.frame(Eigenvalue = eigenvalues_corr, Component = seq_along(eigenvalues_corr)), 
                                 aes(x = Component, y = Eigenvalue)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree Plot of Eigenvalues (Correlation Matrix)", x = "Component", y = "Eigenvalue") +
  theme_minimal()

# Save the scree plot of eigenvalues as a high-resolution PDF
ggsave("scree_plot_eigenvalues_correlation_matrix.pdf", plot = scree_plot_eigenvalues, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Ratio Plot of Eigenvalues
################################################################################

# Number of eigenvalues
num_eigenvalues <- length(eigenvalues_corr)

# Compute eigenvalue ratios (λ_i / λ_{i+1})
eigen_corr_difference <- numeric(num_eigenvalues - 1)  # One less than the number of eigenvalues

for (i in 1:(num_eigenvalues - 1)) {
  eigen_corr_difference[i] <- eigenvalues_corr[i] / eigenvalues_corr[i + 1]
}

# Create the eigenvalue ratio plot
scree_plot_ratios <- ggplot(data.frame(Eigenvalues_ratios = eigen_corr_difference, Component = 1:(num_eigenvalues - 1)), 
                            aes(x = Component, y = Eigenvalues_ratios)) +
  geom_line() +
  geom_point() +
  labs(title = "Eigenvalue Ratios (Correlation Matrix)", x = "Component", y = "Eigenvalue Ratios") +
  theme_minimal()

# Save the eigenvalue ratio plot as a high-resolution PDF
ggsave("eigenvalue_ratios_correlation_matrix.pdf", plot = scree_plot_ratios, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Cumulative Explained Variance using Eigenvalues
################################################################################

# Compute explained variance as a percentage
explained_variance <- (eigenvalues_corr / sum(eigenvalues_corr)) * 100

# Compute cumulative explained variance
cumulative_variance <- cumsum(explained_variance)

# Print cumulative explained variance as percentages
print("Cumulative Explained Variance (%):")
print(cumulative_variance)
