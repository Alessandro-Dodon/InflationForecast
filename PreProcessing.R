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

# Install fbi package from github
#devtools::install_github("cykbennie/fbi")

# Load packages quietly
# Suppress warnings and messages
suppressWarnings(suppressMessages({
  
  library(pls)
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
  library(fbi)
  library(utils)
  library(imputeTS)
  library(randomForest)
  library(vars)
  
}))

################################################################################
# Start by loading the monthly data using the fredmd function (this function correctly treats the autocorrelation of each time series properly)
################################################################################

# Define the fredmd function
fredmd <- function(file = "", date_start = NULL, date_end = NULL, transform = TRUE) {
  # Debug: print the file path
  print(paste("File:", file))
  
  # Error checking
  if (!is.logical(transform))
    stop("'transform' must be logical.")
  if ((class(date_start) != "Date") && (!is.null(date_start)))
    stop("'date_start' must be Date or NULL.")
  if ((class(date_end) != "Date") && (!is.null(date_end)))
    stop("'date_end' must be Date or NULL.")
  
  if (class(date_start) == "Date") {
    if (as.numeric(format(date_start, "%d")) != 1)
      stop("'date_start' must be Date whose day is 1.")
    if (date_start < as.Date("1959-01-01"))
      stop("'date_start' must be later than 1959-01-01.")
  }
  
  if (class(date_end) == "Date") {
    if (as.numeric(format(date_end, "%d")) != 1)
      stop("'date_end' must be Date whose day is 1.")
  }
  
  print("Reading raw data...")
  # Prepare raw data
  rawdata <- readr::read_csv(file, col_names = FALSE, col_types = cols(X1 = col_date(format = "%m/%d/%Y")), skip = 2)
  print(head(rawdata))
  
  rawdata <- as.data.frame(rawdata)
  row_to_remove = c()
  for (row in (nrow(rawdata) - 20):nrow(rawdata)) {
    if (!any(is.finite(unlist(rawdata[row, ])))) {
      row_to_remove = c(row_to_remove, row)  # remove NA rows
    }
  }
  if (length(row_to_remove) > 0) {
    rawdata = rawdata[-row_to_remove, ]
  }
  print("Raw data after removing NA rows:")
  print(head(rawdata))
  
  print("Reading attribute data...")
  attrdata <- utils::read.csv(file, header = FALSE, nrows = 2)
  header <- c("date", unlist(attrdata[1, 2:ncol(attrdata)]))
  colnames(rawdata) <- header
  print("Header:")
  print(header)
  
  # Store transformation codes as tcode
  tcode <- unlist(attrdata[2, 2:ncol(attrdata)])
  print("Transformation codes:")
  print(tcode)
  
  # Subfunction transxf: data transformation based on tcodes
  transxf <- function(x, tcode) {
    # Number of observations (including missing values)
    n <- length(x)
    
    # Value close to zero
    small <- 1e-06
    
    # Allocate output variable
    y <- rep(NA, n)
    y1 <- rep(NA, n)
    
    # TRANSFORMATION: Determine case 1-7 by transformation code
    if (tcode == 1) {
      # Case 1 Level (i.e. no transformation): x(t)
      y <- x
      
    } else if (tcode == 2) {
      # Case 2 First difference: x(t)-x(t-1)
      y[2:n] <- x[2:n] - x[1:(n - 1)]
      
    } else if (tcode == 3) {
      # case 3 Second difference: (x(t)-x(t-1))-(x(t-1)-x(t-2))
      y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
      
    } else if (tcode == 4) {
      # case 4 Natural log: ln(x)
      if (min(x, na.rm = TRUE) > small)
        y <- log(x)
      
    } else if (tcode == 5) {
      # case 5 First difference of natural log: ln(x)-ln(x-1)
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[2:n] <- x[2:n] - x[1:(n - 1)]
      }
      
    } else if (tcode == 6) {
      # case 6 Second difference of natural log:
      # (ln(x)-ln(x-1))-(ln(x-1)-ln(x-2))
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
      }
      
    } else if (tcode == 7) {
      # case 7 First difference of percent change:
      # (x(t)/x(t-1)-1)-(x(t-1)/x(t-2)-1)
      y1[2:n] <- (x[2:n] - x[1:(n - 1)]) / x[1:(n - 1)]
      y[3:n] <- y1[3:n] - y1[2:(n - 1)]
    }
    
    return(y)
  }
  
  # Transform data
  if (transform) {
    # Apply transformations
    N <- ncol(rawdata)
    data <- rawdata
    data[, 2:N] <- NA
    
    # Perform transformation using subfunction transxf (see below for details)
    for (i in 2:N) {
      temp <- transxf(rawdata[, i], tcode[i - 1])
      data[, i] <- temp
    }
    
  } else {
    data <- rawdata
  }
  
  print("Data after transformation:")
  print(head(data))
  
  # Null case of date_start and date_end
  if (is.null(date_start))
    date_start <- as.Date("1959-01-01")
  if (is.null(date_end))
    date_end <- data[, 1][nrow(data)]
  
  # Subset data
  index_start <- which.max(data[, 1] == date_start)
  index_end <- which.max(data[, 1] == date_end)
  
  outdata <- data[index_start:index_end, ]
  class(outdata) <- c("data.frame", "fredmd")
  return(outdata)
}

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

data(fredmd_description) # Click on fredmd_description <Promise> on the Environment 
print(fredmd_description)

################################################################################
# View variable of interest "CPIULFSL"
################################################################################

# Extract the full column for CPIULFSL
cpi_column <- dataset$CPIULFSL

# View the first few values to verify
head(cpi_column)

################################################################################
# Plotting before and after each time series
################################################################################

# Function to create a plot for a single variable
plot_variable <- function(data, var_name, title_suffix) {
  # Use backticks to handle special characters in column names
  ggplot(data, aes_string(x = "date", y = paste0("`", var_name, "`"))) +
    geom_line() +
    labs(title = paste("Plot of", var_name, title_suffix), x = "Date", y = var_name) +
    theme_minimal()
}

# Function to plot and save all variables except date
plot_all_variables <- function(data, title_suffix, output_dir) {
  vars <- names(data)[-1]  # Exclude the first column (date)
  
  for (var in vars) {
    p <- plot_variable(data, var, title_suffix)
    print(p)  # Print the plot to the console
    
    # Save the plot as a file
    ggsave(filename = paste0(output_dir, "/", gsub(" ", "_", var), "_", title_suffix, ".png"), plot = p)
  }
}

# Set your output directory
output_dir <- "plots"
dir.create(output_dir, showWarnings = FALSE)

# Plot and save for original data
plot_all_variables(original_data, "Original Data", output_dir)

# Plot and save for transformed data
plot_all_variables(transformed_data, "Transformed Data", output_dir)

################################################################################
# Plotting before and after the autocorrelation of each time series
################################################################################

# Function to create an autocorrelation plot for a single variable
plot_autocorrelation <- function(data, var_name, title_suffix) {
  # Extract the variable data and remove NA values
  var_data <- na.omit(data[[var_name]])
  
  # Create an autocorrelation plot
  autocorr <- acf(var_data, plot = FALSE)
  
  # Convert to a data frame for ggplot2
  acf_df <- data.frame(Lag = autocorr$lag, Autocorrelation = autocorr$acf)
  
  # Calculate the confidence interval
  ci <- qnorm((1 + 0.95) / 2) / sqrt(length(var_data))
  
  # Plot using ggplot2
  ggplot(acf_df, aes(x = Lag, y = Autocorrelation)) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "red") +
    geom_segment(aes(xend = Lag, yend = 0), color = "black") +
    geom_point(color = "black") +
    labs(title = paste("Autocorrelation of", var_name, title_suffix), x = "Lag", y = "Autocorrelation") +
    theme_minimal()
}

# Function to plot and save all autocorrelation plots except date
plot_all_autocorrelations <- function(data, title_suffix, output_dir) {
  vars <- names(data)[-1]  # Exclude the first column (date)
  
  for (var in vars) {
    p <- plot_autocorrelation(data, var, title_suffix)
    print(p)  # Print the plot to the console
    
    # Save the plot as a file
    ggsave(filename = paste0(output_dir, "/", gsub(" ", "_", var), "_Autocorrelation_", title_suffix, ".png"), plot = p)
  }
}

# Set your output directory
output_dir <- "autocorrelation_plots"
dir.create(output_dir, showWarnings = FALSE)

# Plot and save autocorrelation plots for original data
plot_all_autocorrelations(original_data, "Original Data", output_dir)

# Plot and save autocorrelation plots for transformed data
plot_all_autocorrelations(transformed_data, "Transformed Data", output_dir)

################################################################################
# Function to create a plot for a single variable
################################################################################

plot_variable <- function(data, var_name, title_suffix) {
  # Use backticks to handle special characters in column names
  ggplot(data, aes_string(x = "date", y = paste0("`", var_name, "`"))) +
    geom_line() +
    labs(title = paste("Plot of", var_name, title_suffix), x = "Date", y = var_name) +
    theme_minimal()
}

# Function to create an autocorrelation plot for a single variable
plot_autocorrelation <- function(data, var_name, title_suffix) {
  # Extract the variable data and remove NA values
  var_data <- na.omit(data[[var_name]])
  
  # Create an autocorrelation plot
  autocorr <- acf(var_data, plot = FALSE)
  
  # Convert to a data frame for ggplot2
  acf_df <- data.frame(Lag = autocorr$lag, Autocorrelation = autocorr$acf)
  
  # Calculate the confidence interval
  ci <- qnorm((1 + 0.95) / 2) / sqrt(length(var_data))
  
  # Plot using ggplot2
  ggplot(acf_df, aes(x = Lag, y = Autocorrelation)) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "red") +
    geom_segment(aes(xend = Lag, yend = 0), color = "black") +
    geom_point(color = "black") +
    labs(title = paste("Autocorrelation of", var_name, title_suffix), x = "Lag", y = "Autocorrelation") +
    theme_minimal()
}

# Create and save the plots for CPIULFSL
output_pdf <- "CPIULFSL_plots.pdf"
pdf(output_pdf, width = 10, height = 8)

# Plot and save the original time series plot
p1 <- plot_variable(original_data, "CPIULFSL", "Original Data")
print(p1)

# Plot and save the transformed time series plot
p2 <- plot_variable(transformed_data, "CPIULFSL", "Transformed Data")
print(p2)

# Plot and save the autocorrelation plot of the transformed data
p3 <- plot_autocorrelation(transformed_data, "CPIULFSL", "Transformed Data")
print(p3)

# Close the PDF device
dev.off()

################################################################################
# Original data inspection (check for missing values)
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
# Cleaning dataset (we start from 1960-01-01, end in 2024-01-01 and take out variables that have too many nas in a row)
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

################################################################################
# Missing values for data transformed cleaned
# Count and locate NAs in transformed data cleaned
na_details_transformed_data_cleaned <- count_and_locate_nas(transformed_data_cleaned)

print("Details of NAs in transformed data cleaned:")
print(na_details_transformed_data_cleaned)

################################################################################
# Cleaning dataset from the last nas (we use SMA for the remaining nas)
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

# NOTE: The SMA method calculates the average of the previous 'n' values in a series and uses this average to replace NA values. This helps in smoothing the data by filling in missing values based on the surrounding data points, thereby maintaining the overall trend and continuity of the data.

################################################################################
# Re-check missing values for data transformed cleaned
# Count and locate NAs in transformed data cleaned
na_details_transformed_data_cleaned <- count_and_locate_nas(transformed_data_cleaned)

print("Details of NAs in transformed data cleaned:")
print(na_details_transformed_data_cleaned)

################################################################################
# Covariance and Correlation (standardizing the data)
################################################################################

# Standardizing the data (mean = 0, sd = 1)
standardized_transformed_data_cleaned <- scale(transformed_data_cleaned[-1])  # First column is date

# Compute the correlation and covariance matrices using complete observations
correlation_matrix <- cor(standardized_transformed_data_cleaned, use = "complete.obs")
covariance_matrix <- cov(standardized_transformed_data_cleaned, use = "complete.obs")

# Store the results in new data frames
covariance_matrix <- as.data.frame(covariance_matrix)
correlation_matrix <- as.data.frame(correlation_matrix)

# Heatmaps 
# Ensure the matrices have appropriate row and column names
rownames(covariance_matrix) <- colnames(covariance_matrix) 
rownames(correlation_matrix) <- colnames(correlation_matrix) 

# Convert matrices to long format for ggplot2
correlation_long <- reshape2::melt(as.matrix(correlation_matrix), varnames = c("Variable1", "Variable2"), value.name = "Correlation")
covariance_long <- reshape2::melt(as.matrix(covariance_matrix), varnames = c("Variable1", "Variable2"), value.name = "Covariance")

# Heatmap for the Correlation Matrix
ggplot(correlation_long, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "plasma", direction = -1, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  labs(title = "Correlation Matrix Heatmap (standardized)")

# Create the heatmap plot for the Covariance Matrix
cov_heatmap <- ggplot(covariance_long, aes(Variable1, Variable2, fill = Covariance)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "plasma", direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  labs(title = "Covariance Matrix Heatmap of FRED_MD Dataset (Standardized)")

# Save the plot as a high-resolution PDF
ggsave("covariance_matrix_heatmap.pdf", plot = cov_heatmap, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Eigenvalues and eigenvectors for PCA
################################################################################

# Compute eigenvalues and eigenvectors for the correlation matrix
eigen_corr <- eigen(correlation_matrix)
eigenvalues_corr <- eigen_corr$values
eigenvectors_corr <- eigen_corr$vectors

# Compute eigenvalues and eigenvectors for the covariance matrix
eigen_cov <- eigen(covariance_matrix)
eigenvalues_cov <- eigen_cov$values
eigenvectors_cov <- eigen_cov$vectors

# Optionally, print the results to see them
print("Eigenvalues of the Correlation Matrix:")
print(eigenvalues_corr)
print("Eigenvectors of the Correlation Matrix:")
print(eigenvectors_corr)

print("Eigenvalues of the Covariance Matrix:")
print(eigenvalues_cov)
print("Eigenvectors of the Covariance Matrix:")
print(eigenvectors_cov)

# Visualize the eigenvalues (scree plot) to determine how many components are useful
# Plot for the correlation matrix eigenvalues
ggplot(data.frame(Eigenvalue = eigenvalues_corr, Component = seq_along(eigenvalues_corr)), aes(x = Component, y = Eigenvalue)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree Plot of Eigenvalues (Correlation Matrix standardized)", x = "Component", y = "Eigenvalue") +
  theme_minimal()

# Plot for the covariance matrix eigenvalues
ggplot(data.frame(Eigenvalue = eigenvalues_cov, Component = seq_along(eigenvalues_cov)), aes(x = Component, y = Eigenvalue)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree Plot of Eigenvalues (Covariance Matrix standardized)", x = "Component", y = "Eigenvalue") +
  theme_minimal()

################################################################################
# Ratio plots 
################################################################################

# Number of eigenvalues
num_eigenvalues <- length(eigenvalues_corr)

# Create a loop for eigenvalues correlation difference
eigen_corr_difference <- numeric(num_eigenvalues - 1)  # One less than the number of eigenvalues

for (i in 1:(num_eigenvalues - 1)) {
  eigen_corr_difference[i] = eigenvalues_corr[i] / eigenvalues_corr[i + 1]
}

# Visualize the plot for correlation matrix eigenvalue ratios
ggplot(data.frame(Eigenvalues_ratios = eigen_corr_difference, Component = 1:(num_eigenvalues - 1)), 
       aes(x = Component, y = Eigenvalues_ratios)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree Plot of Eigenvalues' Ratios (Correlation Matrix)", x = "Component", y = "Eigenvalues Ratios") +
  theme_minimal()

# Create a loop for eigenvalues covariance difference
eigen_cov_difference <- numeric(num_eigenvalues - 1)

for (i in 1:(num_eigenvalues - 1)) {
  eigen_cov_difference[i] = eigenvalues_cov[i] / eigenvalues_cov[i + 1]
}

# Visualize the plot for covariance matrix eigenvalue ratios
scree_plot <- ggplot(data.frame(Eigenvalues_ratios = eigen_cov_difference, Component = 1:(num_eigenvalues - 1)), 
                     aes(x = Component, y = Eigenvalues_ratios)) +
  geom_line() +
  geom_point() +
  labs(title = "Eigenvalues' Ratios (Covariance Matrix)", x = "Component", y = "Eigenvalues Ratios") +
  theme_minimal()

# Save the plot as a high-resolution PDF
ggsave("plot_eigenvalues_ratios.pdf", plot = scree_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Take COVID-19 years out
################################################################################

# Remove the years
transformed_data_cleaned_no_COVID <- transformed_data_cleaned[as.Date(transformed_data_cleaned$date) <= as.Date("2019-12-01"), ]

# Display the first few rows of the cleaned data
head(transformed_data_cleaned_no_COVID)

################################################################################
# Holdout method for splitting data 
################################################################################

# Define the split date (we use 70%)
split_date <- as.Date("2000-01-01")

# Split the data
train_data_holdout <- transformed_data_cleaned_no_COVID[transformed_data_cleaned_no_COVID$date < split_date, ]
test_data_holdout <- transformed_data_cleaned_no_COVID[transformed_data_cleaned_no_COVID$date >= split_date, ]

# Verify the split
cat("Training Data Range:\n")
print(range(train_data_holdout$date))

cat("\nTest Data Range:\n")
print(range(test_data_holdout$date))

# Check the dimensions of the split data
cat("\nTraining Data Dimensions:\n")
print(dim(train_data_holdout))

cat("\nTest Data Dimensions:\n")
print(dim(test_data_holdout))

