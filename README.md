# Inflation Forecasting Project

This updated group project focuses on forecasting inflation using various time series techniques. The dataset is sourced from the Federal Reserve Bank of St. Louis and includes key economic indicators used for modeling inflation rates.

## Methodology

The analysis covers multiple forecasting models, with a focus on both traditional and modern machine learning techniques. The key methods used include:

AR(1) Model: A standard autoregressive model to capture inflation trends.

Lasso Regression: A regularization technique to improve model accuracy by selecting key predictors.

Ridge Regression: Similar to Lasso but handles multicollinearity differently by shrinking coefficients.

Principal Component Regression (PCR): Reducing dimensionality in the dataset and using principal components for forecasting.

Vector Autoregression (VAR): A multivariate time series model to capture relationships between inflation and macroeconomic indicators.

Random Forest (RF): A machine learning technique that captures complex, non-linear patterns for forecasting.

## Files

PreProcessing.R: Handles data preparation, including addressing autocorrelation issues, plotting time series graphs, removing missing values, and other essential preprocessing steps.

InflationForecast.R: Conducts the primary analysis using methods such as AR(1), Lasso, Ridge, and Principal Component Regression (PCR).

VAR.R: Implements Vector Autoregression (VAR) models, including an application of PCA to address multicollinearity issues effectively.

RF.R: Explores the use of Random Forests to capture non-linear relationships in the data.

InflationForecastReport.pdf: A detailed report explaining the results, visualizations, and model comparisons.

current.csv: Dataset used in the analysis, consisting in US monthly macroeconomic indicators from the Federal Reserve.
