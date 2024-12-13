# Inflation Forecasting Project

This updated university group project focuses on forecasting inflation using various time series techniques. The dataset is sourced from the Federal Reserve Bank of St. Louis and includes key economic indicators used for modeling inflation rates.

## Methodology

The analysis covers multiple forecasting models, with a focus on both traditional and modern machine learning techniques. The key methods used include:

**AR(1) Model**: A standard autoregressive model to capture inflation trends.

**Lasso Regression**: A regularization technique to improve model accuracy by selecting key predictors.

**Ridge Regression**: Similar to Lasso but handles multicollinearity differently by shrinking coefficients.

**Principal Component Regression (PCR)**: Reducing dimensionality in the dataset and using principal components for forecasting.

**Vector Autoregression (VAR)**: A multivariate time series model to capture relationships between inflation and macroeconomic indicators.

**Random Forest (RF)**: A machine learning technique that captures complex, non-linear patterns for forecasting.

## Files

**NOTE**: For best results, start with the `PreProcessing.R` file, as the other scripts assume the data has already been pre-processed.

### `PreProcessing.R`
- Prepares data by addressing autocorrelation issues, plotting time series graphs, removing missing values, and performing essential preprocessing steps.

### `AR1.R`
- Uses the AR(1) model as a benchmark for evaluating more complex models.

### `Lasso.R`
- Implements Lasso regression with regularization parameters tuned through experimentation and research insights.

### `Ridge.R`
- Applies Ridge regression to handle multicollinearity, with parameters guided by research insights.

### `PCR.R`
- Performs Principal Component Regression (PCR), optimizing performance by experimenting with different numbers of PCs.

### `VAR.R`
- Implements Vector Autoregression (VAR) models, using PCA to address multicollinearity issues.

### `RandomForest.R`
- Explores Random Forests to capture non-linear relationships in the data.

### `Report.pdf`
- Detailed report covering results, visualizations, and model comparisons.

### `current.csv`
- Dataset of US monthly macroeconomic indicators from the Federal Reserve used in the analysis.

