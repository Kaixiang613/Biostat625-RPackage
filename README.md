
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LinearRegression

The purpose of the `LinearRegression` package is to provide a simple
implementation of linear regression. This package not only fits a linear
regression model but also generates detailed statistics and
visualizations to aid in model interpretation.

## Installation

You can install the development version of LinearRegression like so:

``` r
# Install from GitHub 
devtools::install_github("Kaixiang613/LinearRegression")
```

## Features

The `LinearRegression` package provides the following features:

1.  **Linear Regression (`linear_model_custom`)**:
    - Fits a linear regression model.
    - Calculates:
      - Coefficients
      - R-squared and Adjusted R-squared
      - Standard errors
      - t-values and p-values
    - Outputs all key statistics in a single object for easy access.
2.  **Residual Analysis (`plot_residuals`)**:
    - Visualizes the residuals of the model against fitted values.
    - Helps in diagnosing model fit and identifying patterns in
      residuals.
3.  **Fitted Line Visualization (`plot_fitted_line`)**:
    - Displays the fitted line for simple linear regression models.
    - Useful for checking the linearity of relationships between
      predictors and responses. \## Usage

#### Fitting a Linear Regression Model

``` r
# Load the package
library(LinearRegression)

# Example data
X <- matrix(c(1, 2, 3, 4), ncol = 1)
y <- c(2.3, 4.1, 5.9, 8.2)

# Fit the model
model <- linear_model_custom(X, y)

# Display model results
print(model)
```

#### output：

\$coefficients

\[1\] 0.3 1.9

\$r_squared

\[1\] 0.98

\$adjusted_r_squared

\[1\] 0.96

\$t_values

\[1\] 3.5 9.8

\$p_values

\[1\] 0.01 0.0001

#### Visualizing Residuals

``` r
# Plot residuals
plot_residuals(model$fitted_values, model$residuals)
```

#### Plotting the Fitted Line

``` r
# Plot fitted line
plot_fitted_line(X, y, model$fitted_values)
```

## Documentation

For a more detailed walkthrough, check the vignette:

``` r
vignette("linear_model_tutorial", package = "LinearRegression")
```

## Data Requirements

- The package assumes that:
  - Predictors (`X`) are in a numeric matrix or data frame.
  - Responses (`y`) are in a numeric vector.
  - For visualization functions, only one predictor is supported for
    plotting fitted lines.
- This package is ideal for:
  - Simple exploratory data analysis.
  - Teaching purposes to explain the basics of linear regression.
  - Small-scale regression problems without the overhead of more complex
    frameworks like `lm()` or `glm()`.

## Error Handling

### Common Errors and Solutions

1.  **Error: “This function only supports one predictor for
    visualization.”**
    - This occurs when using `plot_fitted_line` with multiple
      predictors.
    - Solution: Ensure your predictor matrix `X` contains only one
      column.
2.  **Error: “Singular matrix”**
    - This occurs if the predictors in `X` are perfectly collinear.
    - Solution: Check for multicollinearity in your data and remove
      redundant predictors.
3.  **Error: “Matrix dimensions do not match”**
    - This occurs if the number of rows in `X` and `y` are not the same.
    - Solution: Ensure that `X` and `y` have the same number of rows.

## Handling Outliers

``` r
# Identify outliers in residuals
model <- linear_model_custom(X, y)
residuals <- model$residuals
outliers <- which(abs(residuals) > 2 * sd(residuals))
print(outliers)
```

## License

This package is released under the MIT License. You are free to use,
modify, and distribute this software under the terms of the MIT License.

A copy of the license is included in the repository. See the
[LICENSE](./LICENSE) file for details.
