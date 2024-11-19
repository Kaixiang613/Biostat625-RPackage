test_that("linear_model_custom computes correct statistics on bike-day data", {
  # Locate the file dynamically
  file_path <- system.file("extdata", "bike-day.csv", package = "LinearRegression")
  data <- read.csv(file_path)

  # Prepare predictors and response
  X <- as.matrix(data[, c("temp")])  # Replace 'temp' with actual predictor column name
  y <- data$cnt  # Replace 'cnt' with actual response column name

  # Fit the linear model
  model <- linear_model_custom(X, y)

  # Test coefficients, R-squared, adjusted R-squared
  expect_equal(length(model$coefficients), ncol(X) + 1)  # Intercept + predictors
  expect_true(model$r_squared >= 0 && model$r_squared <= 1)  # R-squared range
  expect_true(model$adjusted_r_squared >= 0 && model$adjusted_r_squared <= 1)

  # Test t-values and p-values
  expect_equal(length(model$t_values), ncol(X) + 1)
  expect_equal(length(model$p_values), ncol(X) + 1)
})

test_that("plot_residuals works with bike-day data", {
  # Locate the file dynamically
  file_path <- system.file("extdata", "bike-day.csv", package = "LinearRegression")
  data <- read.csv(file_path)

  # Prepare predictors and response
  X <- as.matrix(data[, c("temp")])  # Replace 'temp' with actual predictor column name
  y <- data$cnt  # Replace 'cnt' with actual response column name

  # Fit the linear model
  model <- linear_model_custom(X, y)

  # Test residual plot
  expect_silent(plot_residuals(model$fitted_values, model$residuals))
})

test_that("plot_fitted_line works with bike-day data", {
  # Locate the file dynamically
  file_path <- system.file("extdata", "bike-day.csv", package = "LinearRegression")
  data <- read.csv(file_path)

  # Prepare predictors and response
  X <- as.matrix(data[, c("temp")])  # Replace 'temp' with actual predictor column name
  y <- data$cnt  # Replace 'cnt' with actual response column name

  # Fit the linear model
  model <- linear_model_custom(X, y)

  # Test fitted line plot
  expect_silent(plot_fitted_line(X, y, model$fitted_values))
})

