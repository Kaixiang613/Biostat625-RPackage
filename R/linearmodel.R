#' Linear Regression with Detailed Statistics
#'
#' Fit a linear regression model and calculate R-squared, Adjusted R-squared, t-test, and p-values.
#'
#' @param X A numeric matrix or data frame of predictors.
#' @param y A numeric vector of responses.
#' @return A list containing coefficients, R-squared, Adjusted R-squared, standard errors, t-values, and p-values.
#' @examples
#' X <- matrix(c(1, 2, 3, 4), ncol = 1)
#' y <- c(2.3, 4.1, 5.9, 8.2)
#' linear_model_custom(X, y)
#' @export
linear_model_custom <- function(X, y) {
  # Add intercept
  X <- cbind(Intercept = 1, as.matrix(X))

  # Number of observations and predictors
  n <- nrow(X)
  p <- ncol(X)

  # Calculate coefficients: beta = (X'X)^(-1) X'y
  beta <- solve(t(X) %*% X) %*% t(X) %*% y

  # Fitted values and residuals
  fitted_values <- X %*% beta
  residuals <- y - fitted_values

  # R-squared and Adjusted R-squared
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - (ss_residual / ss_total)
  adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p))

  # Standard errors, t-values, p-values
  mse <- ss_residual / (n - p)  # Mean Squared Error
  var_beta <- mse * solve(t(X) %*% X)  # Variance of coefficients
  se <- sqrt(diag(var_beta))  # Standard errors
  t_values <- beta / se  # t-values
  p_values <- 2 * pt(-abs(t_values), df = n - p)  # Two-tailed p-values

  # Return results
  return(list(
    coefficients = as.vector(beta),
    fitted_values = as.vector(fitted_values),
    residuals = as.vector(residuals),
    r_squared = r_squared,
    adjusted_r_squared = adjusted_r_squared,
    standard_errors = se,
    t_values = t_values,
    p_values = p_values
  ))
}



#' Plot Residuals
#'
#' Visualize residuals of a linear regression model against fitted values.
#'
#' @param fitted_values A numeric vector of fitted values.
#' @param residuals A numeric vector of residuals.
#' @examples
#' X <- matrix(c(1, 2, 3, 4), ncol = 1)
#' y <- c(2.3, 4.1, 5.9, 8.2)
#' model <- linear_model_custom(X, y)
#' plot_residuals(model$fitted_values, model$residuals)
#' @export
plot_residuals <- function(fitted_values, residuals) {
  plot(
    fitted_values, residuals,
    main = "Residuals vs Fitted",
    xlab = "Fitted Values",
    ylab = "Residuals",
    pch = 16,
    col = "blue"
  )
  abline(h = 0, col = "red", lty = 2)  # 添加参考线
}

#' Plot Fitted Line
#'
#' Visualize the fitted line of a linear regression model.
#'
#' @param X A numeric matrix or data frame of predictors (must have one column for this plot).
#' @param y A numeric vector of responses.
#' @param fitted_values A numeric vector of fitted values.
#' @examples
#' X <- matrix(c(1, 2, 3, 4), ncol = 1)
#' y <- c(2.3, 4.1, 5.9, 8.2)
#' model <- linear_model_custom(X, y)
#' plot_fitted_line(X, y, model$fitted_values)
#' @export
plot_fitted_line <- function(X, y, fitted_values) {
  if (ncol(X) > 1) {
    stop("This function only supports one predictor for visualization.")
  }

  plot(
    X[, 1], y,
    main = "Fitted Line Plot",
    xlab = "Predictor",
    ylab = "Response",
    pch = 16,
    col = "blue"
  )
  lines(X[, 1], fitted_values, col = "red", lwd = 2)
}


