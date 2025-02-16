% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearmodel.R
\name{plot_residuals}
\alias{plot_residuals}
\title{Plot Residuals}
\usage{
plot_residuals(fitted_values, residuals)
}
\arguments{
\item{fitted_values}{A numeric vector of fitted values.}

\item{residuals}{A numeric vector of residuals.}
}
\description{
Visualize residuals of a linear regression model against fitted values.
}
\examples{
X <- matrix(c(1, 2, 3, 4), ncol = 1)
y <- c(2.3, 4.1, 5.9, 8.2)
model <- linear_model_custom(X, y)
plot_residuals(model$fitted_values, model$residuals)
}
