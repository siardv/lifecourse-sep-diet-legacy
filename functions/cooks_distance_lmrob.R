cooks_distance_lmrob <- function(fit_lmrob, data) {
  require(robustbase, quietly = TRUE)

  # calculate the robust residuals and the fitted values
  rob_residuals <- residuals(fit_lmrob)
  fitted_values <- fitted(fit_lmrob)

  # calculate the leverage (hat) values for each observation
  model_matrix <- model.matrix(fit_lmrob)
  hat_values <- diag(model_matrix %*% solve(t(model_matrix) %*% model_matrix) %*% t(model_matrix))

  # number of parameters (p) and number of observations (n)
  n <- nrow(data)
  p <- ncol(model_matrix)

  # calculate the estimate of scale of the residuals
  scale_estimate <- summary(fit_lmrob)$sigma

  # cook's distance calculation
  cooks_dist <- (rob_residuals^2 / (p * scale_estimate^2)) * (hat_values / (1 - hat_values)^2)
  return(cooks_dist)
}

# Reference:
# Maronna, R. A., R  Douglas Martin, & YohaiV. J. (2006). Robust statistics : theory and methods. J. Wiley.
# ISBN: 978-0-470-01092-1