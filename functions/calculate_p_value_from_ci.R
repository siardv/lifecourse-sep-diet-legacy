calculate_p_value_from_ci <- function(coef, ci_lower, ci_upper, conf_level = 0.95) {
  # Ensure ci_lower is actually lower than ci_upper
  ci_min <- pmin(ci_lower, ci_upper)
  ci_max <- pmax(ci_lower, ci_upper)

  # Check if 0 is in the confidence interval
  contains_zero <- (ci_min <= 0 & ci_max >= 0)

  # Calculate the critical value
  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)

  # Calculate the standard error
  standard_error <- (ci_max - ci_min) / (2 * z)

  # Calculate the test statistic
  test_statistic <- coef / standard_error

  # Calculate the p-value
  p_value <- 2 * pnorm(-abs(test_statistic))

  # Adjust p-values for consistency with CI
  # .Machine$double.eps is the smallest positive floating-point number x such that 1 + x != 1
  # adding this to the p-value ensures that the p-value is not exactly equal to alpha,
  # in simpler terms, it ensures that the p-value is not exactly equal to the significance level
  p_value[contains_zero & p_value <= alpha] <- alpha + .Machine$double.eps
  p_value[!contains_zero & p_value > alpha] <- alpha - .Machine$double.eps

  return(p_value)
}
# boot_p_value <- calculate_p_value_from_ci(boot_coefs, boot_ci_lower, boot_ci_upper)