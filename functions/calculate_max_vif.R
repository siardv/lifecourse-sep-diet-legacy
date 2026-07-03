# Define the function to calculate the maximum VIF
calculate_max_vif <- function(model_fit, ...) {
  # Check if the model has more than 2 columns (including the intercept)
  if (ncol(model_fit$model) > 2) {
    # Perform the MTest
    mt_result <- MTest::MTest(model_fit, ...)
    # Return the maximum VIF value
    return(max(mt_result$vif.tot, na.rm = TRUE))
  }
  # Return 0 if the model does not have more than 2 columns
  return(0)
}