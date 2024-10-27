calculate_imputation_metrics <- function(original, global, variablewise, column, as_strings = FALSE) {
  # Extract and align non-missing values
  original_values <- original[[column]]
  non_missing_indices <- !is.na(original_values)

  original_values_non_missing <- as.numeric(original_values[non_missing_indices])
  imputed_values_global_non_missing <- as.numeric(global$ximp[[column]][non_missing_indices])
  imputed_values_variablewise_non_missing <- as.numeric(variablewise$ximp[[column]][non_missing_indices])

  # Calculate metrics
  calculate_metrics <- function(imputed_values) {
    c(
      correlation = cor(original_values_non_missing, imputed_values, use = "complete.obs"),
      bias = mean(imputed_values - original_values_non_missing, na.rm = TRUE),
      variance = var(imputed_values, na.rm = TRUE)
    )
  }

  global_metrics <- calculate_metrics(imputed_values_global_non_missing)
  variablewise_metrics <- calculate_metrics(imputed_values_variablewise_non_missing)

  # Prepare results
  result <- data.frame(
    variable = column,
    method = c("Global", "Variable-wise"),
    correlation = c(global_metrics["correlation"], variablewise_metrics["correlation"]),
    bias = c(global_metrics["bias"], variablewise_metrics["bias"]),
    variance = c(global_metrics["variance"], variablewise_metrics["variance"])
  )

  # Print formatted strings if requested
  if (as_strings) {
    formatted_output <- sprintf(
      paste(
        "Correlation (Global): %.3f",
        "Correlation (variable-wise): %.3f",
        "Bias (Global): %.3f",
        "Bias (variable-wise): %.3f",
        "Variance (Global): %.3f",
        "Variance (variable-wise): %.3f",
        sep = "\n"
      ),
      result$correlation[1], result$correlation[2],
      result$bias[1], result$bias[2],
      result$variance[1], result$variance[2]
    )
    cat(formatted_output, "\n")
  }

  result
}