chisq_test <- function(var, data, inf_var) {
  data <- numeric_data_frame(data)
  contingency_table <- table(data[[var]], data[[inf_var]])
  res <- stats::chisq.test(contingency_table)
  out <- data.frame(
    var = var,
    chi_square = res$statistic,
    df = res$parameter,
    p_value = res$p.value
  )
  rownames(out) <- NULL
  return(out)
}