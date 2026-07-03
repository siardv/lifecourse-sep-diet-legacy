t_test <- function(var, data, inf_var) {
  data <- numeric_data_frame(data)
  x <- data[[var]]
  if (!is.numeric(x)) {
    stop(paste("Variable", var, "is not numeric"))
  }
  res <- stats::t.test(x ~ data[[inf_var]], var.equal = FALSE)
  out <- data.frame(
    var = var,
    t = res$statistic,
    df = res$parameter,
    p_value = res$p.value,
    mean_1 = mean(x[data[[inf_var]] == 0], na.rm = TRUE),
    mean_2 = mean(x[data[[inf_var]] == 1], na.rm = TRUE),
    ci_low = res$conf.int[1],
    ci_high = res$conf.int[2]
  )
  out$mean_diff <- out$mean_1 - out$mean_2
  rownames(out) <- NULL
  return(out)
}