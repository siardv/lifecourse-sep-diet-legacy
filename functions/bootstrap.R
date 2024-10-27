# Modify the bootstrap function to accept 'cluster' as an argument
bootstrap <- function(data, formula, num_bootstraps = 1000, lmrob_control = NULL, seed = NULL, cluster = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(lmrob_control)) {
    lmrob_control <- robustbase::lmrob.control(
      maxit = 5000, k.max = 4000, tol = 1e-8, refine.tol = 1e-8
    )
  }

  # Fit the robust linear model
  robust_fit <- robustbase::lmrob(formula = formula, data = data, control = lmrob_control)

  # Define the bootstrap function for robust model
  bootstrap_robust <- function(data, indices) {
    d <- data[indices, ]
    fit <- robustbase::lmrob(formula = formula, data = d, control = lmrob_control)
    return(coef(fit))
  }

  # Export necessary functions and data to the cluster
  parallel::clusterExport(cluster, varlist = c("bootstrap_robust", "formula", "lmrob_control"), envir = environment())

  # Perform bootstrap for robust model with parallel processing
  robust_bootstrap <- boot::boot(
    data = data,
    statistic = bootstrap_robust,
    R = num_bootstraps,
    parallel = "snow",
    cl = cluster
  )

  # Compute confidence intervals and p-values for robust model
  robust_conf_intervals <- sapply(1:length(coef(robust_fit)), function(i) {
    ci <- boot::boot.ci(robust_bootstrap, type = "perc", index = i)
    if (is.null(ci)) {
      warning(paste("Problematic CI for coefficient index", i))
    }
    ci$percent[4:5]
  })

  robust_p_values <- sapply(1:length(coef(robust_fit)), function(i) {
    calculate_p_value_from_ci(
      robust_bootstrap$t0[i],
      robust_conf_intervals[1, i],
      robust_conf_intervals[2, i]
    )
  })

  # Fit the ordinary least squares model
  ols_fit <- stats::lm(formula = formula, data = data)

  # Define the bootstrap function for OLS model
  bootstrap_ols <- function(data, indices) {
    d <- data[indices, ]
    fit <- stats::lm(formula = formula, data = d)
    return(coef(fit))
  }

  # Export necessary functions and data to the cluster for OLS
  parallel::clusterExport(cluster, varlist = c("bootstrap_ols", "formula"), envir = environment())

  # Perform bootstrap for OLS model with parallel processing
  ols_bootstrap <- boot::boot(
    data = data,
    statistic = bootstrap_ols,
    R = num_bootstraps,
    parallel = "snow",
    cl = cluster
  )

  # Compute confidence intervals and p-values for OLS model
  ols_conf_intervals <- sapply(1:length(coef(ols_fit)), function(i) {
    ci <- boot::boot.ci(ols_bootstrap, type = "perc", index = i)
    if (is.null(ci)) {
      warning(paste("Problematic CI for coefficient index", i))
    }
    ci$percent[4:5]
  })

  ols_p_values <- sapply(1:length(coef(ols_fit)), function(i) {
    calculate_p_value_from_ci(
      ols_bootstrap$t0[i],
      ols_conf_intervals[1, i],
      ols_conf_intervals[2, i]
    )
  })

  # Compile results into a list
  result <- list(
    robust = list(
      model = robust_fit,
      standard_errors = sqrt(diag(vcov(robust_fit))),
      bootstrap_coefs = robust_bootstrap,
      p_values = robust_p_values,
      conf_intervals = robust_conf_intervals
    ),
    ols = list(
      model = ols_fit,
      standard_errors = sqrt(diag(vcov(ols_fit))),
      bootstrap_coefs = ols_bootstrap,
      p_values = ols_p_values,
      conf_intervals = ols_conf_intervals
    )
  )

  class(result) <- "lmrob_bootstrap"
  return(result)
}