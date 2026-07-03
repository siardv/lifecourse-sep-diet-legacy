structure_model_fits <- function(model_fits) {
  create_model_list <- function(models, type) {
    lapply(models, function(model, index) {
      list(
        fit = model[[type]]$model,
        bootstrap = list(
          std_errors = model[[type]]$standard_errors,
          coefs = model[[type]]$bootstrap_coefs,
          p_value = unlist(model[[type]]$p_values),
          conf_int = model[[type]]$conf_intervals
        )
      )
    }, seq_along(models))
  }

  create_adjusted_or_unadjusted <- function(models) {
    list(
      robust = setNames(create_model_list(models, "robust"), paste0("model_", 1:9)),
      ols = setNames(create_model_list(models, "ols"), paste0("model_", 1:9))
    )
  }

  list(
    adjusted = create_adjusted_or_unadjusted(model_fits[1:9]),
    unadjusted = create_adjusted_or_unadjusted(model_fits[10:18])
  )
}