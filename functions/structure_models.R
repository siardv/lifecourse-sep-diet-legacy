structure_models <- function() {
  unadjusted_fits <- load_most_recent_rds("unadjusted_fits", "data")
  adjusted_fits <- load_most_recent_rds("adjusted_fits", "data")

  model_1_adjusted_fit <- adjusted_fits$model_1
  model_2_adjusted_fit <- adjusted_fits$model_2
  model_3_adjusted_fit <- adjusted_fits$model_3
  model_4_adjusted_fit <- adjusted_fits$model_4
  model_5_adjusted_fit <- adjusted_fits$model_5
  model_6_adjusted_fit <- adjusted_fits$model_6
  model_7_adjusted_fit <- adjusted_fits$model_7
  model_8_adjusted_fit <- adjusted_fits$model_8
  model_9_adjusted_fit <- adjusted_fits$model_9

  model_1_unadjusted_fit <- unadjusted_fits$model_1
  model_2_unadjusted_fit <- unadjusted_fits$model_2
  model_3_unadjusted_fit <- unadjusted_fits$model_3
  model_4_unadjusted_fit <- unadjusted_fits$model_4
  model_5_unadjusted_fit <- unadjusted_fits$model_5
  model_6_unadjusted_fit <- unadjusted_fits$model_6
  model_7_unadjusted_fit <- unadjusted_fits$model_7
  model_8_unadjusted_fit <- unadjusted_fits$model_8
  model_9_unadjusted_fit <- unadjusted_fits$model_9

  adjusted <- list(
    robust = list(
      model_1 = list(
        fit = model_1_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_1_adjusted_fit$robust$standard_errors,
          coefs = model_1_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_1_adjusted_fit$robust$p_values),
          conf_int = model_1_adjusted_fit$robust$conf_intervals
        )
      ),
      model_2 = list(
        fit = model_2_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_2_adjusted_fit$robust$standard_errors,
          coefs = model_2_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_2_adjusted_fit$robust$p_values),
          conf_int = model_2_adjusted_fit$robust$conf_intervals
        )
      ),
      model_3 = list(
        fit = model_3_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_3_adjusted_fit$robust$standard_errors,
          coefs = model_3_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_3_adjusted_fit$robust$p_values),
          conf_int = model_3_adjusted_fit$robust$conf_intervals
        )
      ),
      model_4 = list(
        fit = model_4_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_4_adjusted_fit$robust$standard_errors,
          coefs = model_4_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_4_adjusted_fit$robust$p_values),
          conf_int = model_4_adjusted_fit$robust$conf_intervals
        )
      ),
      model_5 = list(
        fit = model_5_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_5_adjusted_fit$robust$standard_errors,
          coefs = model_5_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_5_adjusted_fit$robust$p_values),
          conf_int = model_5_adjusted_fit$robust$conf_intervals
        )
      ),
      model_6 = list(
        fit = model_6_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_6_adjusted_fit$robust$standard_errors,
          coefs = model_6_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_6_adjusted_fit$robust$p_values),
          conf_int = model_6_adjusted_fit$robust$conf_intervals
        )
      ),
      model_7 = list(
        fit = model_7_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_7_adjusted_fit$robust$standard_errors,
          coefs = model_7_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_7_adjusted_fit$robust$p_values),
          conf_int = model_7_adjusted_fit$robust$conf_intervals
        )
      ),
      model_8 = list(
        fit = model_8_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_8_adjusted_fit$robust$standard_errors,
          coefs = model_8_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_8_adjusted_fit$robust$p_values),
          conf_int = model_8_adjusted_fit$robust$conf_intervals
        )
      ),
      model_9 = list(
        fit = model_9_adjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_9_adjusted_fit$robust$standard_errors,
          coefs = model_9_adjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_9_adjusted_fit$robust$p_values),
          conf_int = model_9_adjusted_fit$robust$conf_intervals
        )
      )
    ),
    ols = list(
      model_1 = list(
        fit = model_1_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_1_adjusted_fit$ols$standard_errors,
          coefs = model_1_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_1_adjusted_fit$ols$p_values),
          conf_int = model_1_adjusted_fit$ols$conf_intervals
        )
      ),
      model_2 = list(
        fit = model_2_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_2_adjusted_fit$ols$standard_errors,
          coefs = model_2_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_2_adjusted_fit$ols$p_values),
          conf_int = model_2_adjusted_fit$ols$conf_intervals
        )
      ),
      model_3 = list(
        fit = model_3_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_3_adjusted_fit$ols$standard_errors,
          coefs = model_3_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_3_adjusted_fit$ols$p_values),
          conf_int = model_3_adjusted_fit$ols$conf_intervals
        )
      ),
      model_4 = list(
        fit = model_4_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_4_adjusted_fit$ols$standard_errors,
          coefs = model_4_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_4_adjusted_fit$ols$p_values),
          conf_int = model_4_adjusted_fit$ols$conf_intervals
        )
      ),
      model_5 = list(
        fit = model_5_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_5_adjusted_fit$ols$standard_errors,
          coefs = model_5_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_5_adjusted_fit$ols$p_values),
          conf_int = model_5_adjusted_fit$ols$conf_intervals
        )
      ),
      model_6 = list(
        fit = model_6_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_6_adjusted_fit$ols$standard_errors,
          coefs = model_6_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_6_adjusted_fit$ols$p_values),
          conf_int = model_6_adjusted_fit$ols$conf_intervals
        )
      ),
      model_7 = list(
        fit = model_7_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_7_adjusted_fit$ols$standard_errors,
          coefs = model_7_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_7_adjusted_fit$ols$p_values),
          conf_int = model_7_adjusted_fit$ols$conf_intervals
        )
      ),
      model_8 = list(
        fit = model_8_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_8_adjusted_fit$ols$standard_errors,
          coefs = model_8_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_8_adjusted_fit$ols$p_values),
          conf_int = model_8_adjusted_fit$ols$conf_intervals
        )
      ),
      model_9 = list(
        fit = model_9_adjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_9_adjusted_fit$ols$standard_errors,
          coefs = model_9_adjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_9_adjusted_fit$ols$p_values),
          conf_int = model_9_adjusted_fit$ols$conf_intervals
        )
      )
    )
  )

  unadjusted <- list(
    robust = list(
      model_1 = list(
        fit = model_1_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_1_unadjusted_fit$robust$standard_errors,
          coefs = model_1_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_1_unadjusted_fit$robust$p_values),
          conf_int = model_1_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_2 = list(
        fit = model_2_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_2_unadjusted_fit$robust$standard_errors,
          coefs = model_2_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_2_unadjusted_fit$robust$p_values),
          conf_int = model_2_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_3 = list(
        fit = model_3_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_3_unadjusted_fit$robust$standard_errors,
          coefs = model_3_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_3_unadjusted_fit$robust$p_values),
          conf_int = model_3_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_4 = list(
        fit = model_4_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_4_unadjusted_fit$robust$standard_errors,
          coefs = model_4_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_4_unadjusted_fit$robust$p_values),
          conf_int = model_4_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_5 = list(
        fit = model_5_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_5_unadjusted_fit$robust$standard_errors,
          coefs = model_5_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_5_unadjusted_fit$robust$p_values),
          conf_int = model_5_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_6 = list(
        fit = model_6_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_6_unadjusted_fit$robust$standard_errors,
          coefs = model_6_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_6_unadjusted_fit$robust$p_values),
          conf_int = model_6_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_7 = list(
        fit = model_7_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_7_unadjusted_fit$robust$standard_errors,
          coefs = model_7_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_7_unadjusted_fit$robust$p_values),
          conf_int = model_7_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_8 = list(
        fit = model_8_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_8_unadjusted_fit$robust$standard_errors,
          coefs = model_8_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_8_unadjusted_fit$robust$p_values),
          conf_int = model_8_unadjusted_fit$robust$conf_intervals
        )
      ),
      model_9 = list(
        fit = model_9_unadjusted_fit$robust$model,
        bootstrap = list(
          std_errors = model_9_unadjusted_fit$robust$standard_errors,
          coefs = model_9_unadjusted_fit$robust$bootstrap_coefs,
          p_value = unlist(model_9_unadjusted_fit$robust$p_values),
          conf_int = model_9_unadjusted_fit$robust$conf_intervals
        )
      )
    ),
    ols = list(
      model_1 = list(
        fit = model_1_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_1_unadjusted_fit$ols$standard_errors,
          coefs = model_1_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_1_unadjusted_fit$ols$p_values),
          conf_int = model_1_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_2 = list(
        fit = model_2_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_2_unadjusted_fit$ols$standard_errors,
          coefs = model_2_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_2_unadjusted_fit$ols$p_values),
          conf_int = model_2_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_3 = list(
        fit = model_3_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_3_unadjusted_fit$ols$standard_errors,
          coefs = model_3_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_3_unadjusted_fit$ols$p_values),
          conf_int = model_3_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_4 = list(
        fit = model_4_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_4_unadjusted_fit$ols$standard_errors,
          coefs = model_4_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_4_unadjusted_fit$ols$p_values),
          conf_int = model_4_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_5 = list(
        fit = model_5_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_5_unadjusted_fit$ols$standard_errors,
          coefs = model_5_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_5_unadjusted_fit$ols$p_values),
          conf_int = model_5_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_6 = list(
        fit = model_6_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_6_unadjusted_fit$ols$standard_errors,
          coefs = model_6_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_6_unadjusted_fit$ols$p_values),
          conf_int = model_6_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_7 = list(
        fit = model_7_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_7_unadjusted_fit$ols$standard_errors,
          coefs = model_7_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_7_unadjusted_fit$ols$p_values),
          conf_int = model_7_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_8 = list(
        fit = model_8_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_8_unadjusted_fit$ols$standard_errors,
          coefs = model_8_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_8_unadjusted_fit$ols$p_values),
          conf_int = model_8_unadjusted_fit$ols$conf_intervals
        )
      ),
      model_9 = list(
        fit = model_9_unadjusted_fit$ols$model,
        bootstrap = list(
          std_errors = model_9_unadjusted_fit$ols$standard_errors,
          coefs = model_9_unadjusted_fit$ols$bootstrap_coefs,
          p_value = unlist(model_9_unadjusted_fit$ols$p_values),
          conf_int = model_9_unadjusted_fit$ols$conf_intervals
        )
      )
    )
  )

  model_fits <- list(
    adjusted = adjusted,
    unadjusted = unadjusted
  )

  save_rds(model_fits, dir = "data")
  return(invisible(model_fits))
}