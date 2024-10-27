model_info <- function(model, term_names, model_type = "bootstrap") {
  if (model_type %in% c("bootstrap", "fit")) {
    model_type <- model_type
  } else {
    stop("model_type must be either 'bootstrap' or 'fit'")
  }

  if (model_type == "bootstrap") {
    # retrieve the bootstrap model
    boot_model <- model[[model_type]]
    boot_coefs <- as.numeric(boot_model$coefs$t0)
    bootstrap_replicates <- boot_model$coefs$t
    boot_std_errors <- boot_model$std_errors

    boot_ci_lower <- boot_model$conf_int[1, ]
    boot_ci_upper <- boot_model$conf_int[2, ]

    boot_p_value <- calculate_p_value_from_ci(boot_coefs, boot_ci_lower, boot_ci_upper)

    # Calculate bootstrap bias
    bootstrap_means <- colMeans(bootstrap_replicates)
    boot_bias <- bootstrap_means - boot_coefs

    # Calculate relative bias
    relative_bias <- ifelse(boot_coefs != 0, (boot_bias / abs(boot_coefs)) * 100, NA)

    # retrieving the model fit
    model_fit <- model[["fit"]]
    coefs <- model_fit$coefficients
    conf_int <- confint(model_fit)
    coefs <- cbind(coefs, conf_int)

    # Calculate standardized beta coefficients for bootstrap
    x <- model.matrix(model_fit)
    y <- model_fit$model[] # Response variable
    sd_x <- apply(x, 2, sd)
    sd_y <- sd(y)
    beta_std <- boot_coefs * (sd_x / sd_y)

    # reassigning the coefs to the full_coefs matrix
    full_coefs <- matrix(NA, nrow = length(term_names), ncol = 15)
    rownames(full_coefs) <- term_names
    full_coefs[rownames(coefs), 1:6] <- coefs
    full_coefs[rownames(coefs), 7] <- boot_coefs
    full_coefs[rownames(coefs), 8] <- boot_std_errors
    full_coefs[rownames(coefs), 9] <- boot_bias
    full_coefs[rownames(coefs), 10] <- relative_bias
    full_coefs[rownames(coefs), 11] <- boot_p_value
    full_coefs[rownames(coefs), 12] <- boot_ci_lower
    full_coefs[rownames(coefs), 13] <- boot_ci_upper
    # Corrected estimate (bias-corrected estimate from bootstrap)
    full_coefs[rownames(coefs), 14] <- boot_coefs - boot_bias
    full_coefs[rownames(coefs), 15] <- beta_std

    column_names <- c(
      "coef", "se", "t", "p", "ci_lower", "ci_upper",
      "boot_coef", "boot_se", "boot_bias", "relative_bias", "boot_p",
      "boot_ci_lower", "boot_ci_upper", "corrected_estimate", "beta_std"
    )

    # matrix becomes data frame with new column names
    full_coefs <- setNames(as.data.frame(full_coefs), column_names) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(., 3)))

    # Calculate mean absolute bias
    mean_absolute_bias <- mean(abs(full_coefs$boot_bias), na.rm = TRUE)

    # Identify coefficients with large relative bias
    large_bias <- full_coefs[abs(full_coefs$relative_bias) > 10, ]

    # Identify statistically significant coefficients
    significant_coefs <- full_coefs[full_coefs$boot_p < 0.05, ]

    # Create a list to store all results
    results <- list(
      full_coefs = full_coefs,
      mean_absolute_bias = mean_absolute_bias,
      large_bias = large_bias,
      significant_coefs = significant_coefs
    )

    # Original formatting for boot_coefs
    boot_coefs_formatted <- dplyr::select(full_coefs, boot_coef, boot_se, boot_p)
    results$boot_coefs_formatted <- apply(boot_coefs_formatted, 1, function(x) {
      format_coef(x["boot_coef"], x["boot_se"], x["boot_p"])
    })
  } else if (model_type == "fit") {
    # retrieving the model fit
    model_fit <- model[["fit"]]

    # Extract coefficients, standard errors, and p-values
    coefs <- coef(model_fit)
    se <- sqrt(diag(model_fit$cov))
    t_values <- coefs / se
    p_values <- 2 * (1 - pt(abs(t_values), df = model_fit$df.residual))
    conf_int <- confint(model_fit)

    # Calculate standardized beta coefficients
    x <- model.matrix(model_fit)
    y <- model_fit$model[[1]] # Response variable
    sd_x <- apply(x, 2, sd)
    sd_y <- sd(y)
    beta_std <- coefs * (sd_x / sd_y)

    # Combine results into a data frame
    full_coefs <- data.frame(
      coef = coefs,
      se = se,
      t = t_values,
      p = p_values,
      ci_lower = conf_int[, 1],
      ci_upper = conf_int[, 2],
      beta_std = beta_std
    )

    # Round numeric values to 3 decimal places
    full_coefs <- full_coefs %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(., 3)))

    # Identify statistically significant coefficients
    significant_coefs <- full_coefs[full_coefs$p < 0.05, ]

    # Format coefficients
    coefs_formatted <- apply(full_coefs, 1, function(x) {
      format_coef(x["coef"], x["se"], x["p"])
    })

    full_coefs$p <- format_p_value(full_coefs$p, add_stars = TRUE)

    results <- list(
      full_coefs = full_coefs,
      significant_coefs = significant_coefs,
      coefs_formatted = coefs_formatted
    )
  }
  # Add footnote for both bootstrap and fit
  full_coefs_footnote <- paste(
    "Standardized betas indicate the standard deviation (SD) change in the",
    "dependent variable per 1 SD increase in the independent variable.",
    "Due to multicollinearity or strong combined effects, values can range beyond $-1$ to $+1$,",
    "especially for interactions. Interpret cautiously, considering the",
    "coefficients for related variables together rather than in isolation."
  )
  results$full_coefs_footnote <- full_coefs_footnote
  return(results)
}