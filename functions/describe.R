describe <- function(df,
                     exclude = NULL,
                     kable_format = "html",
                     caption = "Descriptive statistics",
                     normality = TRUE,
                     print_only = TRUE,
                     add_footnote = TRUE,
                     latex = TRUE,
                     decimals = 3,
                     exact_p_values = FALSE,
                     verbose = TRUE,
                     copy_to_clipboard = FALSE) {
  # Transform logicals to numeric
  df <- purrr::map_df(df, ~ {
    if (is.logical(.x)) {
      as.numeric(.x)
    } else {
      .x
    }
  })

  # Exclude columns if specified
  if (!is.null(exclude)) {
    df <- dplyr::select(df, -any_of(exclude))
  }

  # Perform normality tests
  normality_cols <- sapply(df, function(.) {
    all(is.numeric(.)) & length(unique(.)) > 4
  })
  normality_test <- purrr::map2_dfr(normality_cols, names(normality_cols), ~ {
    if (.x) {
      if (nrow(df) > 5000) {
        test_result <- DescTools::LillieTest(df[[.y]])
      } else {
        test_result <- stats::shapiro.test(df[[.y]])
      }
      tibble::tibble(
        variable = .y,
        normality = test_result$statistic,
        p_value = test_result$p.value
      )
    } else {
      tibble::tibble(
        variable = .y,
        normality = NA,
        p_value = NA
      )
    }
  }, .id = "variable")

  # Calculate descriptive statistics
  out <- psych::describe(df, ranges = TRUE) %>%
    as.data.frame() %>%
    dplyr::select(-vars) %>%
    dplyr::bind_cols(normality_test) %>%
    dplyr::select(
      variable,
      n,
      mean,
      min,
      max,
      sd,
      skew,
      kurtosis,
      se,
      normality,
      p_value
    ) %>%
    `rownames<-`(NULL)

  # Format p-values if not exact
  if (!exact_p_values) {
    if (exists("format_p_value")) {
      out$p_value <- format_p_value(
        out$p_value,
        add_stars = TRUE,
        use_concat = TRUE,
        omit_p_label = TRUE
      )
    } else {
      warning(
        "The 'format_p_value' function is not available. The p-values will not be formatted."
      )
    }
  }

  # Remove normality columns if not required
  if (!normality) {
    x <- grep("Normality", names(out))
    out <- out[, -c(x, x + 1)]
  }

  # Set kable options
  options(knitr.kable.NA = "—")

  # Round output values if print_only
  if (print_only) {
    out[, -1] <- lapply(out[, -1], function(x) {
      num_x <- suppressWarnings(as.numeric(x))
      is_num <- !is.na(num_x)
      if (any(is_num)) {
        x[is_num] <- format(round(num_x[is_num], decimals), drop0trailing = TRUE)
      }
      return(x)
    })
  }

  # Define output table names
  out_table_names <- if (latex) {
    c(
      "$\\ $",
      "$n$",
      "$\\bar{x}$",
      "$\\text{min}$",
      "$\\text{max}$",
      "$sd$",
      "$\\text{skew}$",
      "$\\text{kurt}$",
      "$SE$",
      "$\\text{Normality}^{\\dagger}$",
      "$p$"
    )
  } else {
    c(
      "var",
      "n",
      "mean",
      "min",
      "max",
      "sd",
      "skew",
      "kurtosis",
      "se",
      "normality",
      "p"
    )
  }

  # Remove normality columns if not required
  if (!normality) {
    out_table_names <- head(out_table_names, -2)
  }

  # Create output table
  out_table <- knitr::kable(
    out,
    format = kable_format,
    digits = decimals,
    row.names = FALSE,
    escape = FALSE,
    col.names = out_table_names,
    caption = caption,
    align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c")
  )

  # Add footnote if required
  normality_info <- paste(
    ifelse(
      nrow(df) <= 5000,
      "Shapiro-Wilk Test",
      "Lilliefors (Kolmogorov-Smirnov) Test"
    )
  )
  if (normality && add_footnote && kable_format == "html") {
    out_table <- kableExtra::footnote(
      out_table,
      general = paste0(normality_info, "$^{\\dagger}$"),
      footnote_as_chunk = TRUE,
      escape = FALSE
    )
  }

  # Display verbose message if required
  if (!print_only & interactive() & verbose) {
    message(
      paste(
        "Normality was assessed using the",
        normality_info,
        "because the sample size",
        paste0("(n=", nrow(df), ")"),
        "is",
        ifelse(
          nrow(df) > 5000,
          "greater than",
          ifelse(nrow(df) == 5000, "equal to", "less than")
        ),
        "5000.\nNon-significant p-values (>0.05) indicate normality."
      )
    )
  }

  # Return output
  if (print_only) {
    if (copy_to_clipboard) {
      clipr::write_clip(out_table)
    } else {
      out_table
    }
  } else {
    return(invisible(out))
  }
}