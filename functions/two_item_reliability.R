two_item_reliability <- function(two_item_data, verbose = FALSE,
                                 latex_names = TRUE) {
  # https://doi.org/10.1007/s00038-012-0416-3

  # Check if the data frame has exactly two columns
  # The paper assumes a two-item scale
  if (ncol(two_item_data) != 2) {
    stop("The input data frame must have exactly two columns.")
  }

  .data <- data.frame(sapply(two_item_data, as.numeric))
  na_omitted <- nrow(.data) - complete.cases(.data)
  .data <- .data[complete.cases(.data), ]

  # Calculate Cronbach's alpha
  # The paper discusses Cronbach's alpha as a common
  # but often inappropriate reliability estimate
  cronbach_alpha <- psych::alpha(.data)$total$raw_alpha

  # Calculate Pearson correlation coefficient
  # The paper states Pearson r is not an adequate reliability
  # estimate for a two-item scale
  pearson_r <- cor(.data[[1]], .data[[2]], method = "pearson")

  # cronbach_alpha and pearson_r are alternative reliability estimates,
  # but the paper recommends Spearman-Brown as the most appropriate

  # Spearman-Brown prediction formula using Pearson r
  # The paper recommends Spearman-Brown as the most appropriate
  # reliability estimate for a two-item scale
  spearman_brown <- 2 * pearson_r / (1 + pearson_r)

  out <- data.frame(
    cronbach_alpha = cronbach_alpha,
    pearson_r = pearson_r,
    spearman_brown = spearman_brown
  )

  print_results_with_info <- function() {
    x <- sapply(out, round, 3)

    italic_r <- cli::style_italic("r")
    cli::cli_alert_warning("Cronbach's alpha: {x[[1]]}")
    cli::cli_alert_warning(paste("Pearson", italic_r, ":", x[[2]]))
    cli::cli_alert_success("Spearman-Brown: {x[[3]]}")
    cat("\n")

    ref <- cli::style_hyperlink(
      "Eisinga et al. (2012)",
      "https://doi.org/10.1007/s00038-012-0416-3"
    )

    cli::cli_alert_info(paste(
      ref, "argue that the Spearman-Brown prediction",
      "formula utilizing the Pearson correlation",
      "coefficient (", italic_r, ") provides the",
      "optimal reliability estimate for a scale",
      "comprising merely two items. In contrast to",
      "Pearson's", italic_r, "or Cronbach's alpha,",
      "the Spearman-Brown coefficient explicitly",
      "incorporates the scale's length into the",
      "reliability calculation. Values approximating",
      "1 indicate satisfactory reliability."
    ))
  }

  if (verbose) {
    print_results_with_info()
  }

  if (latex_names) {
    names(out) <- c("$\\alpha$", "$r$", "$\\rho_{SB}$")
  }

  attr(out, "na_omitted") <- na_omitted
  return(out)
}
