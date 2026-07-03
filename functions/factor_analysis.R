# Exploratory Factor analysis using MinRes (minimum residual)
factor_analysis <- function(data, ...) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("The input data must be a matrix or a data frame.")
  }

  if (is.data.frame(data)) {
    if (any(!sapply(data, is.numeric))) {
      stop("All columns in the data frame must be numeric.")
    }
  }

  data <- na.omit(data)

  fa_result <- tryCatch(
    {
      psych::fa(data, ...)
    },
    error = function(e) {
      stop("Factor analysis failed: ", e$message)
    }
  )

  if (is.null(fa_result)) {
    stop("Factor analysis returned NULL. Check your data and arguments.")
  }
  results_df <- data.frame(
    unclass(fa_result$loadings),
    h2 = fa_result$communalities,
    u2 = fa_result$uniqueness,
    com = fa_result$complexity
  )

  rownames(results_df) <-
    sapply(gsub("_", " ", names(data)), stringr::str_to_title)

  knitr::kable(
    results_df,
    caption = "Factor Analysis Using MinRes (Minimum Residual) with Oblimin Rotation",
    col.names = c("$\\text{Factor 1}$", "$h^2$", "$u^2$", "$\\text{com}$"),
    digits = 2,
    format = "html"
  ) %>%
    kableExtra::footnote(c(
      "$h^2$ = communality, $u^2$ = uniqueness, com = complexity"
    ), footnote_as_chunk = TRUE)

  return(invisible(fa_result))
}
