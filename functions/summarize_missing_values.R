summarize_missing_values <- function(x, print_table = TRUE) {
  if (all(c("n_miss", "pct_miss") %in% names(x))) {
    mvs <- x
  } else {
    mvs <- naniar::miss_var_summary(x, order = TRUE)
  }
  mvs_table <- knitr::kable(mvs,
    format = "html",
    digits = 2,
    caption = glue::glue("Missing values in initial data (n={nrow(x)})"),
    col.names = c("", "$n$", "$\\%$"),
    format.args = list(zero.print = "0")
  ) %>%
    kableExtra::footnote(
      "Missing values as indicated by both user-defined and system-defined missing values."
    )
  if (print_table) {
    print(mvs_table)
  }
  return(mvs)
}