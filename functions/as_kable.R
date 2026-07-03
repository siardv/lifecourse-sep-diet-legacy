as_kable <- function(tbl, cols = NULL, format = "html", row.names = FALSE,
                     digits = 4, align = "l", escape = FALSE, ...) {
  # Assign default columns if none provided
  cols <- cols %||% get0("col_names", envir = parent.frame(), inherits = TRUE) %||% colnames(tbl)

  # Generate kable table with specified arguments
  knitr::kable(
    x = tbl,
    format = format,
    digits = digits,
    row.names = row.names,
    align = align,
    col.names = cols,
    escape = escape,
    ...
  )
}