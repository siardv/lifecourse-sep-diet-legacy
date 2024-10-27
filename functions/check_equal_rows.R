check_equal_rows <- function(...) {
  datasets <- list(...)
  dataset_names <- sapply(substitute(list(...))[-1], deparse)
  row_counts <- sapply(datasets, function(x) {
    if (is.null(x)) {
      NA
    } else {
      nrow(x)
    }
  })

  if (any(is.na(row_counts))) {
    na_datasets <- dataset_names[is.na(row_counts)]
    stop(
      sprintf(
        "The following datasets are NULL or not data frames: %s",
        paste(na_datasets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (length(unique(row_counts)) != 1) {
    counts <- sprintf("%s (%d rows)", dataset_names, row_counts)
    error_msg <- sprintf("Row count mismatch:\n%s", paste(counts, collapse = "\n"))
    stop(error_msg, call. = FALSE)
  }

  invisible(TRUE)
}