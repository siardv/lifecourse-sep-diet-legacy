remove_rows_with_all_na <- function(x) {
  x[rowSums(!is.na(x)) > ncol(x) - (ncol(x) - 1), ]
}