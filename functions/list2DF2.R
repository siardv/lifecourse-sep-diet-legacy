list2DF2 <- function(x, fill = NA) {
  stopifnot(is.list(x))
  max_len <- max(lengths(x))
  padded_list <- lapply(x, function(col) {
    length_diff <- max_len - length(col)
    if (length_diff > 0) {
      c(col, rep(fill, length_diff))
    } else {
      col
    }
  })
  as.data.frame(padded_list, stringsAsFactors = FALSE)
}