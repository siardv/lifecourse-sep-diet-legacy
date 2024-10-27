named_list <- function(...) {
  infered_names <- rlang::enquos(...)
  values <- list(...)
  given_names <- names(values)
  infered_names <- as.character(sapply(infered_names, rlang::as_label))
  if (!is.null(given_names) && length(given_names) > 0) {
    given_names[given_names == ""] <- infered_names[given_names == ""]
  } else {
    given_names <- infered_names
  }
  result <- setNames(
    lapply(values, eval),
    given_names
  )
  return(result)
}
