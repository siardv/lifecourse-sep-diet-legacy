copy_labels <- function(source, target) {
  common_names <- intersect(names(target), names(source))
  unique_names <- setdiff(names(target), names(source))
  col_types <- paste0("as.", sapply(source[common_names], typeof, USE.NAMES = FALSE))
  target_reformatted <- purrr::map2_df(target[common_names], col_types, ~ getFunction(.y)(.x))
  target_reformatted <- labelled::copy_labels(source, target_reformatted)
  target_reformatted <- cbind(target_reformatted, target[unique_names])
  target_reformatted <- target_reformatted[, names(target)]
  if (any(grepl("tbl", class(source)))) {
    tibble::as_tibble(target_reformatted)
  } else {
    target_reformatted
  }
}