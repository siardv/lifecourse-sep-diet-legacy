labels_to_string <- function(data, column) {
  data_column <- data[, column]
  labels <- sjlabelled::get_labels(data_column, values = TRUE)[[1]]
  label_string <- purrr::map2(labels, names(labels), ~ paste(.y, "=", dQuote(.x, FALSE)))
  label_string <- unlist(label_string, use.names = FALSE)
  if (length(label_string) == 0) {
    label_string <- "No labels in original data."
  } else {
    label_string <- toString(label_string)
  }
  paste("<i>Original coding</i>:", label_string)
}
