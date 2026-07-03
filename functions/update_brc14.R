update_brc14 <- function(column_name, data, mapping) {
  values <- unique(na.omit(data[[column_name]]))
  labels <- sapply(values, function(x) unique(mapping$BRC2014beroepsgroep_label[as.numeric(mapping$BRC2014beroepsgroep) == x]))
  values_named <- setNames(values, labels)
  for (i in seq_along(values_named)) {
    eval.parent(substitute(update_value_label(data, column_name, as.numeric(values_named[i]), names(values_named)[i])))
  }
}