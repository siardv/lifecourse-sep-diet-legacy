extract_term_ranges <- function(model, colnames) {
  model_frame <- model.frame(model)
  term_ranges <- sapply(model_frame, function(term) range(term, na.rm = TRUE))
  term_ranges_df <- as.data.frame(t(term_ranges))
  colnames(term_ranges_df) <- c("min", "max")
  if (!missing(colnames)) {
    term_ranges_df <- term_ranges_df[rownames(term_ranges_df) == colnames, ]
  }
  return(term_ranges_df)
}
