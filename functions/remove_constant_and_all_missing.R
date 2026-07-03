remove_constant_and_all_missing <- function(data_frame) {
  has_variation <- sapply(data_frame, sd, na.rm = TRUE) != 0
  not_all_missing <- colSums(is.na(data_frame)) < nrow(data_frame)
  cleaned_data_frame <- data_frame[, has_variation & not_all_missing]
  return(cleaned_data_frame)
}