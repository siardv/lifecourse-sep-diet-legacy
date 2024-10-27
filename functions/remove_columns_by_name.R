remove_columns_by_name <- function(data_frame, ...) {
  call_elements <- sys.call()[-1]
  element_names <- as.character(call_elements)
  columns_to_remove <- element_names[element_names %in% names(data_frame)]
  return(data_frame[, !names(data_frame) %in% columns_to_remove])
}