convert_to_factor <- function(data_frame, target_columns, excluded_columns = NULL, update_in_place = FALSE) {
  if (missing(target_columns)) {
    cli::cli_alert_warning("No columns specified. All columns will be converted to factors.")
    target_columns <- names(data_frame)
  }
  columns_to_convert <- na.omit(setdiff(names(data_frame[target_columns]), excluded_columns))
  data_frame_copy <- data_frame
  data_frame_copy[columns_to_convert] <-
    lapply(data_frame_copy[columns_to_convert], function(column_data) {
      factor_levels <- attr(column_data, "labels")
      if (!is.null(factor_levels)) {
        factor(column_data,
               levels = unname(factor_levels),
               labels = names(factor_levels)
        )
      } else {
        factor(column_data)
      }
    })
  if (update_in_place) {
    parent_environment <- parent.frame()
    assign(deparse(substitute(data_frame)), data_frame_copy, envir = parent_environment)
  } else {
    return(data_frame_copy)
  }
}