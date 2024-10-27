numeric_data_frame <- function(input_data) {
  input_data <- labelled::user_na_to_na(input_data)
  if (is.factor(input_data)) {
    # Convert factor to numeric based on levels
    output_data <- as.numeric(as.character(input_data))
  } else if (haven::is.labelled(input_data)) {
    # Convert labelled data to numeric values directly
    output_data <- as.numeric(input_data)
  }

  if (is.data.frame(input_data)) {
    output_data <- lapply(input_data, function(col) as.numeric(as.character(col)))

    output_data <- data.frame(output_data, stringsAsFactors = FALSE)
  }

  return(output_data)
}