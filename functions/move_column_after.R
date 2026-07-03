# Function to move a column after a specified column in a data frame
move_column_after <- function(df, column_to_move, reference_column) {
  # Get the current column names
  current_names <- names(df)

  # Determine the positions of the columns
  move_position <- match(column_to_move, current_names)
  reference_position <- match(reference_column, current_names)

  # Check if both columns are present in the data frame
  if (is.na(move_position) || is.na(reference_position)) {
    stop("Either the column to move or the reference column is missing in the data frame.")
  }

  # Remove the column to move from its current position
  current_names <- current_names[-move_position]

  # Create a new order with the column_to_move after reference_column
  new_order <- c(current_names[1:reference_position], column_to_move, current_names[(reference_position + 1):length(current_names)])

  # Reorder the columns in the data frame
  df <- df[, new_order]

  # Return the modified data frame
  return(df)
}
