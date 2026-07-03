missForest_params <- function(data) {
  # Number of columns (features) in the dataset
  num_cols <- ncol(data)

  # Number of rows (observations) in the dataset
  num_rows <- nrow(data)

  # Proportion of missing values in the dataset
  missing_proportion <- sum(is.na(data)) / (num_cols * num_rows)

  # Determine mtry based on the number of columns
  mtry <- max(1, floor(sqrt(num_cols)))  # sqrt(p) or at least 1

  # Determine mtry based on the number of columns
  # sqrt(p) for smaller datasets (Breiman, 2001), increased to p/3 for large feature-rich datasets
  mtry <- max(1, floor(sqrt(num_cols)))  # sqrt(p) or at least 1
  if (num_cols > 50) {
    mtry <- max(1, floor(num_cols / 3))  # Increase mtry for large datasets with many features
  }

  # Determine ntree based on dataset size (rows x columns)
  ntree <- ifelse(num_rows * num_cols < 50000, 500, 1000)  # More trees for larger datasets

  maxiter <- floor(5 + (10 * missing_proportion))  # Linear scale from 5 to 15

  # Return the calculated parameters
  return(list(
    mtry = mtry,
    ntree = ntree,
    maxiter = maxiter
  ))
}