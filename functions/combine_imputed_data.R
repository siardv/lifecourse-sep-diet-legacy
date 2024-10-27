# This function ensures that continuous variables are taken from the global imputation, and categorical variables are taken from the variable-wise imputation. The cbind function is used to combine these two sets of imputed data into a single dataset.
combine_imputed_data <- function(cols, imp_glob, imp_varw) {
  stopifnot(is.list(cols))
  imputed_cont <- imp_glob$ximp[, cols$continuous]
  imputed_cat <- imp_varw$ximp[, cols$categorical]

  # Convert factors to characters to avoid issues with levels
  imputed_cat <- as.data.frame(lapply(imputed_cat, as.character), stringsAsFactors = FALSE)

  combined_imputed <- cbind(imputed_cont, imputed_cat)
  return(combined_imputed)
}