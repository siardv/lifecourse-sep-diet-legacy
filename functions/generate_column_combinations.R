generate_column_combinations <- function(df, max_combo_size = 3) {
  numeric_cols <- df %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    names()
  all_combos <- lapply(3:max_combo_size, function(k) {
    utils::combn(numeric_cols, k, simplify = FALSE)
  })
  return(unlist(all_combos, recursive = FALSE))
}
