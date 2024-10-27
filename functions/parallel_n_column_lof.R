parallel_n_column_lof <- function(df, max_combo_size, min_pts = 3, outlier_threshold = 0.001) {
  column_combos <- generate_column_combinations(df, max_combo_size)
  num_cores <- parallel::detectCores() - 1
  cli::cli_alert_info("Using {num_cores} cores for parallel processing.")
  vote_counts <- numeric(nrow(df))
  results <- parallel::mclapply(column_combos, process_column_combo,
    df = df,
    min_pts = min_pts, outlier_threshold = outlier_threshold, mc.cores = num_cores
  )
  for (res in results) {
    if (!is.null(res)) {
      vote_counts[res$outliers] <- vote_counts[res$outliers] + res$weight
    }
  }
  vote_threshold <- max_combo_size
  final_outliers <- which(vote_counts > vote_threshold)
  return(list(
    vote_counts = vote_counts,
    final_outliers = final_outliers
  ))
}
