process_column_combo <- function(combo, df, min_pts = 3, outlier_threshold = 0.001) {
  subset_data <- df %>%
    dplyr::select(dplyr::all_of(combo)) %>%
    dplyr::filter(complete.cases(.))
  if (base::nrow(subset_data) >= 3) {
    lof_scores <- dbscan::lof(subset_data, minPts = min_pts)
    outliers <- which(lof_scores > stats::quantile(lof_scores, probs = 1 - outlier_threshold))
    return(list(outliers = outliers, weight = length(combo)))
  } else {
    return(NULL)
  }
}