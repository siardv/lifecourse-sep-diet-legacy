# Function to identify and remove multivariate outliers
multivariate_outliers <- function(df, threshold = 0.001, exclude = NULL) {
  # Remove missing values and replace infinities
  df_without_na_inf <- remove_constant_and_all_missing(df) %>%
    datawizard::replace_nan_inf()

  # Keep only complete cases with some variance
  df_without_zero_sd <- df_without_na_inf %>%
    dplyr::select(where(~ sd(.x, na.rm = TRUE) != 0))
  df_clean <- df_without_zero_sd %>%
    dplyr::filter(complete.cases(.))
  df_clean <- numeric_data_frame(df_clean)

  # Optionally exclude specified columns
  if (!is.null(exclude)) {
    df_clean <- df_clean[, !names(df_clean) %in% exclude]
  }

  # Identify outliers using LOF method
  lof_outliers <- performance::check_outliers(df_clean, method = "lof", threshold = threshold, ID = TRUE)
  print(lof_outliers)
  cat("\n")

  # Get outlier row indices
  lof_outliers_indices <- lof_outliers %>%
    attr("outlier_count") %>%
    sapply(`[[`, "Row") %>%
    as.numeric() %>%
    unique()

  # Remove outliers from original data frame

  cols <- lapply(list(df, df_clean), names) %>% Reduce(f = intersect)

  df_hash <- df[, cols] %>%
    numeric_data_frame() %>%
    dplyr::mutate(hash = apply(., 1, digest::digest)) %>%
    dplyr::select(hash)

  df_clean_hash <- df_clean[lof_outliers_indices, cols] %>%
    dplyr::mutate(hash = apply(., 1, digest::digest)) %>%
    dplyr::select(hash)

  matching_rows <- which(df_hash$hash %in% df_clean_hash$hash)

  df_without_outliers <- df[-matching_rows, ]

  # Return df without outliers, message, and indices
  return(list(
    df = df_without_outliers,
    msg = lof_outliers,
    id = matching_rows
  ))
}

### Detecting Multivariate Outliers Using Local Outlier Factor
# Outlier detection is an important step in data cleaning and preprocessing, as outliers can skew and distort statistical analyses. One method for identifying multivariate outliers is through the use of Local Outlier Factor (LOF), a density-based algorithm that compares the local density of data points to their neighbors (Breunig et al., 2000). This technique can highlight observations that have substantially lower local density relative to surrounding data points, making them outlier candidates.
# Local Outlier Factor: Based on a K nearest neighbors algorithm, LOF compares the local density of a point to the local densities of its neighbors instead of computing a distance from the center (Breunig et al., 2000). Points that have a substantially lower density than their neighbors are considered outliers. A LOF score of approximately 1 indicates that density around the point is comparable to its neighbors. Scores significantly larger than 1 indicate outliers. The default threshold of 0.025 will classify as outliers the observations located at ⁠qnorm(1-0.025) * SD)⁠ of the log-transformed LOF distance. Requires the dbscan package.
# This function takes a data frame `df` as input and returns a list containing the cleaned dataframe with outliers removed (`df_without_outliers_2`), details of the identified outliers (`msg`), and their indices (`id`). Constants and missing values are first removed. The data is then subset to only numeric columns with variation. Local outlier factor analysis is applied using the `performance::check_outliers()` function, with a default threshold of 0.001. Indices of observations exceeding this threshold are extracted and removed from the original dataframe.
# The local outlier factor algorithm is an effective technique for detecting anomalous multivariate data points that deviate substantially from the norm. By comparing local densities, it avoids some limitations of distance-based outlier detection methods. This makes LOF well-suited for identifying outliers in complex, real-world data. Our implementation provides a flexible tool for multivariate outlier detection and removal as part of the data cleaning process.