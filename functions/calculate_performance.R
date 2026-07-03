calculate_performance <- function(model_fits) {
  purrr::map_depth(model_fits, 3, ~ {
    # Extract performance metrics
    perf <- performance::performance(.x$fit)

    # Extract R2 and its p-value
    r2 <- summary(.x$fit)$r.squared
    # Extract p-value for R2
    r2_p_value <<- summary(.x$fit)$coefficients[, "Pr(>|t|)"][2]

    # Convert to data frame and add p-value
    df <- data.frame(perf)
    df$r2_p_value <- r2_p_value
    df
  }) %>%
    unlist(recursive = FALSE) %>%
    purrr::list_transpose() %>%
    purrr::map2(.y = seq_along(.), ~ rownames_to_column(.x, var = "id") %>%
      dplyr::mutate(
        leverage = gsub("(.*)\\.(.*)", "\\1", .$id),
        method = gsub("(.*)\\.(.*)", "\\2", .$id),
        model = .y, .before = 1
      ) %>%
      remove_columns_by_name("id")) %>%
    Reduce(f = dplyr::bind_rows)
}
