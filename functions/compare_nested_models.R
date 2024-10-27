compare_nested_models <- function(fits, leverage, method) {
  model_list <- purrr::map(fits[[leverage]][[method]], ~ .x$fit)
  model_comparisons <- combn(length(model_list), 2, simplify = FALSE) %>%
    purrr::map_df(~ {
      cbind(
        data.frame(
          A = .x[1],
          B = .x[2],
          Models = sprintf("%d vs. %d", .x[1], .x[2])
        ),
        compare_models(model_list[.x])
      )
    })

  model_comparisons %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("p_value"),
        ~ format_p_value(., add_stars = TRUE, omit_p_label = TRUE)
      )
    )
}