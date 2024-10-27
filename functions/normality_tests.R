normality_tests <- function(model_fit) {
  res <- stats::resid(model_fit)
  list(
    anderson_darling = nortest::ad.test(res)$p.value,
    shapiro_wilk = stats::shapiro.test(res)$p.value,
    lilliefors_ks = nortest::lillie.test(res)$p.value,
    cramer_von_mises = nortest::cvm.test(res)$p.value
  ) %>% purrr::map_chr(~ round(.x, 3) %>% format_p_value(add_stars = TRUE, use_concat = TRUE, omit_p_label = FALSE) %>% as.character())
#  out <- sapply(out, function(x) format_p_value(x, add_stars = TRUE, use_concat = TRUE, omit_p_label = FALSE))
#  data.frame(rbind(out))
}