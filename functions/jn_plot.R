jn_plot <- function(x) {
  purrr::map(second_order_interactions(x), ~ {
    johnson_neyman_plot(x, .x[[1]], .x[[2]])
  })
}