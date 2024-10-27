cooks_distance_df <- function(model_fit, data) {
  model_data <- broom::augment(model_fit)
  cooks_d <- cooks_distance_lmrob(model_fit, data = data)
  cooks_df <- data.frame(Index = 1:length(cooks_d), CooksD = cooks_d)
  return(cooks_df)
}