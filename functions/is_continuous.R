is_continuous <- function(model_data, pred_var, modx_var) {
  vars <- model_data[, c(pred_var, modx_var)]
  all(sapply(vars, function(k) length(unique(k)) > 2))
}