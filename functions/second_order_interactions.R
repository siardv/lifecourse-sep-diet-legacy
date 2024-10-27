second_order_interactions <- function(fit) {
  model_terms <- terms(fit)
  interaction_order <- attr(model_terms, "order")
  # extract second-order interaction terms
  interaction_terms <- attr(model_terms, "term.labels")
  interaction_terms <- interaction_terms[interaction_order == 2]
  interaction_terms <- strsplit(interaction_terms, ":")
  return(interaction_terms)
}