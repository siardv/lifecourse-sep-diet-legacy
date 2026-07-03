create_formula <- function(...) {
  first_argument <- list(...)[[1]]
  formula_str <- deparse(formula(first_argument))
  clean_formula_str <- gsub("\\s+", " ", paste(formula_str, collapse = ""))
  clean_formula <- as.formula(clean_formula_str, env = parent.frame())
  return(clean_formula)
}