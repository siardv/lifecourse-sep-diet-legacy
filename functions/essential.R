essential <- function(df, formula, ...) {
  if (any(is.na(df))) {
    stop("Data contains missing values.")
  }

  if (class(formula) != "formula") {
    stop("formula must be a formula object")
  }
  ecols <- rownames(attr(terms(formula), "factors"))


  ind <- sapply(ecols, match, table = names(df), nomatch = NA)
  if (any(is.na(ind))) {
    stop("Some columns in the formula are not present in the data")
  }

  df_sub <- df[, ind]
  return(df_sub)
}