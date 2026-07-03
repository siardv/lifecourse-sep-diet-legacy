column_type_conversion <- function(df, as_attr = TRUE) {
  char_vars <- names(which(sapply(df, is.character)))
  df[char_vars] <- lapply(df[char_vars], function(x) {
    type.convert(x, as.is = TRUE)
  })
  char_vars <- setNames(names(df) %in% char_vars, names(df))

  if (as_attr) {
    attr(df, "column_types") <- char_vars
    return(df)
  } else {
    char_vars
  }
}
