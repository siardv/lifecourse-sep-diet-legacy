rownames_to_column <- function(df, var = "rn", index = 1, quietly = TRUE) {
  if (!is.data.frame(df)) {
    if (!quietly) {
      cli::cli_inform(c("i" = "The input coersion to a data frame, output may not be as expected."))
      cat("\n")
    }
    df <- as.data.frame(df)
  }
  new_order <- c(var, names(df))
  df[var] <- type.convert(rownames(df), as.is = TRUE)
  if (index == 1) {
    # use `name_order`
  } else if (index > 1 & index <= length(names(df))) {
    nm_ls <- as.list(names(df))
    nm_ls[[index]] <- append(var, nm_ls[[index]])
    new_order <- unlist(nm_ls)
  } else {
    # use `name_order`
    cli::cli_alert_warning("The index provided is out of range, defaulting to 1.")
  }
  df <- df[new_order]
  rownames(df) <- NULL
  return(df)
}
