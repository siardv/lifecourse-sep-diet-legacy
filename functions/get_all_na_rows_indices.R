#' Identify indices of cases where both educational attainment and occupation
#' classifications are unspecified ("NA") across multiple variables of
#' interest for a given sample of individuals
#' Account for instances where companion or partner data may also be present
get_all_na_rows_indices <- function(category, data) {
  na_cols <- grep(category, names(data), ignore.case = TRUE, value = TRUE)
  has_partner <- all(grepl("partner", na_cols, ignore.case = TRUE))
  if (has_partner) {
    has_partner_col <- names(which.min(colSums(!is.na(data[na_cols]))))
    na_cols <- setdiff(na_cols, has_partner_col)
  }
  na_rows <- rowSums(is.na(data[na_cols])) == length(na_cols)
  if (has_partner) {
    na_ind <- which(na_rows & data[[has_partner_col]] == 1)
  } else {
    na_ind <- which(na_rows)
  }
  attr(na_ind, "col_names") <- na_cols
  return(na_ind)
}