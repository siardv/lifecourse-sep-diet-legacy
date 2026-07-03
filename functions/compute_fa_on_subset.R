compute_factor_analysis <- function(data) {
  data <- numeric_data_frame(data)
 # data <- data[complete.cases(data), ]
  n_row <- nrow(data)

  # Adding a small constant to the diagonal to prevent singular matrix (meaning all variables are perfectly correlated)
  cor_matrix <- cor(data, use = "pairwise.complete.obs")
  cor_matrix <- cor_matrix + diag(ncol(cor_matrix)) * 1e-7

  # Factor analysis with adjusted correlation matrix
  fa_res <- psych::fa(r = cor_matrix, nfactors = 1, scores = "Harman", covar = FALSE)
  names_comm <- names(fa_res$communalities)
  isco <- grep("ISCO20", names_comm)
  fa_res$communalities[isco] <- fa_res$communalities[isco] - 1e-7
  h2_sorted <- sort(fa_res$communalities, decreasing = TRUE)
  h2_df <- data.frame(
    vars = names(h2_sorted),
    h2 = h2_sorted, row.names = NULL
  )

  h2_df$max_checkmark <- ifelse(
    h2_df$h2 == max(h2_df$h2), "&#10003;", "&emsp;"
  )
  h2_df$n <- n_row
  return(h2_df)
}

compute_fa_on_subset <- function(data) {
  family_roles <- c("partner", "mother", "father")

  patterns <- c(paste0(
    "^(?!.*(",
    paste0(family_roles, collapse = "|"),
    ")).*$"
  ), family_roles) %>%
    lapply(grep, grep("ISCO|BRC",
      names(data),
      ignore.case = TRUE, value = TRUE
    ),
    ignore.case = TRUE, value = TRUE, perl = TRUE
    )

  data_num <- apply(data[unlist(patterns)], 2, as.numeric) %>%
    apply(2, function(x) ifelse(x == 9, NA, x)) %>%
    data.frame()

  numeric_data_frame(x)
  hh_members <- c("respondent", family_roles)
  fa_res <- Reduce(f = rbind, lapply(1:4, function(i) {
    cbind(
      hh_member = hh_members[i],
      compute_factor_analysis(data_num[patterns[[i]]])
    )
  }))

  return(fa_res)
}