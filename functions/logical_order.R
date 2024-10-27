logical_order <- function(x) {
  dplyr::case_when(
    grepl("^woman$", x) ~ 1,
    grepl("^education", x) ~ 2,
    grepl("^partner_status$", x) ~ 3,
    grepl("^brc14", x) ~ 4,
    grepl("^DHD", x) ~ 5,
    grepl("^diff", x) ~ 6,
    TRUE ~ 7
  )
}