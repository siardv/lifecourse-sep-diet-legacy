clean_up_input <- function(x) {
  x <- tolower(x)
  x <- textclean::replace_non_ascii(x)
  x <- gsub("[^[:alnum:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}

main <- haven::read_sav("data/main.sav")
occupation_columns <- c("occupation", "occupation_partner", "occupation_mother", "occupation_father")

main_with_copies <- main %>%
  dplyr::mutate(dplyr::across(tidyselect::all_of(occupation_columns),
    list(init = ~., cleaned = ~ clean_up_input(.)),
    .names = "{col}_{fn}"
  ))

corrected_df <- readxl::read_xlsx("data/corected_isco08_codes.xlsx") %>%
  dplyr::select(idnummer, source, occupation, isco08, isco08_init, isco08_new)

grouped <- corrected_df %>%
  dplyr::group_by(source) %>%
  dplyr::group_split(.keep = TRUE) %>%
  setNames(unique(corrected_df$source))

respondent <- grouped$respondent %>%
  dplyr::rename(
    BRC14 = isco08_init,
    occupation_cleaned = occupation,
    isco08 = isco08,
    isco08_new = isco08_new
  ) %>%
  dplyr::select(-source)

partner <- grouped$partner %>%
  dplyr::rename(
    BRC14_partner = isco08_init,
    occupation_partner_cleaned = occupation,
    isco08_partner = isco08,
    isco08_new_partner = isco08_new
  ) %>%
  dplyr::select(-source)

mother <- grouped$mother %>%
  dplyr::rename(
    BRC14_mother = isco08_init,
    occupation_mother_cleaned = occupation,
    isco08_mother = isco08,
    isco08_new_mother = isco08_new
  ) %>%
  dplyr::select(-source)

father <- grouped$father %>%
  dplyr::rename(
    BRC14_father = isco08_init,
    occupation_father_cleaned = occupation,
    isco08_father = isco08,
    isco08_new_father = isco08_new
  ) %>%
  dplyr::select(-source)



remove_rows_with_all_na <- function(x) {
  x[rowSums(!is.na(x)) > ncol(x) - (ncol(x) - 1), ]
}

datasets <- list(respondent, partner, mother, father) %>%
  lapply(remove_rows_with_all_na) %>%
  c(list(main_with_copies))

warn <- c(
  "Number of rows in joined data do not match main_with_copies.",
  "Number of rows in datasets do not match."
)

if (length(unique(sapply(datasets, nrow))) == 1) {
  message("Number of rows match.")
  joined_data <- Reduce(function(x, y) dplyr::full_join(x, y[[1]], by = c("idnummer", y[[2]]), keep = TRUE),
    Map(list, datasets[-1], paste0("occupation_", c("", "partner_", "mother_", "father_"), "cleaned")),
    init = main_with_copies
  )
  eq_rows <- Reduce(f = identical, sapply(list(joined_data, main_with_copies), nrow))
  if (eq_rows) {
    warning(warn[1])
  }
} else {
  warning(warn[2])
}



groups %>%
  dplyr::mutate(
    dplyr::across(dplyr::matches("_new_"),convert_to_isei08))
      c(isco08_new, isco08_new_partner, isco08_new_mother, isco08_new_father),
      ~ sapply(., convert_to_isei08),
      .names = "{.col}_ISEI08"
    )
  )
%>%
  dplyr::rename(
    isei08 = isco08_new_ISEI08,
    isei08_partner = isco08_new_partner_ISEI08,
    isei08_mother = isco08_new_mother_ISEI08,
    isei08_father = isco08_new_father_ISEI08
  )


setdiff(names(df_with_isei08), names(main_with_corrected_codes_all))




# writexl::write_xlsx(main_with_corrected_codes, "data/main_with_corrected_codes.xlsx")
#
# readxl::read_xlsx("data/main_with_corrected_codes.xlsx")



haven::write_sav(df_with_isei08, "data/main_with_isei08.sav")

plot_normalized_histograms(isei08, BRC14, df_with_isei08, c("ISEI08", "BRC14", tail_str = "(respondent)"))
plot_normalized_histograms(isei08_partner, BRC14_partner, df_with_isei08, c("ISEI08", "BRC14", tail_str = "(partner)"))
plot_normalized_histograms(isei08_mother, BRC14_mother, df_with_isei08, c("ISEI08", "BRC14", tail_str = "(mother)"))
plot_normalized_histograms(isei08_father, BRC14_father, df_with_isei08, c("ISEI08", "BRC14", tail_str = "(father)"))

names(df_with_isei08)