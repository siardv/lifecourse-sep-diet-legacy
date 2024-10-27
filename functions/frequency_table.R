frequency_table <- function(variable, data_frame, question_label, add_caption = FALSE) {
  var_name <- deparse(substitute(variable))
  col_data <- labelled::user_na_to_regular_na(data_frame[, var_name])
  labels <- attr(col_data, "labels") %||% labelled::get_value_labels(col_data)[[1]]
  labeled_values <- data.frame(value = as.character(labels), label = dQuote(names(labels)))
  freq_data <- summarytools::freq(col_data, transpose = TRUE, cumul = FALSE, cumul.pattern = FALSE)
  freq_data <- rownames_to_column(freq_data, var = "value")
  merged <- dplyr::full_join(freq_data, labeled_values, by = "value") %>%
    dplyr::select(value, label, dplyr::everything()) %>%
    dplyr::arrange(gtools::mixedorder(value)) #%>%
    #dplyr::filter(!(value == "<NA>" & Freq == 0))
  caption <- NULL
  if (add_caption) {
    caption <- sprintf("%s %s", ifelse(missing(question_label), "Responses to the question", ifelse(grepl("\\?", question_label), "Responses to the Question", paste0(question_label, " responses"))), dQuote(ifelse(missing(question_label), var_name, question_label)))
  }
  knitr::kable(merged,
    digits = 2, align = "l",
    format.args = list(drop0trailing = TRUE),
    format = "html",
    row.names = FALSE,
    caption = caption,
    escape = FALSE
  )
}