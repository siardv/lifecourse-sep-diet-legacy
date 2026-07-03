format_coef <- function(coef, se, p) {
  if (is.na(coef)) {
    return("—")
  } else {
    p_value <- format_p_value(p, add_stars = TRUE, omit_p_label = TRUE)
    white_space <- ifelse(coef >= 0 & coef < 10, "<span style=\"display:inline-block; width: 4px\"> </span>", "")
    return(sprintf("%s%.3f<sup>%s</sup><br><span style='font-size: 0.8em'>(%.3f)</span>", white_space, coef, p_value, se))
  }
}