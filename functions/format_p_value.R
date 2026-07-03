format_p_value <- function(p_value, add_stars = FALSE, use_concat = TRUE, omit_p_label = FALSE, verbose = TRUE) {
  p_value <- as.numeric(p_value)

  if (verbose && all(p_value > 0.05) && interactive()) {
    message("All p-values are greater than 0.05, no changes made.")
    return(p_value)
  }

  formatted_p <- ifelse(p_value < 0.001, "≤0.001", sprintf("%.3f", p_value))
  formatted_p[is.na(formatted_p)] <- ""
  formatted_p <- format(formatted_p, nsmall = 4, justify = "left")
  if (add_stars) {
    stars_label <- ifelse(is.na(p_value), " ",
      ifelse(p_value < 0.001, "***",
        ifelse(p_value < 0.01, "** ",
          ifelse(p_value < 0.05, "*  ", " ")
        )
      )
    )
    stars_label <- format(stars_label, justify = ifelse(omit_p_label, "right", "left"))
    if (omit_p_label) {
      formatted_p <- stars_label
    } else {
      formatted_p <-
        if (use_concat) {
          paste0(formatted_p, stars_label)
        } else {
          list(p = formatted_p, stars = stars_label)
        }
    }
  } else {
    if (!use_concat) {
      formatted_p <- list(p = formatted_p, stars = "")
    }
  }

  return(as.character(formatted_p))
}
