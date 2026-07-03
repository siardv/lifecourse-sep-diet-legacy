model_summary <- function(models, model_names = NULL, adjusted) {
  model_names <- if (!is.null(names(models))) {
    names(models)
  } else if (is.null(model_names)) {
    paste("Model", seq_along(models))
  } else {
    model_names
  }

  term_names <- unique(unlist(lapply(models, function(x) variable.names(x$fit))))

  model_infos <- lapply(models, model_info, term_names = term_names)
  combined_table <- do.call(cbind, model_infos)
  combined_table <- data.frame(combined_table)

  model_colnames <- unlist(lapply(seq_along(model_names), function(i) {
    paste0(model_names[i], colnames(model_infos[[i]]))
  }))

  colnames(combined_table) <- model_colnames

  n_obs_text <- paste0("N = ", nrow(model.frame(models[[1]]$fit)), ".")

  caption_text <- paste(
    ifelse(class(models[[1]]$fit) == "lmrob", "Robust", "OLS"),
    "Linear Regression Models with Bootstrapped Estimates",
    ifelse(adjusted, "(adjusted for influential observations)", "")
  )
  p_value_note <- tth::tth("$^\\text{***}~\\textit{p} \\leq 0.001;\\, ^\\text{**}~\\textit{p} \\leq 0.01;\\, ^\\text{*}~\\textit{p} \\leq 0.05;$")
  footnote_text <- paste(
    p_value_note, n_obs_text,
    "All values are bootstrapped estimates. Standard errors are in parentheses. P-values are calculated based on bootstrapped standard errors.",
    "Bootstrap sampling was performed with 1000 replications.",
    ifelse(adjusted, "Influential observations were identified using Cook's distance and omitted from the data used in the analysis.", ""),
    sep = " "
  )

  htmlTable::htmlTable(
    x = combined_table,
    escape = FALSE,
    rnames = beautify_names(term_names),
    label = "robust-model-table",
    align = "l",
    caption = caption_text,
    tfoot = paste(
      tth::tth("\\textit{Note}:\\ "),
      footnote_text
    ),
    css.cell = "padding-left: 1em; padding-right: 1em; text-align: center;"
  )
}