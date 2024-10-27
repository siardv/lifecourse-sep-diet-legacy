cooks_distance_plot <- function(model_fit, dataset, plot_title, line_color, use_latex = TRUE, verbose = FALSE) {
  if (verbose & interactive()) {
    message_text <- paste(
      "Cook's Distance plot for identifying influential observations.",
      "Points above the horizontal line (4/n) may be considered influential.",
      sep = "\n"
    )
    cat(cli::col_silver(message_text))
  }
  if (missing(line_color)) {
    line_color <- viridis::viridis(4)[2]
  }
  if (use_latex) {
    plot_title <- latex2exp::TeX(plot_title, bold = TRUE)
  }
  cooks_data <- cooks_distance_df(model_fit, dataset)
  cooks_values <- cooks_data[[2]]

  ggplot2::ggplot(cooks_data, ggplot2::aes(x = Index, y = CooksD)) +
    ggplot2::geom_bar(stat = "identity", fill = "black", alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 4 / length(cooks_values), color = line_color) +
    ggplot2::ggtitle(plot_title, subtitle = "Influential observations with Cook's Distance") +
    ggplot2::labs(x = "Observation Index", y = "Cook's Distance") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, face = "italic")
    )
}