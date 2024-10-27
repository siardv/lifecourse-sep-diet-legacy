leverage_plot <- function(model_fit, plot_title, use_latex = TRUE, verbose = FALSE) {
  # Residuals vs Leverage: Identifies high leverage points
  if (verbose & interactive()) {
    message_text <- paste(
      "Residuals vs Leverage plot identifies high leverage points.",
      "The plots show the standardized residuals against the leverage values.",
      "In the plot, the color of the points represents the influence of the observation.",
      sep = "\\n"
    )
    cat(cli::col_silver(message_text))
  }
  model_data <- broom::augment(model_fit)

  if (use_latex) {
    plot_title <- latex2exp::TeX(plot_title, bold = TRUE)
  }

  standardized_residuals(model_fit, "std_resid")

  influence_weights <- 2 / (1 - hatvalues(model_fit))
  color_thresholds <- quantile(influence_weights, probs = seq(1 / 3, 1, by = 1 / 3))
  leverage_scores <- hatvalues(model_fit)

  ggplot2::ggplot(model_data, ggplot2::aes(x = leverage_scores, y = std_resid, color = influence_weights)) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = NA, colour = NA)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.6) +
    ggplot2::ggtitle(plot_title, subtitle = "Residuals vs. Leverage") +
    ggplot2::labs(x = "Leverage", y = "Std. Residuals", color = "Influence") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, face = "italic")
    ) +
    ggplot2::scale_color_continuous(
      breaks = color_thresholds,
      labels = c("Low", "Medium", "High")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Influence"))
}
