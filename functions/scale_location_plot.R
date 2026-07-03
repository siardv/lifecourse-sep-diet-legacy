scale_location_plot <- function(model_fit, plot_title, point_color, use_latex = TRUE, verbose = FALSE) {
  if (verbose & interactive()) {
    message_text <- paste(
      "Scale-Location plot for checking homoscedasticity.",
      "Points should be randomly dispersed around the horizontal line.",
      "If the points show a pattern (e.g., a funnel shape), the data may be heteroscedastic.",
      sep = "\n"
    )
    cat(cli::col_silver(message_text))
  }
  if (missing(point_color)) {
    point_color <- viridis::viridis(4)[2]
  }
  if (use_latex) {
    plot_title <- latex2exp::TeX(plot_title, bold = TRUE)
  }

  standardized_residuals(model_fit, "std_resid")

  model_data <- broom::augment(model_fit)
  ggplot2::ggplot(model_data, ggplot2::aes(.fitted, sqrt(abs(std_resid)))) +
    ggplot2::geom_point(color = point_color, alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess") +
    ggplot2::ggtitle(plot_title, subtitle = "Scale-Location") +
    ggplot2::labs(
      x = "Fitted values",
      y = expression(sqrt(
        phantom(x) * abs(" Std. Residuals ") * phantom(x)
      ))
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.title = ggplot2::element_text(size = 16, face = "bold"), # Title size
      plot.subtitle = ggplot2::element_text(size = 12, face = "italic") # Subtitle size
    ) +
    ggplot2::scale_size_continuous(range = c(1, 2))
}
