combined_qq_plot <- function(x, model_names, plot_title, add_jitter = FALSE) {
  # Define color palette
  color_palette <- RColorBrewer::brewer.pal(min(12, length(x)), "Set3")

  # Adjust color palette if more than 12 models
  if (length(x) > 12) {
    color_palette <- grDevices::colorRampPalette(color_palette)
    (length(x))
  }

  # Create plot data
  plot_data <- purrr::map2_df(
    x, model_names,
    ~ data.frame(
      std_resid = standardized_residuals(.x),
      Model = .y
    )
  )

  plot_title <- ifelse(missing(plot_title),
    "Q-Q Plot of Standardized Residuals",
    plot_title
  )

  # Create ggplot object
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(sample = std_resid, colour = Model)
  ) +
    ggplot2::stat_qq() +
    ggplot2::geom_abline(
      intercept = 0, slope = 1, linetype = "solid",
      size = 1, color = "black"
    ) +
    ggplot2::scale_color_manual(values = color_palette, breaks = model_names) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(
      x = "Theoretical Quantiles",
      y = "Standardized Residuals", color = "Model"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right", legend.direction = "vertical")

  # Add jitter if requested
  if (add_jitter) {
    plot <- plot + ggplot2::geom_jitter(width = 0.1, height = 0)
  }

  # Return plot
  return(plot)
}
