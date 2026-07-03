density_difference_plot <- function(original_data, global_imputed, variablewise_imputed, variable, plot_title = NULL, plot_subtitle = NULL, include_footer = FALSE) {
  plot_title <- if (is.null(plot_title)) {
    "Density Differences: Original vs. Imputed Data"
  } else {
    plot_title
  }

  plot_subtitle <- if (is.null(plot_subtitle)) {
    "Comparing Density Distributions of Original Data with Global and Variable-wise Imputations"
  } else {
    plot_subtitle
  }

  footer <- "Positive values indicate higher original data density. Negative values indicate higher imputed data density."

  original_values <- as.numeric(original_data[[variable]])
  global_values <- as.numeric(global_imputed$ximp[[variable]])
  variablewise_values <- as.numeric(variablewise_imputed$ximp[[variable]])

  original_density <- stats::density(original_values, na.rm = TRUE)
  global_density <- stats::density(global_values, na.rm = TRUE)
  variablewise_density <- stats::density(variablewise_values, na.rm = TRUE)

  density_df <- data.frame(
    x = original_density$x,
    Original = original_density$y,
    Global = stats::approx(global_density$x, global_density$y, xout = original_density$x, rule = 2)$y,
    Variablewise = stats::approx(variablewise_density$x, variablewise_density$y, xout = original_density$x, rule = 2)$y
  )

  density_df <- density_df %>%
    dplyr::mutate(
      Global_Diff = Original - ifelse(is.na(Global), 0, Global),
      Variablewise_Diff = Original - ifelse(is.na(Variablewise), 0, Variablewise)
    )

  ggplot2::ggplot(density_df) +
    ggplot2::geom_line(
      ggplot2::aes(x = x, y = Global_Diff, color = "Global Difference"),
      linewidth = 1.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = x, y = Variablewise_Diff, color = "Variable-wise Difference"),
      linewidth = 1.5
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = paste(tools::toTitleCase(gsub("_", " ", variable)), "Values"),
      y = "Density Difference",
      color = NULL,
      caption = if (include_footer) footer else NULL
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Global Difference" = viridis::viridis(3)[2],
        "Variable-wise Difference" = viridis::viridis(3)[3]
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, face = "italic"),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 8),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 8, face = "italic"),
      plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "lines")
    )
}
