# Function to plot difficulty distributions
plot_difficulty_distributions <- function(df, columns, labels) {
  column_indices <- which(names(df) %in% columns)
  selected_data <- df[, column_indices, drop = FALSE]

  # Rename columns for the plot
  if (!missing(labels)) {
    names(selected_data) <- labels
    names(df)[column_indices] <- names(selected_data)
  }

  # Convert to long format for ggplot
  long_format_data <- reshape2::melt(df, measure.vars = names(selected_data))

  # Create the density plot
  difficulty_plot <- ggplot2::ggplot(long_format_data, ggplot2::aes(x = value, fill = variable)) +
    ggplot2::geom_density(alpha = 0.5, adjust = 1.5) +
    ggplot2::scale_x_continuous(breaks = seq(min(long_format_data$value),
      max(long_format_data$value),
      by = 1
    )) +
    ggplot2::scale_fill_manual(
      values = c("blue", "red", "green"),
      labels = latex2exp::TeX(c(
        "Living from Income",
        "Paying Regular Bills",
        "Financial Difficulty [scale]"
      ))
    ) +
    ggplot2::labs(
      x = latex2exp::TeX("Difficulty Level"),
      y = latex2exp::TeX("Density"),
      title = latex2exp::TeX("Comparison of Difficulty Distributions")
    ) +
    ggplot2::theme_minimal(base_size = 14, base_family = "serif") +
    ggplot2::theme(
      legend.title =ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 12, hjust = 0.5),
      plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 20)),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.background = ggplot2::element_rect(fill = "white", colour = "black"),
      legend.direction = "horizontal",
      legend.key.size = grid::unit(0.25, "cm")
    )

  return(difficulty_plot)
}
