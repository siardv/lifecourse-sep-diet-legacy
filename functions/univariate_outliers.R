univariate_outliers <- function(data, variable, title = NULL) {
  outlier_color <- "red"
  data_color <- viridis::viridis(10)[4]

  # Remove missing values and non-finite values
  data_filtered <- data[!is.na(data[[variable]]) &
    !is.infinite(data[[variable]]), ]

  # Calculate IQR and outlier thresholds
  Q1 <- stats::quantile(data_filtered[[variable]], 0.25)
  Q3 <- stats::quantile(data_filtered[[variable]], 0.75)
  IQR <- Q3 - Q1
  upper_threshold <- Q3 + 1.5 * IQR
  lower_threshold <- Q1 - 1.5 * IQR

  # Identify outliers
  outliers <- data_filtered[[variable]][data_filtered[[variable]] > upper_threshold |
    data_filtered[[variable]] < lower_threshold]
  # Return the outliers' indices
  outliers_indices <- which(data_filtered[[variable]] > upper_threshold |
    data_filtered[[variable]] < lower_threshold)

  # Calculate mean and confidence intervals
  mean_val <- mean(data_filtered[[variable]])
  stderr <- sd(data_filtered[[variable]]) / sqrt(length(data_filtered[[variable]]))
  ci_lower <- mean_val - 1.96 * stderr
  ci_upper <- mean_val + 1.96 * stderr

  # Handle non-finite values in mean and confidence intervals
  if (is.infinite(mean_val) || is.na(mean_val) ||
    is.infinite(ci_lower) || is.na(ci_lower) ||
    is.infinite(ci_upper) || is.na(ci_upper)) {
    warning(
      "Non-finite values encountered in mean or confidence intervals. Plot may be inaccurate."
    )
    return(NULL) # Or handle the situation as needed
  }

  # Create a data frame for the mean and confidence interval
  mean_data <- data.frame(
    x = 1,
    y = mean_val,
    ymin = ci_lower,
    ymax = ci_upper
  )

  # Calculate the median
  median_val <- median(data_filtered[[variable]])

  # Create boxplot with outliers highlighted and confidence intervals
  plot <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = factor(1), y = .data[[variable]])) +
    ggplot2::geom_boxplot(
      fill = "lightgrey",
      alpha = 0.5,
      outlier.shape = "."
    ) +
    ggplot2::geom_point(
      data = data_filtered[data_filtered[[variable]] %in% outliers, ],
      ggplot2::aes(x = factor(1), y = .data[[variable]]),
      color = "red",
      size = 1
    ) +
    ggplot2::geom_jitter(
      data = data_filtered[!(data_filtered[[variable]] %in% outliers), ],
      width = 0.3,
      height = 0.25,
      alpha = 0.25,
      color = data_color
    ) +
    ggplot2::geom_errorbarh(
      data = mean_data,
      ggplot2::aes(xmin = 0.8, xmax = 1.2, y = y),
      height = 0,
      color = "blue",
      size = 1.2
    ) +
    ggplot2::geom_point(
      data = mean_data,
      ggplot2::aes(x = x, y = y),
      color = "blue",
      size = 2,
      shape = 18
    ) +
    ggplot2::labs(
      title = beautify_names(title),
      x = NULL,
      y = beautify_names(variable)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 16, face = "bold", family = "Cambria"),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold", family = "Cambria"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12, family = "Cambria")
    ) +
    ggplot2::scale_color_manual(
      name = "Legend",
      values = c(outlier_color, "blue"),
      labels = c("Outliers", "Confidence Interval")
    )

  return(list(
    outliers = outliers,
    outliers_indices = outliers_indices,
    plot = plot
  ))
}
