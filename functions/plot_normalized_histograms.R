plot_normalized_histograms <- function(y, x, df, labels = NULL, offset = 0.005, footnote = NULL,
                                       hide_title = TRUE, tail_str = "") {
  if (length(labels) == 2) {
    x_label <- labels[1]
    y_label <- labels[2]
  } else {
    x_label <- deparse(substitute(x))
    y_label <- deparse(substitute(y))
  }

  data_processed <- df %>%
    dplyr::select({{ x }}, {{ y }}) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::mutate(dplyr::across(c({{ x }}, {{ y }}), as.numeric)) %>%
    dplyr::mutate(
      x_norm = ({{ x }} - min({{ x }}, na.rm = TRUE)) /
        (max({{ x }}, na.rm = TRUE) - min({{ x }}, na.rm = TRUE)),
      y_norm = ({{ y }} - min({{ y }}, na.rm = TRUE)) /
        (max({{ y }}, na.rm = TRUE) - min({{ y }}, na.rm = TRUE))
    )

  # get range of the data
  min_val <- min(min(data_processed$x_norm), min(data_processed$y_norm)) - offset
  max_val <- max(max(data_processed$x_norm), max(data_processed$y_norm)) + offset

  # breaks that span the entire range
  breaks <- seq(min_val, max_val, length.out = 21)

  graphics::hist(data_processed$x_norm,
    breaks = breaks,
    col = rgb(1, 0, 0, 0.5),
    main = ifelse(hide_title, "", paste("Overlay of Normalized", x_label, "and", y_label, tail_str)),
    xlab = "Normalized Value",
    ylab = "Density",
    xlim = c(min_val, max_val),
    probability = TRUE,
    xaxt = "n" # no x-axis labels
  )

  # slight offset second histogram
  graphics::hist(data_processed$y_norm + offset,
    breaks = breaks + offset,
    col = grDevices::rgb(0, 0, 1, 0.5),
    add = TRUE,
    probability = TRUE
  )
  graphics::axis(1, at = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1))

  # legend
  graphics::legend("topright",
    legend = c(x_label, y_label),
    fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))
  )
}