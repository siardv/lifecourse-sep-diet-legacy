johnson_neyman_plot <- function(model_fit, pred_var, modx_var, print_insignificant = FALSE) {
  # Check if the predictor and moderator variables are in the model's data
  model_fit$model <- numeric_data_frame(model_fit$model)
  model_data <- model.frame(model_fit)
  model_formula <- create_formula(model_fit)

  if (!(pred_var %in% names(model_data))) {
    stop(paste("Predictor variable", pred_var, "not found in the model data."))
  }
  if (!(modx_var %in% names(model_data))) {
    stop(paste("Moderator variable", modx_var, "not found in the model data."))
  }

  if (!is_continuous(model_data, pred_var, modx_var)) {
    message("Both predictor and moderator variables must be categorical.")
    return(invisible())
  }

  # Perform Johnson-Neyman analysis with FDR control
  jn_results <- interactions::johnson_neyman(
    model = model_fit,
    pred = !!rlang::sym(pred_var),
    modx = !!rlang::sym(modx_var),
    control.fdr = TRUE
  )

  # Extract the necessary data for plotting
  jn_data <- jn_results$cbands %>%
    dplyr::rename_with(~"slope", dplyr::starts_with("Slope of ")) %>%
    dplyr::rename(
      modx = !!rlang::sym(modx_var),
      conf.low = Lower,
      conf.high = Upper,
      significance = Significance
    )
  if (!print_insignificant & !any(jn_data$significance == "Significant")) {
    message("No significant Johnson-Neyman regions found.")
    return(invisible())
  }

  pred_lab <- beautify_names(pred_var)
  out_lab <- all.vars(model_formula)[1]
  out_lab <- beautify_names(out_lab)
  mod_lab <- beautify_names(modx_var)

  boldify <- function(x) {
    paste0("\\bf{", x, "}")
  }

  plot_title <- sprintf(
    "Effect of %s on %s, Moderated by %s",
    boldify(pred_lab),
    boldify(out_lab),
    boldify(mod_lab)
  )

  # Get the range of observed data
  modx_range <- extract_term_ranges(model_fit, colnames = modx_var)
  modx_min <- modx_range$min
  modx_max <- modx_range$max
  modx_mean <- mean(c(modx_min, modx_max))

  # Define color settings using viridis
  # line_color <- viridis::viridis(4)[2]
  # fill_colors <- c("Insignificant" = viridis::viridis(8)[5], "Significant" = viridis::viridis(8)[7])
  line_color <- "#377eb8"
  fill_colors <- c("Insignificant" = "#fbb4ae", "Significant" = "#b3cde3")
  change_points <- jn_data$modx[which(diff(jn_data$significance != "Insignificant") != 0)]

  # Determine the maximum y-value for positioning the label at the top
  max_y <- max(jn_data$slope, jn_data$conf.high, na.rm = TRUE)

  # Create the plot using ggplot2
  jn_plot <- ggplot2::ggplot(jn_data, ggplot2::aes(x = modx, y = slope)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = significance),
      alpha = 0.2
    ) +
    ggplot2::geom_ribbon(
      data = jn_data %>% dplyr::filter(modx >= modx_min & modx <= modx_max),
      ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = significance), alpha = 0.6
    ) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "solid",
      color = "black", size = 0
    ) +
    ggplot2::geom_ribbon(
      data = data.frame(modx = c(modx_min, modx_max), slope = c(-Inf, Inf)),
      ggplot2::aes(x = modx, ymin = -Inf, ymax = Inf),
      fill = "grey", alpha = 0.1
    ) +
    ggplot2::geom_line(ggplot2::aes(color = significance), size = 1) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::scale_color_manual(values = fill_colors) +
    ggplot2::ggtitle(latex2exp::TeX(plot_title),
      subtitle = "Johnson-Neyman Regions with FDR Control"
    ) +
    ggplot2::labs(
      x = mod_lab,
      y = paste("Estimated Effect of", pred_lab, "on", out_lab),
      fill = NULL,
      color = NULL,
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11, face = "italic"),
      legend.position = "right",
      legend.direction = "vertical",
      aspect.ratio = 1 # Maintain a 1:1 aspect ratio
    ) +
    ggplot2::annotate("text",
      x = modx_mean,
      y = max_y, # Position label slightly above the maximum y-value
      label = "Range of observed data", angle = 0,
      hjust = 0.5, size = 4,
      fontface = "italic", color = "darkgrey"
    ) +
    ggplot2::geom_vline(
      xintercept = change_points,
      linetype = "dashed",
      color = "darkgrey", size = 0.3
    )

  return(jn_plot)
}

# johnson_neyman_plot(lmrob_fits$model_8, "sep", "sep_mother")
