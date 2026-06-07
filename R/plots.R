# Standardised-rate plotting helpers
# Helper functions split from the former R/utils.R
# =============================================================================

plot_std_rate <- function(data, tt = "", y_lim = NULL, ...) {
  # Detect which standardized rate is in the data
  y_var <- if ("std_mort_rate" %in% names(data)) "std_mort_rate" else "std_fert_rate"
  y_label <- if (y_var == "std_mort_rate") "Standardised mortality rate" else "Standardised fertility rate"
  
  # Dynamically get y-range if not supplied
  y_ran <- range(data[[y_var]], na.rm = TRUE)
  
  ggplot(data = data) +
    geom_point(aes(x = mpi, y = .data[[y_var]], color = capital, size = capital)) +
    scale_color_manual(
      name = "", 
      values = c("#FF000099", "#0000FF99"), 
      labels = c("Non-capital", "Department capital")
    ) +
    scale_size_manual(
      name = "", 
      values = c(1, 3), 
      labels = c("Non-capital", "Department capital")
    ) +
    geom_smooth(
      data = data[data$capital == 1, ], 
      aes(x = mpi, y = .data[[y_var]]), 
      method = "lm", 
      formula = y ~ x, 
      se = TRUE, 
      color = "blue", 
      linewidth = 0.5
    ) +
    geom_smooth(
      data = data[data$capital == 0, ], 
      aes(x = mpi, y = .data[[y_var]]), 
      method = "lm", 
      formula = y ~ x, 
      se = TRUE, 
      color = "red", 
      linewidth = 0.5
    ) +
    labs(
      title = tt,
      x = "MPI",
      y = y_label
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 12)
    )
}

#-------------------------------------------------------------------------------
