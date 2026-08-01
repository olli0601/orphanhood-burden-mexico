# Standardised-rate plotting helpers
# Helper functions split from the former R/utils.R
# =============================================================================

# `y_lim` used to be accepted and then silently ignored: the function computed a
# `y_ran` it never referenced and built the plot with no scale_y_continuous at
# all. Three call sites passed y_lim = c(0, 0.15) believing the fertility axes
# were clipped, and none of them were. It is now honoured.
#
# `capital` must be a factor with levels "0"/"1" for the colour and size scales
# to line up with their labels; a municipality missing from the geo lookup
# yields NA and silently shifts them.
plot_std_rate <- function(data, tt = "", y_lim = NULL, ...) {
  # Detect which standardized rate is in the data
  y_var <- if ("std_mort_rate" %in% names(data)) "std_mort_rate" else "std_fert_rate"
  y_label <- if (y_var == "std_mort_rate") "Standardised mortality rate" else "Standardised fertility rate"

  p <- ggplot(data = data) +
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

  # coord_cartesian, not ylim(): ylim() drops out-of-range points before the
  # geom_smooth fits, which would silently change the regression lines rather
  # than just the visible window.
  if (!is.null(y_lim)) p <- p + coord_cartesian(ylim = y_lim)
  p
}

#-------------------------------------------------------------------------------
