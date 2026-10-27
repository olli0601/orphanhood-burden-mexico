# Plot helpers shared by the chapter 2-3 scripts.
# =============================================================================

# Standardised rate (std_mort_rate or std_fert_rate) against marginalization
# (`mpi`), coloured by state capital, with a linear fit per group. `y_label`
# overrides the default axis title.
# `capital` must be a factor with levels "0"/"1" for the colour and size scales
# to line up with their labels. `y_lim` zooms with coord_cartesian(), which,
# unlike ylim(), does not drop points before the fits.
plot_std_rate <- function(data, tt = "", y_lim = NULL, y_label = NULL, ...) {
  y_var <- if ("std_mort_rate" %in% names(data)) "std_mort_rate" else "std_fert_rate"
  y_label <- y_label %||%
    if (y_var == "std_mort_rate") "Standardised mortality rate" else "Standardised fertility rate"
  labels <- c("Non-capital", "Department capital")
  fit <- function(rows, colour) {
    geom_smooth(data = data[rows, ], aes(x = mpi, y = .data[[y_var]]), method = "lm",
                formula = y ~ x, se = TRUE, color = colour, linewidth = 0.5)
  }

  p <- ggplot(data = data) +
    geom_point(aes(x = mpi, y = .data[[y_var]], color = capital, size = capital)) +
    scale_color_manual(name = "", values = c("#FF000099", "#0000FF99"), labels = labels) +
    scale_size_manual(name = "", values = c(1, 3), labels = labels) +
    fit(data$capital == 1, "blue") +
    fit(data$capital == 0, "red") +
    labs(title = tt, x = "MPI", y = y_label) +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 12))
  if (!is.null(y_lim)) p <- p + coord_cartesian(ylim = y_lim)
  p
}

# Save to the calling script's `fig_dir`.
save_fig <- function(p, name, width = 8, height = 6, dir = fig_dir) {
  ggplot2::ggsave(file.path(dir, name), p, width = width, height = height)
}

# Age profile of a standardised rate by age band (output of
# compute_*std_rate_age_gender()). `what` is "Mortality" or "Fertility".
#   by = "year": one panel per year, one line per sex
#   by = "sex" : one panel per sex, one line per year
plot_age_profile <- function(d, what, by = c("year", "sex"), title = NULL) {
  by <- match.arg(by)
  ylab <- sprintf("Standardized %s Rate", what)
  if (by == "year") {
    ggplot2::ggplot(d, ggplot2::aes(x = age, y = std_rate, color = sex, group = sex)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ year) +
      ggplot2::labs(title = title %||% sprintf("%s by Age and Sex", ylab), x = "Age Group", y = ylab) +
      ggplot2::theme_minimal() +
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                     legend.title = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     axis.title.x = ggplot2::element_text(size = 8))
  } else {
    ggplot2::ggplot(d, ggplot2::aes(x = age, y = std_rate, color = as.factor(year),
                                    group = as.factor(year))) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ sex, nrow = 1) +
      ggplot2::labs(title = title %||% sprintf("%s by Age Group", ylab), x = "Age Group", y = ylab) +
      ggplot2::theme_minimal()
  }
}

# Mean of `value` per year x sex x marginalization quintile, as lines.
plot_quintile_trend <- function(d, value, ylab, title) {
  d |>
    dplyr::filter(!is.na(poverty_quintile)) |>
    dplyr::group_by(year, sex, poverty_quintile) |>
    dplyr::summarise(value = mean(.data[[value]], na.rm = TRUE), .groups = "drop") |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = as.factor(poverty_quintile),
                                 group = poverty_quintile)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ sex) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, x = "Year", y = ylab, color = "Poverty Quintile")
}
