# Registration-delay helpers: the gap between the year an event occurred
# (`year`) and the year it was registered (`year_reg`).
# =============================================================================

# Delay in years, optionally bucketed into a factor 0, 1, ..., max_delay, "max_delay+".
add_delay <- function(df, max_delay = 5, bucket = TRUE) {
  df <- dplyr::mutate(df, delay = as.numeric(year_reg) - as.numeric(year))
  if (bucket) {
    df <- dplyr::mutate(df,
      delay = dplyr::if_else(delay > max_delay, paste0(max_delay, "+"), as.character(delay)),
      delay = factor(delay, levels = c(as.character(0:max_delay), paste0(max_delay, "+"))))
  }
  df
}

# One row per occurrence year x registration year with the bucketed delay,
# `total`, and its share within each occurrence year (`prop_occ`) and each
# registration year (`prop_reg`).
# `count` is the event column ("deaths", "births"), summed. Panel rows are not
# events (cells range from 1 to thousands), so there is no row-count mode.
delay_summary <- function(df, count, max_delay = 5) {
  df |>
    dplyr::group_by(year, year_reg) |>
    dplyr::summarise(total = sum(.data[[count]]), .groups = "drop") |>
    add_delay(max_delay = max_delay) |>
    dplyr::mutate(prop_occ = total / sum(total), .by = year) |>
    dplyr::mutate(prop_reg = total / sum(total), .by = year_reg)
}

# Fixed colours per delay level. createPalette() is stochastic, hence the seed.
delay_palette <- function(delay) {
  lv <- levels(factor(delay))
  set.seed(20250815)
  pal <- Polychrome::createPalette(length(lv), seedcolors = c("#FF0000", "#0000FF", "#00FF00"))
  stats::setNames(pal, lv)
}

# Stacked bars of column `col_y` by year column `col_x`, filled by delay level.
#   limits  : fixed x categories (e.g. 1990:2023), or NULL
#   percent : percentage y axis
#   grid    : "thin"/"plain" add vertical x labels and a grey y grid (0.3 / default
#             width); "none" leaves theme_minimal() alone
#   sqrt_y  : shares only. Square-root y axis (applied after stacking, so band
#             heights stay true shares) with gridlines at 1-10%, to show the
#             long-delay bands at the bottom of the stack; bars get gaps.
# Remaining arguments go to labs().
plot_delay_bars <- function(d, col_x, col_y, pal, ..., limits = NULL, percent = FALSE,
                            grid = c("thin", "plain", "none"), sqrt_y = FALSE) {
  grid <- match.arg(grid)
  # One bar segment per delay level ("5+" spans several registration years).
  d <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(col_x, "delay")))) |>
    dplyr::summarise("{col_y}" := sum(.data[[col_y]]), .groups = "drop")
  p <- ggplot2::ggplot(d,ggplot2::aes(x = factor(.data[[col_x]]), y = .data[[col_y]],
                                       fill = factor(delay))) +
    ggplot2::geom_bar(stat = "identity", width = if (sqrt_y) 0.7 else 0.9)
  if (!is.null(limits)) p <- p + ggplot2::scale_x_discrete(limits = as.character(limits))
  if (sqrt_y) {
    p <- p + ggplot2::scale_y_continuous(
      breaks = c(0, seq(0.01, 0.10, 0.01), 0.25, 0.5, 0.75, 1),
      labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
      ggplot2::coord_trans(y = "sqrt", ylim = c(0, 1))
  } else if (percent) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  p <- p + ggplot2::scale_fill_manual(values = pal) + ggplot2::labs(...) + ggplot2::theme_minimal()
  if (sqrt_y) p <- p + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  if (grid == "none") return(p)
  line <- if (grid == "thin") ggplot2::element_line(color = "gray80", size = 0.3)
          else ggplot2::element_line(color = "gray80")
  p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                     panel.grid.major.y = line)
}

# --- Delay vs deprivation, per year -----------------------------------------------
# Mean registration delay per unit-year, weighted by events, in DAYS (delay is
# recorded in whole years; x 365 for readability, not precision). `df` holds
# group_id, year, year_reg and the event column `count`; one row per cell.
unit_mean_delay <- function(df, count, years = 1990:2020) {
  df |>
    dplyr::filter(!is.na(group_id)) |>
    add_delay(bucket = FALSE) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::filter(year %in% years) |>
    dplyr::group_by(group_id, year) |>
    dplyr::summarise(delay_days = 365 * stats::weighted.mean(delay, .data[[count]]),
                     .groups = "drop")
}

# Regresses unit mean delay on deprivation standardised within year (-IMN:
# higher = more deprived), one OLS per year over units, for the fixed-weight
# index and for CONAPO. CONAPO exists for 2010-2020 only; before 2010 each unit
# takes its 2010 value (extend_marg_years(), as in the thesis). Reads both
# index files regardless of ORPHAN_MARG. Returns list(dep, slopes, plot,
# slopes_point, plot_point): `slopes` in days of mean delay per SD of
# deprivation (both indices); `slopes_point` per index point of the fixed-weight
# index (0-100, = 100 * (1 - IMN)), which is on one scale across years. The SD
# of that index shrinks from ~15 (1990) to ~6 (2020), so per-SD slopes fall
# over time even at a constant per-point slope.
delay_vs_deprivation <- function(unit_delay, what, years = 1990:2020) {
  with_depriv <- function(file, label) {
    readRDS(file.path("input-data-processed", file)) |>
      extend_marg_years(years) |>
      dplyr::filter(year %in% years) |>
      dplyr::inner_join(unit_delay, by = c("group_id", "year")) |>
      dplyr::group_by(year) |>
      dplyr::mutate(score = 100 * (1 - IMN),
                    depriv = -as.numeric(scale(IMN)),
                    decile = factor(dplyr::ntile(depriv, 10),
                                    labels = c("1\nleast", 2:9, "10\nmost"))) |>
      dplyr::ungroup() |>
      dplyr::mutate(index = label)
  }
  dep <- dplyr::bind_rows(with_depriv("index_grouped_mun_fw.RDS", "Fixed-weight"),
                          with_depriv("index_grouped_mun.RDS", "CONAPO (2010 carried back)"))
  slopes <- dep |>
    dplyr::group_by(index, year) |>
    dplyr::group_modify(~ {
      m <- summary(stats::lm(delay_days ~ depriv, .x))$coefficients
      tibble::tibble(est = m[2, 1], se = m[2, 2], p = m[2, 4], n_units = nrow(.x))
    }) |>
    dplyr::ungroup()

  index_cols <- c("Fixed-weight" = "#1b7837", "CONAPO (2010 carried back)" = "#762a83")
  p_slopes <- ggplot2::ggplot(slopes, ggplot2::aes(year, est, colour = index)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                             position = ggplot2::position_dodge(width = 0.5), size = 0.25) +
    ggplot2::geom_smooth(ggplot2::aes(weight = 1 / se^2), method = "lm", formula = y ~ x,
                         se = FALSE, linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = index_cols) +
    ggplot2::labs(title = "A. Extra delay per SD of deprivation",
                  subtitle = paste("Each point: by how much a unit's mean delay rises when its deprivation is 1 SD higher",
                                   "(OLS across units, 95% CI). Lines: linear trend over years.", sep = "\n"),
                  x = paste("Year of", what), y = "Extra mean delay (days) per SD of deprivation",
                  colour = "Index") +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom")
  p_decile <- dep |>
    dplyr::filter(index == "Fixed-weight", year %in% c(1990, 2000, 2010, 2020)) |>
    dplyr::group_by(year, decile) |>
    dplyr::summarise(delay_days = mean(delay_days), .groups = "drop") |>
    ggplot2::ggplot(ggplot2::aes(decile, delay_days, colour = factor(year), group = factor(year))) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::scale_colour_viridis_d(end = 0.85) +
    ggplot2::labs(title = "B. Mean delay by deprivation decile",
                  subtitle = paste("Average over the units in each decile of their mean delay",
                                   sprintf("(days from %s to registration). Fixed-weight index.", what), sep = "\n"),
                  x = "Deprivation decile (within year)", y = "Mean registration delay (days)",
                  colour = "Year") +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom")
  slopes_point <- dep |>
    dplyr::filter(index == "Fixed-weight") |>
    dplyr::group_by(year) |>
    dplyr::group_modify(~ {
      m <- summary(stats::lm(delay_days ~ score, .x))$coefficients
      tibble::tibble(est = m[2, 1], se = m[2, 2], p = m[2, 4], sd_score = stats::sd(.x$score),
                     n_units = nrow(.x))
    }) |>
    dplyr::ungroup()
  p_point <- ggplot2::ggplot(slopes_point, ggplot2::aes(year, est)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                             colour = index_cols[["Fixed-weight"]], size = 0.25) +
    ggplot2::geom_smooth(ggplot2::aes(weight = 1 / se^2), method = "lm", formula = y ~ x,
                         se = FALSE, linewidth = 0.8, colour = index_cols[["Fixed-weight"]]) +
    ggplot2::labs(title = "Extra delay per point of the fixed-weight deprivation index",
                  subtitle = paste("Each point: by how much a unit's mean delay rises when its index is 1 point higher",
                                   "(index 0-100, same scale every year; OLS across units, 95% CI). Line: linear trend.",
                                   sep = "\n"),
                  x = paste("Year of", what), y = "Extra mean delay (days) per index point") +
    ggplot2::theme_minimal(base_size = 11)
  list(dep = dep, slopes = slopes,
       plot = gridExtra::arrangeGrob(p_slopes, p_decile, ncol = 2, widths = c(1.4, 1)),
       slopes_point = slopes_point, plot_point = p_point)
}
