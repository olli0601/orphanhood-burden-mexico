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
# Joins unit mean delay to a deprivation index file and adds, within each year,
# the standardised deprivation (-IMN), the 0-100 score and the deprivation decile.
# Years past the index's last year carry its last value forward.
with_depriv <- function(unit_delay, file, label, years) {
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

# Mean delay by deprivation decile over every occurrence year (fixed-weight index,
# 2020 values carried forward). An occurrence year y has only (last_reg - y) years
# of registrations, so recent years are shaded: their long delays are not yet seen.
plot_delay_decile_years <- function(unit_delay, what, years = 1990:2023, last_reg = 2024,
                                    min_followup = 5) {
  d <- with_depriv(unit_delay, "index_grouped_mun_fw.RDS", "Fixed-weight", years) |>
    dplyr::group_by(year, decile) |>
    dplyr::summarise(delay_days = mean(delay_days), .groups = "drop") |>
    dplyr::mutate(decile = factor(as.integer(decile), labels = c("1 (least)", 2:9, "10 (most)")))
  ends <- dplyr::filter(d, year == max(year), decile %in% c("1 (least)", "10 (most)"))
  ggplot2::ggplot(d, ggplot2::aes(year, delay_days, colour = decile, group = decile)) +
    ggplot2::annotate("rect", xmin = last_reg - min_followup + 0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = "grey92") +
    ggplot2::annotate("text", x = last_reg - min_followup + 0.7, y = Inf, vjust = 1.5, hjust = 0, size = 3,
                      colour = "grey35", label = sprintf("< %d years of\nregistrations", min_followup)) +
    ggplot2::geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey55", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.6) + ggplot2::geom_point(size = 1) +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = decile), hjust = -0.15, size = 3,
                       show.legend = FALSE) +
    ggplot2::scale_colour_viridis_d(option = "magma", begin = 0.15, end = 0.8, direction = -1) +
    ggplot2::scale_x_continuous(breaks = seq(1990, max(years), 5), minor_breaks = years,
                                expand = ggplot2::expansion(add = c(0.5, 3))) +
    ggplot2::labs(title = "Mean registration delay by deprivation decile, every year",
                  subtitle = paste("Average over the units in each decile (within year) of their mean delay",
                                   sprintf("(days from %s to registration; delay recorded in whole years).", what),
                                   "Fixed-weight index; 2021+ uses each unit's 2020 value. Dashed line: 2020.",
                                   sep = "\n"),
                  x = paste("Year of", what), y = "Mean registration delay (days)",
                  colour = "Deprivation decile") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))
}

# Cumulative share registered within d = 0..max_d years of occurrence, by
# deprivation decile (fixed-weight index, within year; 2021+ carries 2020) and
# occurrence year. Events are pooled over the units of a decile. The denominator
# is every registration up to `last_reg`, so years with < min_followup years of
# registrations overstate completeness. `df` holds group_id, year, year_reg and
# `count`. Returns year, decile, d, share.
decile_cumulative <- function(df, count, years, max_d) {
  cells <- df |>
    dplyr::filter(!is.na(group_id)) |>
    add_delay(bucket = FALSE) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::filter(year %in% years) |>
    dplyr::group_by(group_id, year, delay) |>
    dplyr::summarise(n = sum(.data[[count]]), .groups = "drop")
  deciles <- with_depriv(dplyr::distinct(cells, group_id, year), "index_grouped_mun_fw.RDS",
                         "Fixed-weight", years) |>
    dplyr::select(group_id, year, decile)
  cells |>
    dplyr::inner_join(deciles, by = c("group_id", "year")) |>
    dplyr::group_by(year, decile) |>
    dplyr::reframe(d = 0:max_d, share = sapply(0:max_d, \(k) sum(n[delay <= k]) / sum(n))) |>
    dplyr::mutate(decile = factor(as.integer(decile), labels = c("1 (least)", 2:9, "10 (most)")))
}

# Facets d = 0..max_d of decile_cumulative(); d = 0 is "registered in the same
# calendar year". Shaded: occurrence years with < min_followup years of registrations.
plot_cumulative_by_decile <- function(df, count, what, years = 1990:2023, max_d = 7,
                                      last_reg = 2024, min_followup = 10) {
  d <- decile_cumulative(df, count, years, max_d) |>
    dplyr::mutate(d = factor(d, labels = c("Same calendar year", "By the next year",
                                           sprintf("Within %d years", seq_len(max_d - 1) + 1))))
  ggplot2::ggplot(d, ggplot2::aes(year, share, colour = decile, group = decile)) +
    ggplot2::annotate("rect", xmin = last_reg - min_followup + 0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = "grey92") +
    ggplot2::geom_hline(yintercept = c(0.90, 0.95, 0.98), linetype = "dotted", colour = "grey40",
                        linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.5) + ggplot2::geom_point(size = 0.6) +
    ggplot2::facet_wrap(~d, ncol = 4) +
    ggplot2::scale_colour_viridis_d(option = "magma", begin = 0.15, end = 0.8, direction = -1) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1, drop0trailing = TRUE)) +
    ggplot2::scale_x_continuous(breaks = seq(1990, max(years), 10)) +
    ggplot2::labs(title = sprintf("Share of %ss registered within d years, by deprivation decile", what),
                  subtitle = paste(sprintf("Share of %ss registered by %d that were registered within d calendar years of occurrence.",
                                           what, last_reg),
                                   "Events pooled over the units in each decile (within year). Dotted lines: 90%, 95%, 98%.",
                                   sprintf("Shaded: < %d years of registrations, so the denominator is still growing and shares are overstated.",
                                           min_followup),
                                   "Fixed-weight index; 2021+ uses each unit's 2020 value.", sep = "\n"),
                  x = paste("Year of", what), y = "Cumulative share registered",
                  colour = "Deprivation decile") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))
}

# Years until 90/95/98% of a decile's events are registered: the smallest whole
# delay d whose cumulative share (decile_cumulative()) reaches the threshold. Delays
# are whole calendar years, so the quantile is an integer; lines are offset
# vertically by decile so ties stay visible. An occurrence year y cannot show a
# quantile above last_reg - y, hence the shading.
plot_quantile_by_decile <- function(df, count, what, years = 1990:2023, probs = c(0.90, 0.95, 0.98),
                                    last_reg = 2024, min_followup = 10, offset = 0.04) {
  d <- decile_cumulative(df, count, years, max_d = last_reg - min(years)) |>
    tidyr::crossing(p = probs) |>
    dplyr::filter(share >= p - 1e-9) |>
    dplyr::group_by(year, decile, p) |>
    dplyr::summarise(q = min(d), .groups = "drop") |>
    dplyr::mutate(q_plot = q + (as.integer(decile) - 5.5) * offset,
                  p = factor(p, labels = sprintf("%g%% registered", 100 * probs)))
  ggplot2::ggplot(d, ggplot2::aes(year, q_plot, colour = decile, group = decile)) +
    ggplot2::annotate("rect", xmin = last_reg - min_followup + 0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = "grey92") +
    ggplot2::geom_step(direction = "mid", linewidth = 0.5) +
    ggplot2::facet_wrap(~p, nrow = 1) +
    ggplot2::scale_colour_viridis_d(option = "magma", begin = 0.15, end = 0.8, direction = -1) +
    ggplot2::scale_y_continuous(breaks = 0:max(d$q), minor_breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(1990, max(years), 10)) +
    ggplot2::labs(title = sprintf("Years until 90/95/98%% of %ss are registered, by deprivation decile", what),
                  subtitle = paste(sprintf("Smallest whole delay (calendar years after the %s) by which the share is reached; 0 = same calendar year.",
                                           what),
                                   sprintf("Denominator: all registrations up to %d. Events pooled over the units in each decile (within year).",
                                           last_reg),
                                   "Lines offset slightly by decile so equal values stay visible.",
                                   sprintf("Shaded: < %d years of registrations, so quantiles are understated. Fixed-weight index; 2021+ uses 2020 values.",
                                           min_followup), sep = "\n"),
                  x = paste("Year of", what), y = "Years until registered",
                  colour = "Deprivation decile") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))
}

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
  dep <- dplyr::bind_rows(with_depriv(unit_delay, "index_grouped_mun_fw.RDS", "Fixed-weight", years),
                          with_depriv(unit_delay, "index_grouped_mun.RDS", "CONAPO (2010 carried back)", years))
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
