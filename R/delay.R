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
# Remaining arguments go to labs().
plot_delay_bars <- function(d, col_x, col_y, pal, ..., limits = NULL, percent = FALSE,
                            grid = c("thin", "plain", "none")) {
  grid <- match.arg(grid)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = factor(.data[[col_x]]), y = .data[[col_y]],
                                       fill = factor(delay))) +
    ggplot2::geom_bar(stat = "identity")
  if (!is.null(limits)) p <- p + ggplot2::scale_x_discrete(limits = as.character(limits))
  if (percent) p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  p <- p + ggplot2::scale_fill_manual(values = pal) + ggplot2::labs(...) + ggplot2::theme_minimal()
  if (grid == "none") return(p)
  line <- if (grid == "thin") ggplot2::element_line(color = "gray80", size = 0.3)
          else ggplot2::element_line(color = "gray80")
  p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                     panel.grid.major.y = line)
}
