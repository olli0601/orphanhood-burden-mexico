# =============================================================================
# ch3_050_delay_analysis_births_and_deaths.R  ·  Chapter 3 — Birth and death delays vs deprivation
# For deaths and births on the grouped municipalities: registration-delay
# shares (thesis Figs 4, 6), delay by sex, age and marginalization quintile,
# cumulative registration by cohort (Figs 5, 7), registration completeness, and
# unit mean delay by GM and area type (Figs 8-11). The mortality panels hold
# parental ages only, so every death figure is about adult deaths.
#
# Reads : input-data-processed/{deaths, births, grouped_municipality_50000,
#         index_grouped_mun[_fw], rural_urban_area}.RDS
# Writes: output/ch3[_fw]/ch3_050_*.{pdf,csv}
# Run after: ch2_060 (ch2_061 for ORPHAN_MARG=fw)
# =============================================================================

source("R/setup.R")
fig_dir <- marg_fig_dir("output/ch3")

marg_index <- readRDS(marg_file("index_grouped_mun")) |>
  add_marg_quintile() |>
  select(-c(GM, marginalization_quintile))

deaths_grouped_mun <- aggregate_to_grouped_mun(readRDS("input-data-processed/deaths.RDS"), "deaths")
births_grouped_mun <- aggregate_to_grouped_mun(readRDS("input-data-processed/births.RDS"), "births")

# --- Delay shares and delay by sex, age and marginalization ----------------------
# `value` is the event column; shares are computed on `share_rows` (mother rows
# for births: every birth is on both a mother's and a father's row).
# `reg_word` is the capitalisation used in the by-registration title.
delay_figures <- function(grouped, value, reg_word, share_rows = grouped, point_size = FALSE) {
  force(share_rows)
  fig <- function(name) sprintf("ch3_050_%s_%s.pdf", value, name)
  grouped <- add_delay(grouped, bucket = FALSE)

  delays <- delay_summary(share_rows, count = value)
  pal <- delay_palette(delays$delay)
  save_fig(plot_delay_bars(delays, "year", "prop_occ", pal, limits = 1990:2023, percent = TRUE,
                           x = "Year of occurrence", y = paste("Proportion of", value), fill = "Delay (year)",
                           title = sprintf("Reporting Delays for %s by Year of Occurrence", value),
                           subtitle = sprintf("Each bar shows the proportion of %s by how many years later they were registered", value)),
           fig("delay_by_occurrence"))
  save_fig(plot_delay_bars(delays, "year", "prop_occ", pal, limits = 1990:2023, sqrt_y = TRUE,
                           x = "Year of occurrence", y = paste("Proportion of", value, "(square-root scale)"),
                           fill = "Delay (year)",
                           title = sprintf("Reporting Delays for %s by Year of Occurrence", value),
                           subtitle = sprintf("Share of %s by years until registration; registrations up to 2024", value)),
           fig("delay_by_occurrence_sqrt"), 9, 9)
  save_fig(plot_delay_bars(delays, "year_reg", "prop_reg", pal, limits = 1990:2023, percent = TRUE,
                           grid = "plain",
                           x = "Year of registration", y = paste("Proportion of", value), fill = "Delay (year)",
                           title = sprintf("Reporting Delays for %s by Year of Registration", reg_word),
                           subtitle = sprintf("Each bar shows the proportion of registered %s by how many years earlier the death occurred", value)),
           fig("delay_by_registration"))

  # Delay by sex, age and marginalization, 2010-2020
  d <- grouped |>
    filter(year >= 2010 & year <= 2020) |>
    left_join(marg_index, by = c("group_id", "year")) |>
    filter(!(sex == "male" & is.na(age)))
  ylab <- tools::toTitleCase(value)

  save_fig(ggplot(d, aes(x = delay, fill = sex, weight = .data[[value]])) +
             geom_bar(position = "dodge") +
             labs(title = "Delay Distribution by Sex", x = "Delay (years)", y = ylab) +
             theme_minimal(),
           fig("delay_by_sex"))
  save_fig(ggplot(d, aes(x = delay, weight = .data[[value]])) +
             geom_bar(fill = "#69b3a2") +
             facet_wrap(~ age) +
             labs(title = "Delay Distribution by Age Group", x = "Delay (years)", y = ylab) +
             theme_minimal(),
           fig("delay_by_age"))
  imn_aes <- if (point_size) aes(x = as.numeric(IMN), y = delay, size = !!rlang::sym(value))
             else aes(x = as.numeric(IMN), y = delay)
  save_fig(ggplot(d, imn_aes) +
             geom_point(alpha = 0.4) +
             geom_smooth(method = "lm", color = "red", se = FALSE) +
             labs(title = "Delay vs Marginalization Index", x = "MPI", y = "Delay (years)") +
             theme_minimal(),
           fig("delay_vs_imn"))
  save_fig(ggplot(d, aes(x = mpi_label, y = delay, weight = .data[[value]])) +
             geom_boxplot() +
             labs(title = "Delay by GM Category", x = "GM", y = "Delay (years)") +
             theme_minimal(),
           fig("delay_by_mpi_category"))
}

delay_figures(deaths_grouped_mun, "deaths", "Deaths")
delay_figures(births_grouped_mun, "births", "births", point_size = TRUE,
              share_rows = filter(births_grouped_mun, sex == "female"))

# --- Birth delay vs deprivation per year, both indices (1990-2020; mother rows) --
# Occurrence years near 2020 have fewer years of follow-up, so their long delays
# are not yet observed (right-censored); this affects all units in a year alike.
dvd_births <- delay_vs_deprivation(unit_mean_delay(filter(births_grouped_mun, sex == "female"), "births"),
                                   "birth")
write.csv(dvd_births$slopes, file.path(fig_dir, "ch3_050_births_delay_vs_deprivation_slopes.csv"),
          row.names = FALSE)
save_fig(dvd_births$plot, "ch3_050_births_delay_vs_deprivation_by_year.pdf", 13, 5.5)
write.csv(dvd_births$slopes_point, file.path(fig_dir, "ch3_050_births_delay_vs_deprivation_per_point.csv"),
          row.names = FALSE)
save_fig(dvd_births$plot_point, "ch3_050_births_delay_vs_deprivation_per_point.pdf", 9, 5.5)

# --- Thesis §3.3: cumulative registration, completeness, mean delay ------------
# Counts are events throughout; births use mother rows only.
D_MAX <- 7   # thesis: common maximum observable delay

events <- bind_rows(
  deaths_grouped_mun |> transmute(group_id, year, year_reg, n = deaths, event = "Deaths"),
  births_grouped_mun |> filter(sex == "female") |>
    transmute(group_id, year, year_reg, n = births, event = "Births")
) |>
  mutate(year = as.integer(year), year_reg = as.integer(year_reg), delay = year_reg - year) |>
  filter(year >= 1990, year <= 2023)
stopifnot(all(events$delay >= 0))

last_reg <- max(events$year_reg)
last_full_year <- last_reg - D_MAX   # last occurrence year with all d <= D_MAX observed
message(sprintf("ch3_050: registrations observed to %d; d <= %d fully observed only for occurrence years <= %d.",
                last_reg, D_MAX, last_full_year))

# Figs 5 / 7: cohort CDF of the delay, CDF(t) = registered within t years /
# registered within D_MAX years. The thesis's "common exposure window across
# cohorts" is undefined; here each cohort is only truncated at D_MAX, so cohorts
# reaching past last_full_year are right-censored (CDF biased upward) and are
# flagged in the legend. The CSV adds each cohort restricted to fully observed years.
cohort_of <- function(y) cut(y, c(1990, 2000, 2010, 2016, 2024), right = FALSE,
                             labels = c("1990-1999", "2000-2009", "2010-2015", "2016-2023"))
reg_cdf <- function(ev) ev |>
  filter(delay <= D_MAX) |>
  mutate(cohort = cohort_of(year)) |>
  group_by(event, cohort, delay) |>
  summarise(n = sum(n), .groups = "drop") |>
  complete(nesting(event, cohort), delay = 0:D_MAX, fill = list(n = 0)) |>
  arrange(event, cohort, delay) |>
  group_by(event, cohort) |>
  mutate(cdf = cumsum(n) / sum(n)) |>
  ungroup()

cdf_all  <- reg_cdf(events)
cdf_full <- reg_cdf(filter(events, year <= last_full_year))
censored <- events |> mutate(cohort = cohort_of(year)) |> group_by(cohort) |>
  summarise(censored = max(year) > last_full_year, .groups = "drop")
cdf_all <- cdf_all |> left_join(censored, by = "cohort") |>
  mutate(cohort_lab = ifelse(censored, paste0(cohort, " (right-censored)"), as.character(cohort)))

readr::write_csv(
  full_join(select(cdf_all, event, cohort, delay, cdf_as_plotted = cdf),
            select(cdf_full, event, cohort, delay, cdf_fully_observed_years = cdf),
            by = c("event", "cohort", "delay")),
  file.path(fig_dir, "ch3_050_registration_cdf.csv"))

d99 <- cdf_all |> filter(cdf >= 0.99) |> group_by(event, cohort_lab) |>
  summarise(d99 = min(delay), .groups = "drop")

plot_cdf <- function(ev) {
  ggplot(filter(cdf_all, event == ev), aes(delay, cdf, colour = cohort_lab)) +
    geom_hline(yintercept = c(0.80, 0.95), colour = "grey60", linewidth = 0.3) +
    geom_vline(data = filter(d99, event == ev), aes(xintercept = d99, colour = cohort_lab),
               linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
    geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
    scale_x_continuous(breaks = 0:D_MAX) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Registration delay d (years)", y = sprintf("Share registered within d (of d <= %d)", D_MAX),
         colour = "Occurrence cohort",
         title = paste("Cumulative registration by occurrence cohort:", tolower(ev)),
         subtitle = "Grey lines: 80% and 95%. Dashed: smallest d with CDF >= 99%.") +
    theme_minimal()
}
save_fig(plot_cdf("Births"), "ch3_050_births_registration_cdf.pdf", height = 5.5)
save_fig(plot_cdf("Deaths"), "ch3_050_deaths_registration_cdf.pdf", height = 5.5)

# Completeness by occurrence year: share of all registrations so far made in the
# occurrence year (d = 0) and within one year (d <= 1) - the basis of §3.3.5's
# "mortality does not require nowcasting". Exact for years <= last_full_year,
# an upper bound after.
completeness <- events |>
  group_by(event, year) |>
  summarise(total = sum(n), share_d0 = sum(n[delay == 0]) / total,
            share_d01 = sum(n[delay <= 1]) / total,
            share_gt1 = 1 - share_d01, .groups = "drop") |>
  mutate(fully_observed = year <= last_full_year)
readr::write_csv(completeness, file.path(fig_dir, "ch3_050_registration_completeness.csv"))
completeness |> filter(fully_observed) |> group_by(event) |>
  summarise(years = paste(range(year), collapse = "-"),
            share_d0_min = min(share_d0), share_d0_max = max(share_d0),
            share_d01_min = min(share_d01), share_d01_max = max(share_d01), .groups = "drop") |>
  as.data.frame() |> print()

# Figs 8-11: event-weighted mean delay per unit x occurrence year, 2010-2020
# (the index's years), by CONAPO grade GM and by area type. Later years have
# less follow-up (2020: d <= 4 observed), so their means are slightly truncated.
gm_index <- readRDS(marg_file("index_grouped_mun")) |>
  transmute(group_id, year = as.integer(year),
            GM = factor(GM, levels = c("Very Low", "Low", "Medium", "High", "Very High")))
area <- readRDS("input-data-processed/rural_urban_area.RDS") |> select(group_id, area_type)

unit_delay <- events |>
  filter(year >= 2010, year <= 2020) |>
  group_by(event, group_id, year) |>
  summarise(mean_delay = weighted.mean(delay, n), n = sum(n), .groups = "drop") |>
  filter(n > 0) |>
  left_join(gm_index, by = c("group_id", "year")) |>
  left_join(area, by = "group_id")
stopifnot(!anyNA(unit_delay$GM), !anyNA(unit_delay$area_type))

plot_box <- function(ev, x, xlab, what) {
  ggplot(filter(unit_delay, event == ev), aes(.data[[x]], mean_delay)) +
    geom_boxplot(fill = "#69b3a2", outlier.size = 0.6) +
    labs(x = xlab, y = "Mean registration delay (years)", title = sprintf("%s: mean delay by %s", ev, what),
         subtitle = "One point per grouped unit and occurrence year, 2010-2020; event-weighted") +
    theme_minimal()
}
for (ev in c("Births", "Deaths")) {
  save_fig(plot_box(ev, "GM", "Degree of marginalization (GM)", "GM"),
           sprintf("ch3_050_%s_mean_delay_by_gm.pdf", tolower(ev)), height = 5.5)
  save_fig(plot_box(ev, "area_type", "Area type", "area type"),
           sprintf("ch3_050_%s_mean_delay_by_area.pdf", tolower(ev)), height = 5.5)
}

summarise_delay <- function(by) {
  unit_delay |> group_by(event, .data[[by]]) |>
    summarise(median = median(mean_delay), q75 = quantile(mean_delay, 0.75), max = max(mean_delay),
              .groups = "drop") |>
    as.data.frame() |> print()
}
summarise_delay("GM")
summarise_delay("area_type")
