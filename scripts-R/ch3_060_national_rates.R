# =============================================================================
# ch3_060_national_rates.R  ·  Chapter 3 — National rates as a data-health check
# National age-specific fertility and mortality rates (crude per band, not
# standardised), TFR, and the 18-year cumulative fertility that feeds the
# orphanhood engine, all before any nowcasting. Each year is drawn against the
# most recent year with population (dotted) as a reference.
#
# Reads : input-data-processed/{fert_by_grouped_mun, mort_by_grouped_mun}.RDS
# Writes: output/ch3/ch3_060_{fert_rate_by_age, mort_rate_by_age, tfr,
#         cum_fert_by_age}.pdf, output/ch3/ch3_060_national_rates.csv
# Run after: ch3_010, ch3_020
# =============================================================================

source("R/setup.R")
fig_dir <- "output/ch3"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
sex_colours <- c(female = "#E69F00", male = "#56B4E9")

# Rows outside the grouping (group_id NA) or without population (2024: the
# population table ends in 2023) have no denominator; drop them from numerator
# and denominator alike rather than let national_rates() keep their events.
national <- function(file, count_col, out_col, rate_col) {
  readRDS(file) |>
    filter(!is.na(group_id), !is.na(population)) |>
    collapse_year_reg(count_col) |>
    national_rates(count_col, out_col, rate_col) |>
    mutate(year = as.integer(as.character(year)), sex = as.character(sex))
}
fert <- national("input-data-processed/fert_by_grouped_mun.RDS", "tot_births", "births", "fert_rate")
mort <- national("input-data-processed/mort_by_grouped_mun.RDS", "tot_deaths", "deaths", "mort_rate") |>
  drop_nonparental_ages()
ref_year <- max(fert$year)
stopifnot(ref_year == max(mort$year))

# One panel per year; dotted line = the same sex's profile in `ref_year`.
plot_by_year <- function(d, y, ylab, title, x = "age", xlab = "Age band") {
  ref <- filter(d, year == ref_year) |> select(-year)
  ggplot(d, aes(.data[[x]], .data[[y]], colour = sex, group = sex)) +
    geom_line(data = ref, linetype = "dotted", linewidth = 0.5) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ year) +
    scale_colour_manual(values = sex_colours) +
    labs(title = title, subtitle = sprintf("Dotted: %d, the most recent year with population. No nowcasting.", ref_year),
         x = xlab, y = ylab, colour = "Sex") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6), legend.position = "bottom")
}

save_fig(plot_by_year(fert, "fert_rate", "Births per person-year",
                      "National age-specific fertility rate (mother and father rows)"),
         "ch3_060_fert_rate_by_age.pdf", 14, 11)
save_fig(plot_by_year(mort, "mort_rate", "Deaths per person-year",
                      "National age-specific mortality rate, parental ages") +
           scale_y_log10(),
         "ch3_060_mort_rate_by_age.pdf", 14, 11)

# --- TFR -------------------------------------------------------------------------
# Sum of five-year band rates x 5. Mothers 15-50 (INEGI top-codes at 50; births
# to mothers under 15 are not in the panels), fathers 15-79.
tfr <- fert |>
  summarise(tfr = 5 * sum(fert_rate), .by = c(year, sex))
p_tfr <- ggplot(tfr, aes(year, tfr, colour = sex)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_colour_manual(values = sex_colours) +
  labs(title = "Total fertility rate, by registration up to 2024 (no nowcasting)",
       subtitle = "Female: mothers 15-50. Male: same births on fathers' rows, fathers 15-79.",
       x = "Year of occurrence", y = "TFR", colour = "Sex") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
save_fig(p_tfr, "ch3_060_tfr.pdf")

# --- Cumulative fertility over the past 18 years -------------------------------
# For a parent aged a in year t: sum_{k=0..17} f(a - k, t - k), with f the
# single-year rate taken as constant within its five-year band. This is the
# children-born-in-the-last-18-years term of ch5_020 (before child survival).
# Only (a, t) whose 18-year history lies within the data (t - 17 >= 1990) are
# complete; ages under 15 at birth contribute zero.
band_of <- function(a) sprintf("%d-%d", 15 + 5 * ((a - 15) %/% 5), 19 + 5 * ((a - 15) %/% 5))
f_single <- fert |>
  select(year, sex, band = age, fert_rate) |>
  inner_join(tidyr::expand_grid(age = 15:79) |> mutate(band = band_of(age)),
             by = "band", relationship = "many-to-many")
first_year <- min(fert$year)
cum <- tidyr::expand_grid(year = (first_year + 17):ref_year, sex = c("female", "male"),
                          age = 15:79, k = 0:17) |>
  mutate(age_b = age - k, year_b = year - k) |>
  filter(age_b >= 15) |>
  left_join(f_single, by = c(year_b = "year", sex = "sex", age_b = "age")) |>
  summarise(cum_fert = sum(fert_rate, na.rm = TRUE), .by = c(year, sex, age))

save_fig(plot_by_year(cum, "cum_fert", "Children born in the past 18 years, per person",
                      "National cumulative fertility over the past 18 years",
                      xlab = "Age of parent (single years)") +
           theme(axis.text.x = element_text(angle = 0, size = 7)),
         "ch3_060_cum_fert_by_age.pdf", 14, 9)

bind_rows(fert |> transmute(measure = "fert_rate", year, sex, age, value = fert_rate),
          mort |> transmute(measure = "mort_rate", year, sex, age, value = mort_rate),
          tfr  |> transmute(measure = "tfr", year, sex, age = NA_character_, value = tfr),
          cum  |> transmute(measure = "cum_fert_18y", year, sex, age = as.character(age), value = cum_fert)) |>
  readr::write_csv(file.path(fig_dir, "ch3_060_national_rates.csv"))
