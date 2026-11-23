# =============================================================================
# ch3_051_birth_delay_days.R  ·  Chapter 3 — Birth registration delay in days
# Birth-cohort view of registration delay, with the delay measured in days from
# birth to registration rather than in calendar years (ch3_050): share of each
# cohort registered within d days, and days until 95% are registered, by
# deprivation decile, and months until 90/95/98% are registered, by year and by
# month of birth. Cohorts 1998-2023 (day of birth is recorded from 1998).
#
# Reads : input-data-processed/birth delay days/delay_*.RDS (ch1_013),
#         input-data-processed/{grouped_municipality_50000, index_grouped_mun_fw}.RDS
# Writes: output/ch3/ch3_051_births_{cumulative_days,quantile95_days}_by_decile.pdf
#         output/ch3/ch3_051_births_quantile{,95}_months_by_decile.pdf
#         output/ch3/ch3_051_births_quantile_months_by_birth_month_decile.pdf
#         output/ch3/ch3_051_births_{days,quantile_months}_by_decile.csv
# Run after: ch1_013, ch2_061
# =============================================================================

source("R/setup.R")
fig_dir <- "output/ch3"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files("input-data-processed/birth delay days", pattern = "^delay_[0-9]{4}[.]RDS$",
                    full.names = TRUE)
stopifnot(setequal(as.integer(gsub("\\D", "", basename(files))), 1998:2024))
# ~1M rows per registration year: collapse each to grouped units before stacking.
grouping <- read_group_lookup()
days <- lapply(files, \(f) readRDS(f) |> filter(year %in% 1998:2023) |>
                 aggregate_to_grouped_mun("births", by = c("year", "month", "delay_days"),
                                          keep_year_reg = FALSE, grouping = grouping)) |>
  dplyr::bind_rows() |>
  group_by(group_id, year, month, delay_days) |>
  summarise(births = sum(births), .groups = "drop")

no_delay <- days |> filter(year %in% 1998:2023) |>
  summarise(share = sum(births[is.na(delay_days)]) / sum(births))
message(sprintf("ch3_051: %.3f%% of 1998-2023 births have no valid delay (dropped)", 100 * no_delay$share))

dd <- decile_days(days, 1998:2023)
dd_month <- decile_days(days, 1998:2023, by_month = TRUE)
message(sprintf("ch3_051: %.3f%% of births with a valid delay have unknown month of birth (dropped by month)",
                100 * sum(dd_month$births[is.na(dd_month$month)]) / sum(dd_month$births)))

p_cum <- plot_cumulative_days_by_decile(dd)
p_q95 <- plot_quantile_days_by_decile(dd)
save_fig(p_cum, "ch3_051_births_cumulative_days_by_decile.pdf", 13, 9)
save_fig(p_q95, "ch3_051_births_quantile95_days_by_decile.pdf", 9, 5.5)
# The same quantiles in months, the analogue of ch3_050's yearly plots.
p_qm <- plot_quantile_months_by_decile(dd)
save_fig(p_qm, "ch3_051_births_quantile_months_by_decile.pdf", 13, 5.5)
save_fig(plot_quantile_months_by_decile(dd, probs = 0.95),
         "ch3_051_births_quantile95_months_by_decile.pdf", 9, 5.5)
save_fig(plot_quantile_months_by_decile(dd_month),
         "ch3_051_births_quantile_months_by_birth_month_decile.pdf", 16, 5.5)
write.csv(tidyr::pivot_wider(dplyr::select(p_qm$data, -x), names_from = p, values_from = q),
          file.path(fig_dir, "ch3_051_births_quantile_months_by_decile.csv"), row.names = FALSE)
# The plotted values: share within d days, and days until 95% registered.
write.csv(left_join(tidyr::pivot_wider(p_cum$data, names_from = d, values_from = share),
                    rename(p_q95$data, days_to_95pct = q), by = c("year", "decile")),
          file.path(fig_dir, "ch3_051_births_days_by_decile.csv"), row.names = FALSE)
