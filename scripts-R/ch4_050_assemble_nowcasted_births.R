# =============================================================================
# ch4_050_assemble_nowcasted_births.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Assemble the delay-adjusted birth panel consumed by Chapter 5 (orphanhood):
# per grouped-municipality × year × sex × age-band total births =
# registered-by-today + nowcasted-additional, for 2016-2023. Split out of the
# former ch4_230 (which mixed barplots + this ch5-input assembly).
#
# Reads : input-data-processed/{nowcast_predictions_2016_2023, births_grouped_mun}.RDS
# Writes: input-data-processed/birth_data_all.RDS   (consumed by ch5_020 / ch5_040)
# Run after: ch4_030_nowcast_barplots
# =============================================================================

library(dplyr)

# Per-cell nowcast predictions for 2016-2023 (from ch4_030)
nowcast_predictions <- readRDS("input-data-processed/nowcast_predictions_2016_2023.RDS")

# Registered-by-today counts per occurrence cell (delay observable by 2023)
births <- readRDS("input-data-processed/births_grouped_mun.RDS") %>%
  rename(event_year = year, reg_year = year_reg, municipality = group_id,
         age_group = age, n = births) %>%
  mutate(reg_year = as.numeric(reg_year), delay = reg_year - event_year)

registered_cells <- births %>%
  filter(event_year >= 2016 & event_year <= 2023) %>%
  filter(delay >= 0, reg_year <= 2023) %>%
  mutate(max_delay = 2023 - event_year) %>%
  filter(delay <= max_delay) %>%
  group_by(municipality, event_year, age_group, sex) %>%
  summarise(registered_n = sum(n, na.rm = TRUE), .groups = "drop")

# tot_births = nowcasted incidence (predicted) + already-registered
nowcast_cell_predictions_full <- nowcast_predictions %>%
  left_join(registered_cells, by = c("municipality", "event_year", "age_group", "sex")) %>%
  mutate(registered_n = ifelse(is.na(registered_n), 0, registered_n))

birth_data_all <- nowcast_cell_predictions_full |>
  rename(year = event_year,
         group_id = municipality) |>
  mutate(tot_births = predicted + registered_n) |>
  # any_of(): tolerate per-cell columns this model variant may not carry
  select(-dplyr::any_of(c("log_expected", "age_group", "parent_sex", "expected",
                          "predicted", "se", "lower_95", "upper_95", "mu", "theta",
                          "model_family", "registered_n"))) |>
  relocate(dplyr::any_of(c("group_id", "year", "sex", "age", "tot_births",
                          "population", "fert_rate")))

saveRDS(birth_data_all, "input-data-processed/birth_data_all.RDS")
cat("ch4_050: wrote birth_data_all.RDS (", nrow(birth_data_all), " rows)\n", sep = "")
