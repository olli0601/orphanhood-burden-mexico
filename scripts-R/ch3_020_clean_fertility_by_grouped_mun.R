# =============================================================================
# ch3_020_clean_fertility_by_grouped_mun.R  ·  Chapter 3 — Fertility by grouped municipality
# Births re-expressed on the grouped municipalities and joined to grouped
# population and marginalization; delay and rate-vs-deprivation figures.
# Grouped counterpart of ch2_050 (municipal level).
#
# Reads : input-data-processed/{births, grouped_municipality_50000, geo_info_grouped_mun,
#         population_grouped_mun, index_marg[_fw], index_grouped_mun[_fw]}.RDS
# Writes: input-data-processed/fert_by_grouped_mun.RDS
#         output/ch3[_fw]/ch3_020_by_grouped_mun_*.pdf
# Run after: ch2_060 (ch2_061 for ORPHAN_MARG=fw)
# =============================================================================

source("R/setup.R")
library(sf)
fig_dir <- marg_fig_dir("output/ch3")
fig <- function(name) paste0("ch3_020_by_grouped_mun_", name, ".pdf")

births <- readRDS("input-data-processed/births.RDS")

# --- Registration delay (births on mother rows) ------------------------------
delays <- delay_summary(filter(births, sex == "female"), count = "births", max_delay = 8)
pal <- delay_palette(delays$delay)
save_fig(plot_delay_bars(delays, "year_reg", "total", pal,
                         x = "Year of Registration", y = "Total Births", fill = "Delay (Years)",
                         title = "Total Births by Occurrence Year and Reporting Year"),
         fig("births_total_by_reg_year"))
save_fig(plot_delay_bars(delays, "year", "prop_occ", pal, limits = 1990:2023, percent = TRUE,
                         x = "Year of occurrence", y = "Proportion of births", fill = "Delay (year)",
                         title = "Reporting Delays for births by Year of Occurrence",
                         subtitle = "Each bar shows the proportion of births by how many years later they were registered"),
         fig("births_prop_by_occ_year"))
save_fig(plot_delay_bars(delays, "year_reg", "prop_reg", pal, limits = 1990:2023, percent = TRUE,
                         grid = "plain",
                         x = "Year of registration", y = "Proportion of births", fill = "Delay (year)",
                         title = "Reporting Delays for births by Year of Registration",
                         subtitle = "Each bar shows the proportion of registered births by how many years earlier the death occurred"),
         fig("births_prop_by_reg_year"))

# --- Grouped panel -----------------------------------------------------------
births_grouped_mun <- aggregate_to_grouped_mun(births, "births", out_col = "tot_births") |>
  filter(year >= 1990)

geo_info_grouped_mun   <- readRDS("input-data-processed/geo_info_grouped_mun.RDS")
population_grouped_mun <- readRDS("input-data-processed/population_grouped_mun.RDS")

fert <- births_grouped_mun |>
  select(group_id, year, sex, age, year_reg, tot_births) |>
  left_join(population_grouped_mun, by = c("group_id", "year", "sex", "age")) |>
  mutate(tot_births = replace_na(tot_births, 0), fert_rate = tot_births / population) |>
  drop_nonparental_ages() |>
  add_delay(bucket = FALSE)
saveRDS(fert, file = "input-data-processed/fert_by_grouped_mun.RDS")

# --- Standardised rate vs marginalization (one value per unit) --------------
index_marg <- readRDS(marg_file("index_marg"))

df_2023_fert <- fert |>
  filter(year == 2023) |>
  full_join(index_marg, by = "group_id") |>
  drop_na()

std_raw <- compute_std_fert_rate(df_2023_fert) |>
  left_join(geo_info_grouped_mun |> select(group_id, capital), by = "group_id") |>
  left_join(index_marg, by = "group_id") |>
  mutate(capital = factor(capital)) |>
  rename(mpi = marg_index_weighted) |>
  mutate(mpi = as.numeric(mpi))
save_fig(plot_std_rate(data = std_raw, tt = "Raw data (fertility)"), fig("std_fert_rate_vs_mpi"))

# --- National age profile -----------------------------------------------------
fert <- mutate(fert, year = as.character(year))
fert_national <- fert |>
  collapse_year_reg("tot_births") |>
  national_rates("tot_births", "births", "fert_rate")
std_age_sex <- compute_national_std_rate_age_gender(fert_national, std_ref_population(population_grouped_mun)) |>
  arrange(year, sex, age)
save_fig(plot_age_profile(std_age_sex, "Fertility"), fig("std_fert_rate_by_age_sex_year"), 12, 8)
save_fig(plot_age_profile(filter(std_age_sex, year == 2023), "Fertility", by = "sex"),
         fig("std_fert_2023_by_sex"))

# --- Rates by marginalization quintile ----------------------------------------
# Years outside the index's coverage take the nearest observed year.
index_grouped <- readRDS(marg_file("index_grouped_mun")) |>
  extend_marg_years(1990:2023) |>
  mutate(year = as.character(year))

# Quintiles are cut over 1990-2023 (year is character here; the bounds compare as strings).
df_fert <- fert |>
  left_join(index_grouped, by = c("group_id", "year")) |>
  filter(year >= 1990 & year <= 2023) |>
  mutate(poverty_quintile = ntile(IMN, 5), fert_rate = tot_births / population)
save_fig(plot_quintile_trend(df_fert, "fert_rate", "Average Fertility Rate", "Fertility Trends Over Time"),
         fig("fert_trends_by_quintile"))

std_by_quintile <- compute_std_fert_rate(df_fert) |>
  left_join(index_grouped, by = c("group_id", "year")) |>
  mutate(poverty_quintile = ntile(IMN, 5))
save_fig(plot_quintile_trend(std_by_quintile, "std_fert_rate", "Average Standardized Fertility Rate",
                             "Fertility Trends Over Time"),
         fig("std_fert_trends_by_quintile"))
