# =============================================================================
# ch3_030_eda_mexico.R  ·  Chapter 3 — Exploratory data analysis
# Standardised fertility and mortality on the grouped panels: sampled unit
# trends, national age profiles, urban/rural and marginalization contrasts.
#
# Reads : input-data-processed/{births_grouped_mun, deaths_grouped_mun, population_grouped_mun,
#         index_grouped_mun[_fw], rural_urban_area}.RDS
# Writes: output/ch3[_fw]/ch3_030_*.pdf
# Run after: ch3_010 (deaths_grouped_mun), ch2_060
# =============================================================================

source("R/setup.R")
library(forcats)
fig_dir <- marg_fig_dir("output/ch3")

births <- readRDS("input-data-processed/births_grouped_mun.RDS")
deaths <- readRDS("input-data-processed/deaths_grouped_mun.RDS")
population <- readRDS("input-data-processed/population_grouped_mun.RDS") |>
  filter(!age %in% c("00-04", "05-09", "10-14"))
marg_index <- readRDS(marg_file("index_grouped_mun"))
std_ref <- std_ref_population(population)
rural_urban <- readRDS("input-data-processed/rural_urban_area.RDS") |>
  select(group_id, area_type)

# --- Fertility ------------------------------------------------------------------
fertility <- births |>
  left_join(population, by = c("group_id", "year", "sex", "age")) |>
  mutate(fert_rate = births / population) |>
  drop_na()

std_fert <- compute_std_fert_rate(fertility)
std_fert_age <- compute_std_rate_age_gender(fertility, std_ref)

set.seed(123)
sampled_ids <- sample(unique(std_fert$group_id), 9)

# Trend per sampled unit, on a complete yearly grid
fert_prep <- std_fert |>
  filter(group_id %in% sampled_ids) |>
  mutate(year = as.integer(year), sex = as_factor(sex)) |>
  summarise(std_fert_rate = mean(std_fert_rate, na.rm = TRUE), .by = c(group_id, sex, year)) |>
  group_by(group_id, sex) |>
  complete(year = full_seq(year, 1)) |>
  arrange(group_id, sex, year) |>
  ungroup()

p_facets <- ggplot(fert_prep, aes(x = year, y = std_fert_rate, color = sex)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 1.6, na.rm = TRUE) +
  facet_wrap(~ group_id, scales = "free_y") +
  labs(title = "Trend of standardized fertility rate by sex", subtitle = "One panel per group_id",
       x = "Year", y = "Standardized fertility rate", color = "Sex") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
save_fig(p_facets, "ch3_030_std_fert_trend_by_sex_facets.pdf", 12, 8)

# Age composition per sampled unit, stacked to 100% within unit x sex x year
fert_share <- std_fert_age |>
  filter(group_id %in% sampled_ids) |>
  mutate(year = as.integer(year), age = as_factor(age), sex = as_factor(sex)) |>
  summarise(std_fert_rate = mean(std_rate, na.rm = TRUE), .by = c(group_id, sex, age, year)) |>
  group_by(group_id, sex) |>
  complete(year = full_seq(year, 1), age) |>
  arrange(group_id, sex, age, year) |>
  ungroup() |>
  mutate(share = std_fert_rate / sum(std_fert_rate, na.rm = TRUE), .by = c(group_id, sex, year))

p_area <- ggplot(fert_share, aes(year, share, fill = age)) +
  geom_area(color = "white", size = 0.15, na.rm = TRUE) +
  facet_grid(sex ~ group_id) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Composition across age groups", subtitle = "Within each group_id × sex, stacked to 100%",
       x = "Year", y = "Share of total", fill = "Age group") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
save_fig(p_area, "ch3_030_fert_age_composition_area.pdf", 12, 8)

std_nat <- compute_national_std_rate_age_gender(fertility, std_ref) |>
  arrange(year, sex, age) |>
  filter(year >= 1990)
save_fig(plot_age_profile(std_nat, "Fertility"), "ch3_030_std_fert_by_age_year_facets.pdf", 12, 8)

# --- Mortality ------------------------------------------------------------------
mortality <- deaths |>
  filter(!age %in% c("80-84")) |>
  left_join(population, by = c("group_id", "year", "sex", "age")) |>
  mutate(mort_rate = tot_deaths / population, deaths = tot_deaths) |>
  select(-tot_deaths)

# National age profile over all years, before the 2010-2020 restriction below.
std_mort_nat <- compute_national_std_rate_age_gender(mortality, std_ref) |>
  arrange(year, sex, age) |>
  filter(year >= 1990)
save_fig(plot_age_profile(std_mort_nat, "Mortality"), "ch3_030_std_mort_by_age_year_facets.pdf", 12, 8)

# Marginalization and urban/rural, 2010-2020
add_context <- function(d) {
  d |>
    filter(year >= 2010 & year <= 2020) |>
    left_join(marg_index, by = c("group_id", "year")) |>
    left_join(rural_urban, by = "group_id")
}
fertility <- add_context(fertility)
mortality <- add_context(mortality)

std_mort_mun <- compute_std_rate_age_gender(mortality, std_ref)

# --- Distributions and trends ---------------------------------------------------
rate_hist <- function(d, rate, bins, fill, title, xlab) {
  ggplot(filter(d, !is.na(.data[[rate]]) & !is.na(area_type)), aes(x = .data[[rate]])) +
    geom_histogram(bins = bins, fill = fill, color = "black") +
    facet_wrap(~ area_type) +
    labs(title = title, x = xlab, y = "Count")
}
national_trend <- function(d, what) {
  ggplot(d, aes(x = year, y = std_rate, color = sex)) +
    geom_line(aes(group = interaction(sex, age))) +
    labs(title = sprintf("National Standardised %s Rate", what),
         x = "Year", y = sprintf("Standardised %s Rate", what))
}

save_fig(rate_hist(fertility, "fert_rate", 30, "skyblue", "Fertility Rate Distribution by Urban/Rural Area",
                   "Fertility Rate"), "ch3_030_fert_rate_hist_by_area.pdf")
save_fig(national_trend(std_nat, "Fertility"), "ch3_030_std_fert_national.pdf")

save_fig(rate_hist(mortality, "mort_rate", 100, "salmon", "Mortality Rate Distribution by Urban/Rural Area",
                   "Mortality Rate"), "ch3_030_mort_rate_hist_by_area.pdf")

p_mort_vs_marg <- ggplot(filter(mortality, !is.na(IMN) & !is.na(mort_rate)),
                         aes(x = IMN, y = mort_rate, color = area_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Mortality Rate vs. Marginalization Index", x = "Marginalization Index", y = "Mortality Rate")
save_fig(p_mort_vs_marg, "ch3_030_mort_vs_marginalization.pdf")

set.seed(123)
mort_sampled_ids <- sample(unique(std_mort_mun$group_id), 12)
p_std_mort_mun_sample <- ggplot(filter(std_mort_mun, group_id %in% mort_sampled_ids),
                                aes(x = year, y = std_rate, color = sex)) +
  geom_line(aes(group = interaction(sex, age))) +
  facet_wrap(~ group_id, scales = "free_y") +
  labs(title = "Standardised Mortality Rate by Municipality (sampled)",
       x = "Year", y = "Standardised Mortality Rate")
save_fig(p_std_mort_mun_sample, "ch3_030_std_mort_mun_sample.pdf", 12, 8)
save_fig(national_trend(std_mort_nat, "Mortality"), "ch3_030_std_mort_national.pdf")

# --- Rates vs marginalization and area type --------------------------------------
# Correlation across units within each year of the age-standardised rate (per
# woman for fertility, both sexes for mortality) with IMN. IMN runs backwards:
# a NEGATIVE correlation means more marginalized, higher rate.
imn_unit <- distinct(fertility, group_id, year, IMN) |> mutate(year = as.integer(as.character(year)))
cor_by_year <- function(std, col) {
  std |>
    mutate(year = as.integer(as.character(year))) |>
    inner_join(imn_unit, by = c("group_id", "year")) |>
    group_by(year) |>
    summarise(r = cor(.data[[col]], IMN, use = "complete.obs"), units = n(), .groups = "drop")
}
cat("Correlation across units, age-standardised fertility (per woman) vs IMN, by year:\n")
print(as.data.frame(cor_by_year(compute_std_fert_rate(filter(fertility, sex == "female")), "std_fert_rate")))
cat("Correlation across units, age-standardised mortality (both sexes) vs IMN, by year:\n")
print(as.data.frame(cor_by_year(compute_std_mort_rate(mortality, by_sex = FALSE), "std_mort_rate")))

fertility |> group_by(area_type) |> summarise(mean_fert_rate = mean(fert_rate, na.rm = TRUE)) |> print()
mortality |> group_by(area_type) |> summarise(mean_mort_rate = mean(mort_rate, na.rm = TRUE)) |> print()
