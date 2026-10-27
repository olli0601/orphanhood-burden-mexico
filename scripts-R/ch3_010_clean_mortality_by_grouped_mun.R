# =============================================================================
# ch3_010_clean_mortality_by_grouped_mun.R  ·  Chapter 3 — Mortality by grouped municipality
# Deaths re-expressed on the grouped municipalities and joined to grouped
# population and marginalization; delay, rate-vs-deprivation and map figures.
# Rates are PARENTAL-AGE mortality: women 15-64, men 15-79 (the panels stop at
# 64 / 84; drop_nonparental_ages() removes men 80-84). Grouped counterpart of
# ch2_040 (municipal level).
#
# Marginalization is CONAPO's IMN (or its fixed-weight twin): higher = LESS
# marginalized, so quintile 1 is the most marginalized fifth.
#
# Reads : input-data-processed/{deaths, grouped_municipality_50000, geo_info_grouped_mun,
#         population_grouped_mun, index_grouped_mun[_fw]}.RDS
# Writes: input-data-processed/{deaths_grouped_mun, mort_by_grouped_mun}.RDS
#         output/ch3[_fw]/ch3_010_by_grouped_mun_*.pdf
# Run after: ch2_060 (ch2_061 for ORPHAN_MARG=fw)
# =============================================================================

source("R/setup.R")
library(sf)
fig_dir <- marg_fig_dir("output/ch3")
fig <- function(name) paste0("ch3_010_by_grouped_mun_", name, ".pdf")
what <- "Parental-Age Mortality"                       # figure titles
imn_lab <- "Marginalization index (IMN; higher = less marginalized)"

deaths <- readRDS("input-data-processed/deaths.RDS")

# --- Registration delay (deaths) ---------------------------------------------
delays <- delay_summary(deaths, count = "deaths")
pal <- delay_palette(delays$delay)
save_fig(plot_delay_bars(filter(delays, year >= 1990), "year", "total", pal,
                         x = "Year of Registration", y = "Total Deaths", fill = "Delay (Years)",
                         title = "Total Deaths by Occurrence Year and Reporting Year"),
         fig("deaths_by_delay_bar"))
save_fig(plot_delay_bars(delays, "year", "prop_occ", pal, limits = 1990:2023, percent = TRUE,
                         x = "Year of occurrence", y = "Proportion of deaths", fill = "Delay (year)",
                         title = "Reporting Delays for Deaths by Year of Occurrence",
                         subtitle = "Each bar shows the proportion of deaths by how many years later they were registered"),
         fig("delay_prop_by_occurrence"))
save_fig(plot_delay_bars(delays, "year_reg", "prop_reg", pal, limits = 1990:2023, percent = TRUE,
                         grid = "plain",
                         x = "Year of registration", y = "Proportion of deaths", fill = "Delay (year)",
                         title = "Reporting Delays for Deaths by Year of Registration",
                         subtitle = "Each bar shows the proportion of registered deaths by how many years earlier the death occurred"),
         fig("delay_prop_by_registration"))

# --- Grouped panel -----------------------------------------------------------
deaths_grouped_mun <- aggregate_to_grouped_mun(deaths, "deaths", out_col = "tot_deaths") |>
  filter(!(sex == "male" & is.na(age)), year >= 1990)
saveRDS(deaths_grouped_mun, "input-data-processed/deaths_grouped_mun.RDS")

geo_info_grouped_mun   <- readRDS("input-data-processed/geo_info_grouped_mun.RDS")
population_grouped_mun <- readRDS("input-data-processed/population_grouped_mun.RDS")

mort <- deaths_grouped_mun |>
  select(group_id, year, sex, age, tot_deaths, year_reg) |>
  left_join(population_grouped_mun, by = c("group_id", "year", "sex", "age")) |>
  mutate(tot_deaths = replace(tot_deaths, is.na(tot_deaths), 0)) |>
  drop_nonparental_ages() |>
  add_delay(bucket = FALSE)
saveRDS(mort, file = "input-data-processed/mort_by_grouped_mun.RDS")

# --- Rates by marginalization quintile ----------------------------------------
# Years outside the index's coverage take the nearest observed year.
index_marg <- readRDS(marg_file("index_grouped_mun")) |> extend_marg_years(1990:2023)

# Cross-section in 2020, the last year the index is observed.
df_2020_mort <- mort |>
  filter(year == 2020) |>
  left_join(index_marg, by = c("group_id", "year")) |>
  mutate(mort_rate = tot_deaths / population) |>
  drop_na()

# Quintiles are cut over 1990-2023, then the window is narrowed to 2010-2020.
df_mort <- mort |>
  filter(year >= 1990 & year <= 2023) |>
  left_join(index_marg, by = c("group_id", "year")) |>
  mutate(poverty_quintile = ntile(IMN, 5), mort_rate = tot_deaths / population) |>
  filter(year >= 2010 & year <= 2020)

save_fig(plot_quintile_trend(df_mort, "mort_rate", paste("Average", what, "Rate"), paste(what, "Trends Over Time")),
         fig("mortality_trends_quintile"))

std_by_quintile <- compute_std_mort_rate(df_mort) |>
  left_join(index_marg, by = c("group_id", "year")) |>
  mutate(poverty_quintile = ntile(IMN, 5)) |>
  filter(!is.na(poverty_quintile)) |>
  group_by(year, sex, poverty_quintile) |>
  summarise(std_mort_rate = mean(std_mort_rate, na.rm = TRUE), .groups = "drop")
save_fig(plot_quintile_trend(std_by_quintile, "std_mort_rate", paste("Average Standardized", what, "Rate"),
                             paste(what, "Trends Over Time")),
         fig("std_mortality_trends_quintile"))

# --- Standardised rate vs marginalization -------------------------------------
std_raw <- compute_std_mort_rate(df_2020_mort) |>
  left_join(geo_info_grouped_mun |> select(group_id, capital), by = "group_id") |>
  left_join(index_marg, by = c("group_id", "year")) |>
  mutate(capital = factor(capital)) |>
  rename(mpi = IMN) |>
  mutate(mpi = as.numeric(mpi))
save_fig(plot_std_rate(data = std_raw, tt = "Raw data (parental-age mortality, 2020)",
                       y_label = "Standardised parental-age mortality rate") + labs(x = imn_lab),
         fig("std_rate_vs_mpi"))

# --- National age profile -----------------------------------------------------
mort_2000 <- readRDS("input-data-processed/mort_by_grouped_mun.RDS") |> filter(year >= 2000)
mort_national <- mort_2000 |>
  collapse_year_reg("tot_deaths") |>
  national_rates("tot_deaths", "deaths", "mort_rate")
std_age_sex <- compute_national_std_rate_age_gender(mort_national, std_ref_population(population_grouped_mun)) |>
  arrange(year, sex, age) |>
  filter(year >= 1990)
save_fig(plot_age_profile(std_age_sex, what), fig("std_rate_age_sex"), 12, 8)
save_fig(plot_age_profile(filter(std_age_sex, year == 2023), what, by = "sex"), fig("std_rate_age_2023"))

# --- Exploratory figures ------------------------------------------------------
title_theme <- theme(plot.title = element_text(size = 14, face = "bold"),
                     plot.subtitle = element_text(size = 12))
quintile_labels <- c("1 (Poorest)", "2", "3", "4", "5 (Richest)")
sex_colours <- c("female" = "#E69F00", "male" = "#56B4E9")

plot_quintile_series <- function(d, y, ylab, title, subtitle) {
  ggplot(d, aes(x = year, y = .data[[y]], color = as.factor(poverty_quintile), group = poverty_quintile)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_viridis_d(name = "Poverty\nQuintile", labels = quintile_labels) +
    labs(title = title, subtitle = subtitle, x = "Year", y = ylab) +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 12)) +
    title_theme
}

mort_trends <- df_mort |>
  filter(!is.na(poverty_quintile), year >= 2010, year <= 2020) |>
  group_by(year, poverty_quintile) |>
  summarise(avg_mortality_rate = mean(mort_rate, na.rm = TRUE), .groups = "drop")
save_fig(plot_quintile_series(mort_trends, "avg_mortality_rate", paste("Average", what, "Rate"),
                              paste(what, "Trends by Poverty Quintiles (2010-2020)"),
                              "Average parental-age mortality rates across municipality groups"),
         fig("timeseries_raw_quintile"))

std_trends <- std_by_quintile |>
  group_by(year, poverty_quintile) |>
  summarise(std_mort_rate = mean(std_mort_rate, na.rm = TRUE), .groups = "drop")
save_fig(plot_quintile_series(std_trends, "std_mort_rate", paste("Standardized", what, "Rate"),
                              paste("Standardized", what, "Trends by Poverty Quintiles (2010-2020)"),
                              "Age-standardized parental-age mortality rates across municipality groups"),
         fig("timeseries_std_quintile"))

# Marginalization vs standardised rate, 2020
scatter_data <- std_raw |>
  filter(!is.na(mpi)) |>
  mutate(capital_label = ifelse(capital == 1, "Capital", "Non-Capital"))

p_scatter_sex <- ggplot(scatter_data, aes(x = mpi, y = std_mort_rate, color = sex)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = sex_colours) +
  labs(title = paste("Marginalization vs. Standardized", what, "Rates by Sex (2020)"),
       subtitle = "Each point represents a municipality group",
       x = imn_lab, y = paste("Standardized", what, "Rate"), color = "Sex") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  title_theme
save_fig(p_scatter_sex, fig("mpi_vs_mortality_sex"))

p_scatter_capital <- ggplot(scatter_data, aes(x = mpi, y = std_mort_rate, color = capital_label)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  facet_wrap(~ sex) +
  scale_color_manual(values = c("Capital" = "#D55E00", "Non-Capital" = "#009E73")) +
  labs(title = paste("Marginalization vs. Standardized", what, "Rates by Capital Status (2020)"),
       subtitle = "Comparison between capital and non-capital municipalities",
       x = imn_lab, y = paste("Standardized", what, "Rate"),
       color = "Municipality Type") +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
  title_theme
save_fig(p_scatter_capital, fig("mpi_vs_mortality_capital"))

# Delay distributions, one row per death
delay_data <- mort_2000 |>
  filter(!is.na(delay), delay >= 0, delay <= 10) |>
  left_join(index_marg, by = c("group_id", "year")) |>
  filter(!is.na(IMN)) |>
  mutate(poverty_quintile = ntile(IMN, 5),
         poverty_label = quintile_labels[poverty_quintile],
         age_group_broad = case_when(
           age %in% c("15-19", "20-24", "25-29", "30-34") ~ "Young Adults (15-34)",
           age %in% c("35-39", "40-44", "45-49", "50-54") ~ "Middle Age (35-54)",
           age %in% c("55-59", "60-64", "65-69", "70-74") ~ "Older Adults (55-74)",
           TRUE ~ "Elderly (75+)")) |>
  uncount(tot_deaths)

box_theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_box_sex_poverty <- ggplot(delay_data, aes(x = poverty_label, y = delay, fill = sex)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = sex_colours) +
  labs(title = "Reporting Delay Distribution by Sex and Poverty Level",
       subtitle = "Years between death occurrence and registration",
       x = "Poverty Quintile", y = "Reporting Delay (Years)", fill = "Sex") +
  theme_minimal() +
  theme(legend.position = "bottom") + box_theme + title_theme
save_fig(p_box_sex_poverty, fig("delay_box_sex_poverty"))

p_box_age <- ggplot(delay_data, aes(x = age_group_broad, y = delay, fill = age_group_broad)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~ sex) +
  scale_fill_viridis_d() +
  labs(title = "Reporting Delay Distribution by Age Group and Sex",
       subtitle = "Years between death occurrence and registration",
       x = "Age Group", y = "Reporting Delay (Years)", fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) + box_theme + title_theme
save_fig(p_box_age, fig("delay_box_age"))

# --- Maps -----------------------------------------------------------------------
map_units <- function(d, fill, name, option, title, subtitle) {
  ggplot(d) +
    geom_sf(aes(fill = .data[[fill]]), color = "white", size = 0.1) +
    scale_fill_viridis_c(name = name, na.value = "grey90", option = option) +
    labs(title = title, subtitle = subtitle) +
    theme_void() +
    title_theme +
    theme(legend.position = "bottom")
}

delay_geo <- delay_data |>
  group_by(group_id) |>
  summarise(avg_delay = mean(delay, na.rm = TRUE), .groups = "drop") |>
  right_join(geo_info_grouped_mun, by = "group_id") |>
  st_as_sf()
save_fig(map_units(delay_geo, "avg_delay", "Average\nDelay\n(Years)", "D",
                   "Geographic Distribution of Average Reporting Delays",
                   "Average years between death occurrence and registration by municipality group"),
         fig("map_avg_delay"), height = 8)

mortality_geo <- std_raw |>
  group_by(group_id) |>
  summarise(avg_std_mortality = mean(std_mort_rate, na.rm = TRUE),
            avg_mpi = mean(mpi, na.rm = TRUE), .groups = "drop") |>
  right_join(geo_info_grouped_mun, by = "group_id") |>
  st_as_sf()
save_fig(map_units(mortality_geo, "avg_std_mortality", "Std. parental-\nage mortality\nrate", "plasma",
                   paste("Geographic Distribution of Standardized", what, "Rates"),
                   "Age-standardized parental-age mortality rates by municipality group (2020)"),
         fig("map_std_mortality"), height = 8)
save_fig(map_units(mortality_geo, "avg_mpi", "IMN", "cividis",
                   "Geographic Distribution of the Marginalization Index (IMN, 2020)",
                   "Higher values indicate LESS marginalization (CONAPO orientation)"),
         fig("map_mpi"), height = 8)
