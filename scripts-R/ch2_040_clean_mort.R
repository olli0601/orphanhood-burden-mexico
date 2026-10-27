# =============================================================================
# ch2_040_clean_mort.R  ·  Chapter 2 — Mortality at municipality level
# Registered deaths by municipality x sex x age band x year, joined to
# population and IMM 2020; registration-delay figures (all registered deaths)
# and parental-age mortality rates (women 15-64, men 15-79).
# Municipal counterpart of ch3_010 (grouped units).
#
# Reads : input-data-processed/{mortality datasets/*.RDS, grouped_municipality_50000.RDS},
#         input-data-raw/population/.../1_Grupo_Quinq_00_RM.xlsx, input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/mort.RDS
#         output/ch2/ch2_040_*.pdf
# Run after: ch1_015, ch2_010
# =============================================================================

source("R/setup.R")
library(RColorBrewer)
fig_dir <- "output/ch2"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

mort_panel <- load_year_panels("mortality datasets")

# --- Occurrence year within each registration year (last 20 years) -----------
# Sum deaths, not rows: panel cells range from 1 death to thousands.
data_summary <- mort_panel |>
  filter(year >= as.numeric(year_reg) - 20) |>
  group_by(year_reg, year) |>
  summarise(count = sum(deaths), .groups = "drop") |>
  mutate(perc = count / sum(count) * 100, .by = year_reg)

p <- ggplot(data_summary, aes(x = factor(year_reg), y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(perc, 1), "%")), position = position_stack(vjust = 0.5),
            color = "white", size = 3) +
  labs(x = "Registration Year", y = "Total Deaths Count", fill = "Death Year",
       title = "Registered Deaths by Year and Actual Death Year") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(length(unique(data_summary$year)))) +
  theme_minimal() +
  facet_wrap(~year_reg, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right",
        legend.title = element_text(size = 10), legend.text = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"), legend.box.spacing = unit(0.5, "cm"))
save_fig(p, "ch2_040_reg_deaths_by_year.pdf", 12, 8)

data_summary_pie <- data_summary |>
  mutate(perc = round(count / sum(count) * 100, 2), .by = year_reg) |>
  mutate(is_max = perc == max(perc), .by = year_reg)
p_pie <- ggplot(data_summary_pie, aes(x = "", y = perc, fill = factor(year))) +
  geom_bar(stat = "identity", width = 0.5, color = "white", size = 0.1) +
  geom_text(data = subset(data_summary_pie, is_max), aes(label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~year_reg) +
  theme_void() +
  labs(title = "Death Year Distribution per Registration Year") +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  scale_fill_manual(values = rainbow(length(unique(data_summary_pie$year))))
save_fig(p_pie, "ch2_040_death_year_dist_pie.pdf", 12, 8)

# --- Municipal panel with group_id ------------------------------------------------
# tot_deaths = deaths of the grouped unit; the frame stays grouped by unit-year-sex-age.
deaths <- mort_panel |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  filter(year >= 1990 & year < 2024) |>
  attach_group_id(cols = c("state_name", "mun_name", "mun", "group_id")) |>
  group_by(group_id, year, sex, age) |>
  mutate(tot_deaths = sum(deaths))

# --- Registration delay -----------------------------------------------------------
delays <- delay_summary(deaths, count = "deaths") |> mutate(log_total = log(total))
pal <- delay_palette(delays$delay)
save_fig(plot_delay_bars(delays, "year_reg", "log_total", pal, grid = "none",
                         x = "Year of Registration", y = "Total Deaths", fill = "Delay (Years)",
                         title = "Total Deaths by Occurrence Year and Reporting Year"),
         "ch2_040_total_deaths_delay.pdf")
save_fig(plot_delay_bars(delays, "year", "prop_occ", pal, limits = 1990:2023, grid = "none",
                         x = "Year of occurrence", y = "Proportion of deaths", fill = "Delay (year)",
                         title = "Reporting Delays for Deaths by Year of Occurrence",
                         subtitle = "Each bar shows the proportion of deaths by how many years later they were registered"),
         "ch2_040_delay_by_occurrence.pdf")
save_fig(plot_delay_bars(delays, "year_reg", "prop_reg", pal, limits = 1990:2023, grid = "none",
                         x = "Year of registration", y = "Proportion of deaths", fill = "Delay (year)",
                         title = "Reporting Delays for Deaths by Year of Registration",
                         subtitle = "Each bar shows the proportion of registered deaths by how many years earlier the death occurred"),
         "ch2_040_delay_by_registration.pdf")
save_fig(plot_delay_bars(delays, "year_reg", "prop_reg", pal, percent = TRUE, grid = "none",
                         x = "Year of Occurrence", y = "Percentage of Deaths", fill = "Year Reported",
                         title = "Proportion of Deaths by Occurrence Year and Reporting Delay"),
         "ch2_040_delay_pct_by_reg.pdf")

# --- Rates panel: parental ages (women 15-64, men 15-79) ------------------------------
# Unknown-age deaths have no population and are removed by drop_na() below.
population <- load_population(keep_child = FALSE)
mort <- full_join(deaths |> drop_nonparental_ages() |>
                    select(mun, group_id, year, sex, age, year_reg, deaths, tot_deaths),
                  population, by = c("mun", "year", "sex", "age"))
mort$tot_deaths[is.na(mort$tot_deaths)] <- 0
mort$deaths[is.na(mort$deaths)] <- 0

index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)
im.tmp <- index |>
  select(mun, IM, IMN, illiterate_pct, no_basic_edu_pct, no_drainage_pct, no_electricity_pct,
         no_piped_water_pct, dirt_floors_pct, overcrowding_pct, small_towns_pct, low_income_pct) |>
  mutate(mun = as.factor(mun), IM = as.numeric(IM), IMN = as.numeric(IMN))
mort <- mort |> full_join(im.tmp, by = c("mun"))
mort$mort_rate <- mort$deaths / mort$population

n_before <- nrow(mort); d_before <- sum(mort$deaths, na.rm = TRUE)
mort <- mort |> drop_na()
message(sprintf("ch2_040: drop_na() removed %s rows and %s deaths (%.2f%% of parental-age deaths).",
                format(n_before - nrow(mort), big.mark = ","),
                format(round(d_before - sum(mort$deaths)), big.mark = ","),
                100 * (d_before - sum(mort$deaths)) / d_before))
saveRDS(mort, file = "input-data-processed/mort.RDS")

# --- Both-sex standardised parental-age death rate vs IMN, per year ----------------
geo_info <- build_geo_info(index)
mort_mun <- mort |>
  group_by(mun, year, sex, age) |>
  summarise(deaths = sum(deaths), population = first(population), .groups = "drop") |>
  mutate(mort_rate = deaths / population)

std_all <- compute_std_mort_rate(mort_mun, unit_col = "mun", by_sex = FALSE) |>
  select(-sex) |>
  left_join(y = geo_info[, c("mun", "capital")], by = "mun") |>
  left_join(y = index, by = "mun") |>
  mutate(capital = factor(capital)) |>
  rename(mpi = IMN) |>
  mutate(mpi = as.numeric(mpi))

for (yr in 1990:2023) {
  save_fig(plot_std_rate(data = std_all |> filter(year == yr) |> select(-year),
                         tt = paste("Raw data (parental-age mortality) - Year", yr)),
           paste0("ch2_040_std_rate_", yr, ".pdf"))
}
save_fig(plot_std_rate(data = std_all |> filter(year %in% 2005:2023),
                       tt = "Raw data (parental-age mortality) - Years 2005 to 2023") + facet_wrap(~ year),
         "ch2_040_std_rate_multiyear.pdf", 12, 8)

# --- National age profile, 2023 -------------------------------------------------------
mort_national <- mort |>
  collapse_year_reg("deaths", keys = c("mun", "year", "sex", "age")) |>
  national_rates("deaths", "deaths", "mort_rate", na_rm = FALSE)
std_age_sex <- compute_national_std_rate_age_gender(mort_national, std_ref_population(population)) |>
  filter(year == 2023)
save_fig(plot_age_profile(std_age_sex, "Parental-Age Mortality", by = "sex"), "ch2_040_std_rate_age_2023.pdf", 12, 8)
save_fig(plot_age_profile(arrange(std_age_sex, year, sex, age), "Parental-Age Mortality"),
         "ch2_040_std_rate_age_sex.pdf", 12, 8)
