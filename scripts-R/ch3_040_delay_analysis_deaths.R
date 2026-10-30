# =============================================================================
# ch3_040_delay_analysis_deaths.R  ·  Chapter 3 — Death registration-delay structure
# Death registration delays by sex, age and year, and against marginalization
# (grade, index, regressions) across grouped municipalities.
#
# Reads : input-data-processed/{deaths, grouped_municipality_50000, aggregated_muni_50000,
#         mpi_imputed[_fw], index_grouped_mun[_fw]}.RDS
# Writes: output/ch3[_fw]/ch3_040_*.pdf
# Run after: ch2_060 (ch2_061 for ORPHAN_MARG=fw)
# =============================================================================

source("R/setup.R")
library(sf)
fig_dir <- marg_fig_dir("output/ch3")

deaths <- readRDS("input-data-processed/deaths.RDS")

# --- Registration delay -------------------------------------------------------------
delays <- delay_summary(deaths, count = "deaths")
save_fig(plot_delay_bars(filter(delays, year >= 1990), "year_reg", "total", delay_palette(delays$delay),
                         x = "Year of Registration", y = "Total Deaths", fill = "Delay (Years)",
                         title = "Total Deaths by Occurrence Year and Reporting Year"),
         "ch3_040_deaths_by_reg_year.pdf")

# --- Delay per grouped unit -------------------------------------------------------
# Unmatched municipalities are kept here as group_id = NA.
aggregated_muni_50000 <- readRDS("input-data-processed/aggregated_muni_50000.RDS")
delay_df <- aggregate_to_grouped_mun(deaths, "deaths", out_col = "tot_deaths", drop_unmatched = FALSE) |>
  filter(!(sex == "male" & is.na(age)), year >= 1990) |>
  add_delay(bucket = FALSE)

# --- Delay vs marginalization grade (death-weighted unit-year means, 2010-2020) ---
delay_df_GM <- delay_df |>
  filter(year >= 2010 & year <= 2020) |>
  left_join(readRDS(marg_file("index_grouped_mun")), by = c("year", "group_id")) |>
  group_by(group_id, year) |>
  summarise(IMN = mean(IMN, na.rm = TRUE), GM = first(GM),
            avg_delay = weighted.mean(delay, tot_deaths, na.rm = TRUE), .groups = "drop") |>
  mutate(GM = factor(GM, levels = c("Very Low", "Low", "Medium", "High", "Very High")))

marg_box <- function(fill, title) {
  ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
    geom_boxplot(fill = fill, color = "black") +
    theme_minimal() +
    labs(x = "Marginalization Category", y = "Average Delay (years)", title = title)
}
save_fig(marg_box("#69b3a2", "Registration Delay by Marginalization Category"), "ch3_040_delay_by_marg.pdf")
save_fig(marg_box("#69b3a2", "Registration Delay by Marginalization Category and Year") +
           facet_wrap(~ year) + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
         "ch3_040_delay_by_marg_year.pdf")

p_delay_vs_imn <- ggplot(delay_df_GM, aes(x = IMN, y = avg_delay)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Marginalization Index (IMN)", y = "Average Delay (years)", title = "Delay vs. Marginalization Index") +
  theme_minimal()
save_fig(p_delay_vs_imn, "ch3_040_delay_vs_imn.pdf")

p_delay_by_marg_cat <- ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Marginalization Category", y = "Average Delay (years)", title = "Average Delay by Marginalization Category") +
  theme_minimal()
save_fig(p_delay_by_marg_cat, "ch3_040_delay_by_marg_cat.pdf")

summary(lm(avg_delay ~ IMN + GM + factor(year), data = delay_df_GM))

# --- Delay vs interpolated index (death-weighted unit means, 2005+) --------------
# mpi covers 2010-2020 only, so poverty_index averages the years where it exists.
municipality_summary <- delay_df |>
  filter(year >= 2005) |>
  left_join(readRDS(marg_file("mpi_imputed")), by = c("group_id", "year")) |>
  group_by(group_id) |>
  summarise(avg_delay = weighted.mean(delay, tot_deaths, na.rm = TRUE),
            poverty_index = mean(mpi, na.rm = TRUE))
stopifnot(sum(is.finite(municipality_summary$poverty_index)) > 0.9 * nrow(municipality_summary))

p_delay_vs_poverty <- ggplot(municipality_summary, aes(x = poverty_index, y = avg_delay)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Average Delay vs. Poverty Index by Municipality", x = "Poverty Index",
       y = "Average Registration Delay (years)") +
  theme_minimal()
save_fig(p_delay_vs_poverty, "ch3_040_delay_vs_poverty.pdf")

cor_test_result <- tryCatch(cor.test(municipality_summary$poverty_index, municipality_summary$avg_delay),
                            error = function(e) { message("ch3_040: cor.test skipped (", conditionMessage(e), ")"); NULL })
if (!is.null(cor_test_result)) print(cor_test_result)
summary(lm(avg_delay ~ poverty_index, data = municipality_summary))

# --- Delay vs deprivation per year, both indices (1990-2020) --------------------------
dvd <- delay_vs_deprivation(unit_mean_delay(delay_df, "tot_deaths"), "death")
write.csv(dvd$slopes, file.path(fig_dir, "ch3_040_delay_vs_deprivation_slopes.csv"), row.names = FALSE)
save_fig(dvd$plot, "ch3_040_delay_vs_deprivation_by_year.pdf", 13, 5.5)
write.csv(dvd$slopes_point, file.path(fig_dir, "ch3_040_delay_vs_deprivation_per_point.csv"), row.names = FALSE)
save_fig(dvd$plot_point, "ch3_040_delay_vs_deprivation_per_point.pdf", 9, 5.5)

# --- Delay by sex, age and year ------------------------------------------------------
p_delay_hist_by_sex <- ggplot(delay_df, aes(x = delay, weight = tot_deaths)) +
  geom_histogram(bins = 10, fill = "#69b3a2") +
  facet_wrap(~ sex) +
  labs(title = "Delay Distribution by Gender", x = "Delay (years)", y = "Weighted Count") +
  theme_minimal()
save_fig(p_delay_hist_by_sex, "ch3_040_delay_hist_by_sex.pdf")

p_delay_hist_by_sex_age <- ggplot(delay_df, aes(x = delay, fill = sex, weight = tot_deaths)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~ age) +
  labs(title = "Distribution of Reporting Delays by Gender and Age Group",
       x = "Reporting Delay (years)", y = "Count") +
  theme_minimal()
save_fig(p_delay_hist_by_sex_age, "ch3_040_delay_hist_by_sex_age.pdf")

p_delay_box_by_age_sex <- ggplot(delay_df, aes(x = age, y = delay, fill = sex)) +
  geom_boxplot() +
  labs(title = "Reporting Delays by Age Group and Gender", x = "Age Group", y = "Delay (years)") +
  theme_minimal()
save_fig(p_delay_box_by_age_sex, "ch3_040_delay_box_by_age_sex.pdf")

mean_delay_year <- delay_df |>
  group_by(year, sex) |>
  summarise(mean_delay = weighted.mean(delay, tot_deaths, na.rm = TRUE), .groups = "drop")
p_mean_delay_over_time <- ggplot(mean_delay_year, aes(x = year, y = mean_delay, color = sex)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Mean Reporting Delay Over Time by Gender", x = "Year", y = "Mean Delay (years)") +
  theme_minimal()
save_fig(p_mean_delay_over_time, "ch3_040_mean_delay_over_time.pdf")

# --- Map ------------------------------------------------------------------------------
delay_map <- left_join(delay_df_GM, aggregated_muni_50000, by = "group_id") |> st_as_sf()
p_delay_map_groups <- ggplot(delay_map) +
  geom_sf(aes(fill = avg_delay)) +
  scale_fill_viridis_c(option = "viridis", name = "Delay") +
  theme_minimal()
save_fig(p_delay_map_groups, "ch3_040_delay_map_groups.pdf", height = 8)
