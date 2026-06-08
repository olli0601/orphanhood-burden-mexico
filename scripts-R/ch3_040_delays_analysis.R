# =============================================================================
# ch3_040_delays_analysis.R  ·  Chapter 3 — Registration-delay structure
# Characterise birth/death reporting delays (delay distributions, cumulative
# registration curves) and how they vary by marginalization and area type.
#
# Reads : input-data-processed/{deaths, grouped_municipality_50000,
#         aggregated_muni_50000, mpi_imputed, index_grouped_mun}.RDS
# Writes: output/ch3/ (delay figures)
# Run after: ch1_060, ch2_010
# =============================================================================

library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(readxl)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(foreign)
library(Polychrome)
library(sf)

fig_dir <- "output/ch3"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################

#------------------------ Reporting delays analysis ----------------------------

################################################################################
deaths <- readRDS("input-data-processed/deaths.RDS")

# 1. Aggregate total deaths by occurrence year and reporting year
deaths_summary <- deaths %>%
  group_by(year, year_reg) %>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  mutate(
    delay = as.numeric(year_reg) - as.numeric(year),
    delay = ifelse(delay > 5, "5+", as.character(delay)),
    delay = factor(delay, levels = c("0", "1", "2", "3", "4", "5", "5+"))
  )


deaths_prop_reg <- deaths_summary %>%
  group_by(year_reg) %>%
  mutate(
    prop_deaths = total_deaths / sum(total_deaths)
  ) %>%
  ungroup()

deaths_prop_occ <- deaths_summary %>%
  group_by(year) %>%
  mutate(
    prop_deaths = total_deaths / sum(total_deaths)
  ) %>%
  ungroup()

# --------- GROUP BY THE NEW MUNICIPALITIES ---------------
grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
aggregated_muni_50000 <- readRDS(file = "input-data-processed/aggregated_muni_50000.RDS")

deaths <- left_join(
  deaths,
  grouped_municipality_50000 %>% dplyr::select(mun, group_id),
  by = "mun"
)

deaths_grouped_mun <- deaths |>
  group_by(group_id, year, sex, age, year_reg)  |>
  summarise(tot_deaths = sum(deaths), .groups = "drop") |>
  filter(!(sex == "male" & is.na(age)))

deaths_grouped_mun <- left_join(
  deaths_grouped_mun, 
  aggregated_muni_50000 %>% dplyr::select(group_id, geometry),
  by="group_id")  %>%
  st_as_sf()


deaths_grouped_mun <- deaths_grouped_mun |>
  filter(year >= 1990)


################################################################################

#---------------------------------- Plots --------------------------------------

################################################################################
delay_levels <- levels(factor(deaths_summary$delay))

# Generate colors and assign names
library(Polychrome)
seed_colors <- c("#FF0000", "#0000FF", "#00FF00")
my_colors <- createPalette(length(delay_levels), seedcolors = seed_colors)
names(my_colors) <- delay_levels

#---------- BARPLOT ------------
## Total deaths
p_deaths_by_reg_year <- ggplot(deaths_summary |> filter(year >=1990), aes(x = factor(year_reg), y = (total_deaths), fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of Registration",
    y = "Total Deaths",
    fill = "Delay (Years)",
    title = "Total Deaths by Occurrence Year and Reporting Year"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3)
  )
ggsave(file.path(fig_dir, "ch3_040_deaths_by_reg_year.pdf"), p_deaths_by_reg_year, width = 8, height = 6)


################################################################################

#------------- Interaction between delays and other covariates -----------------

################################################################################
mpi <- readRDS("input-data-processed/mpi_imputed.RDS")
mpi_classified <- readRDS("input-data-processed/index_grouped_mun.RDS")

delay_df <- deaths_grouped_mun %>%
  sf::st_drop_geometry() %>%   # delay_df is used only for non-spatial summaries/plots;
  mutate(                       # the map plots re-join geometry from aggregated_muni_50000.
    delay = as.numeric(year_reg) - as.numeric(year)
  )


long_deaths_df <- delay_df |>
  uncount(weights = tot_deaths)


delay_df_GM <- delay_df |>
  filter(year>=2010 & year<=2020)|>
  left_join(mpi_classified, by = c("year", "group_id"))

delay_df_GM <- delay_df_GM %>%
  group_by(group_id, year) %>%
  summarise(
    IMN = mean(IMN, na.rm = TRUE),
    GM = dplyr::first(GM),
    avg_delay = mean(delay, na.rm = TRUE),
    .groups = "drop"
  )



# Ensure GM is an ordered factor
delay_df_GM <- delay_df_GM %>%
  mutate(
    GM = factor(GM, levels = c("Very Low", "Low", "Medium", "High", "Very High"))
  )
# DEGREE OF MARGINALIZATION
p_delay_by_marg <- ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
  geom_boxplot(fill = "#69b3a2", color = "black") +
  theme_minimal() +
  labs(
    x = "Marginalization Category",
    y = "Average Delay (days)",
    title = "Registration Delay by Marginalization Category"
  )
ggsave(file.path(fig_dir, "ch3_040_delay_by_marg.pdf"), p_delay_by_marg, width = 8, height = 6)


# Boxplot faceted by year
p_delay_by_marg_year <- ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
  geom_boxplot(fill = "#69b3a2", color = "black") +
  facet_wrap(~ year) +
  theme_minimal() +
  labs(
    x = "Marginalization Category",
    y = "Average Delay (days)",
    title = "Registration Delay by Marginalization Category and Year"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(fig_dir, "ch3_040_delay_by_marg_year.pdf"), p_delay_by_marg_year, width = 8, height = 6)


#---------------------------------------------------------------------------------
p_delay_vs_imn <- ggplot(delay_df_GM, aes(x = IMN, y = avg_delay)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Marginalization Index (IMN)",
    y = "Average Delay (days)",
    title = "Delay vs. Marginalization Index"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_delay_vs_imn.pdf"), p_delay_vs_imn, width = 8, height = 6)


p_delay_by_marg_cat <- ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    x = "Marginalization Category",
    y = "Average Delay (days)",
    title = "Average Delay by Marginalization Category"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_delay_by_marg_cat.pdf"), p_delay_by_marg_cat, width = 8, height = 6)

model <- lm(avg_delay ~ IMN + GM + factor(year), data = delay_df_GM)
summary(model)
#---------------------------------------------------------------------------------


# INDEX OF MARGINALIZATION
delay_df_mpi <- delay_df |>
  filter(year>=2005) |>
  left_join(mpi, by = c("group_id", "year"))

municipality_summary <- delay_df_mpi |>
  group_by(group_id) |>
  summarise(
    avg_delay = weighted.mean(delay, tot_deaths, na.rm = TRUE),
    poverty_index = first(mpi)  # Assuming it's constant per municipality
  )

p_delay_vs_poverty <- ggplot(municipality_summary, aes(x = poverty_index, y = avg_delay)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Average Delay vs. Poverty Index by Municipality",
    x = "Poverty Index",
    y = "Average Registration Delay (years)"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_delay_vs_poverty.pdf"), p_delay_vs_poverty, width = 8, height = 6)

# Choose method depending on distribution/linearity
cor_test_result <- tryCatch(cor.test(municipality_summary$poverty_index, municipality_summary$avg_delay, use = "complete.obs"), error = function(e) {message("ch3_040: cor.test skipped (", conditionMessage(e), ")"); NULL})

if (!is.null(cor_test_result)) print(cor_test_result)

model <- lm(avg_delay ~ poverty_index, data = municipality_summary)

summary(model)



#Histogram of Reporting Delays by Gender
library(ggplot2)
# Boxplot: Delay by Gender
p_delay_hist_by_sex <- ggplot(delay_df, aes(x = delay, weight = tot_deaths)) +
  geom_histogram(bins = 10, fill = "#69b3a2") +
  facet_wrap(~ sex) +
  labs(
    title = "Delay Distribution by Gender",
    x = "Delay (days)",
    y = "Weighted Count"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_delay_hist_by_sex.pdf"), p_delay_hist_by_sex, width = 8, height = 6)



p_delay_hist_by_sex_age <- ggplot(delay_df, aes(x = delay, fill = sex, weight = tot_deaths)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~age) +
  labs(title = "Distribution of Reporting Delays by Gender and Age Group",
       x = "Reporting Delay (Days)",
       y = "Count") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_delay_hist_by_sex_age.pdf"), p_delay_hist_by_sex_age, width = 8, height = 6)

#  Boxplot of Delay by Age Group and Gender
p_delay_box_by_age_sex <- ggplot(delay_df, aes(x = age, y = delay, fill = sex)) +
  geom_boxplot() +
  labs(title = "Reporting Delays by Age Group and Gender",
       x = "Age Group",
       y = "Delay (Days)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_delay_box_by_age_sex.pdf"), p_delay_box_by_age_sex, width = 8, height = 6)


library(dplyr)
mean_delay_year <- delay_df %>%
  group_by(year, sex) %>%
  summarize(mean_delay = mean(delay, na.rm = TRUE))

p_mean_delay_over_time <- ggplot(mean_delay_year, aes(x = year, y = mean_delay, color = sex)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Mean Reporting Delay Over Time by Gender",
       x = "Year",
       y = "Mean Delay (Days)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_mean_delay_over_time.pdf"), p_mean_delay_over_time, width = 8, height = 6)


delay_by_year <- delay_df |>
  group_by(year, group_id) |>
  summarise(
    avg_delay = weighted.mean(delay, tot_deaths, na.rm = TRUE)
  )

if (exists("municipality_shapes")) {
  municipality_yearly_map <- municipality_shapes |>
    left_join(delay_by_year, by = "municipality_id")

  p_delay_map_yearly <- ggplot(municipality_yearly_map) +
    geom_sf(aes(fill = avg_delay)) +
    facet_wrap(~ year) +
    scale_fill_viridis_c() +
    theme_minimal()
  ggsave(file.path(fig_dir, "ch3_040_delay_map_yearly.pdf"), p_delay_map_yearly, width = 8, height = 8)
} else {
  message("ch3_040: skipping delay_map_yearly — object 'municipality_shapes' does not exist upstream")
}

if (exists("grouped_mun_50000")) {
  p_group_boundaries <- ggplot() +
    geom_sf(data = grouped_mun_50000, fill = NA, color = "grey70", size = 0.1) +
    geom_sf(data = aggregated_muni_50000, fill = NA, color = "red", size = 0.5) +
    theme_minimal() +
    labs(title = "Group Boundaries Overlaid on Original Municipalities")
  ggsave(file.path(fig_dir, "ch3_040_group_boundaries.pdf"), p_group_boundaries, width = 8, height = 8)
} else {
  message("ch3_040: skipping group_boundaries — object 'grouped_mun_50000' does not exist upstream")
}

#------------------
# Average delay per municipality
if (exists("your_data") && exists("municipality_shapes")) {
  delay_summary <- your_data |>
    group_by(municipality_id) |>
    summarise(
      avg_delay = weighted.mean(registration_delay, deaths, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE)
    )

  municipality_map <- municipality_shapes |>
    left_join(delay_summary, by = "municipality_id")

  p_delay_map_overall <- ggplot(municipality_map) +
    geom_sf(aes(fill = avg_delay)) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    theme_minimal() +
    labs(
      fill = "Avg. Delay (days)",
      title = "Average Death Registration Delay by Municipality"
    )
  ggsave(file.path(fig_dir, "ch3_040_delay_map_overall.pdf"), p_delay_map_overall, width = 8, height = 8)
} else {
  message("ch3_040: skipping delay_map_overall — objects 'your_data'/'municipality_shapes' do not exist upstream")
}


delay_map <- left_join(
  delay_df_GM, 
  aggregated_muni_50000, 
  by="group_id")  %>%
  st_as_sf()

p_delay_map_groups <- ggplot(data = delay_map) +
  geom_sf(aes(fill = avg_delay)) +
  scale_fill_viridis_c(option = "viridis", name = "Delay") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_040_delay_map_groups.pdf"), p_delay_map_groups, width = 8, height = 8)

# Interactive tmap "view" maps are not saved as static figures (require a browser/leaflet renderer).
message("ch3_040: skipping interactive tmap view maps — interactive HTML widgets, not static figures")



