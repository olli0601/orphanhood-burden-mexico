# =============================================================================
# ch3_010_clean_mortality_by_grouped_mun.R  ·  Chapter 3 — Mortality by grouped municipality
# Re-express deaths on the AGGREGATED (grouped) municipalities and merge with the
# grouped population, geography and (interpolated) marginalization to produce the
# analysis-ready mortality table at grouped-municipality level. This is the
# grouped counterpart of ch1_040_clean_mort.R (which stays at original-municipality
# level); the two intentionally write different files (mort_by_grouped_mun.RDS vs
# mort.RDS).
#
# Reads : input-data-processed/{deaths, grouped_municipality_*, geo_info*,
#         population*, marg_index}.RDS
# Writes: input-data-processed/{deaths_grouped_mun, mort_by_grouped_mun}.RDS
#         output/ch3/ch3_010_by_grouped_mun_*.pdf
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
source("R/rates.R"); source("R/plots.R"); source("R/load_year_panels.R"); source("R/grouped_mun.R")
fig_dir <- "output/ch3"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
################################################################################

#---------------------------- SHOW DELAY REPORTING ----------------------------

################################################################################
deaths <- readRDS("input-data-processed/deaths.RDS")

# 1. Aggregate total deaths by occurrence year and reporting year
deaths_summary <- deaths %>%
  group_by(year, year_reg) %>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  add_delay(max_delay = 5)


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


delay_levels <- levels(factor(deaths_summary$delay))

# Generate colors and assign names
library(Polychrome)
seed_colors <- c("#FF0000", "#0000FF", "#00FF00")
my_colors <- createPalette(length(delay_levels), seedcolors = seed_colors)
names(my_colors) <- delay_levels

#---------- BARPLOT ------------
## Total deaths
deaths_summary <- deaths_summary |>
  filter(year >=1990)
p_deaths_by_delay_bar <- ggplot(deaths_summary, aes(x = factor(year), y = (total_deaths), fill = factor(delay))) +
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
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_deaths_by_delay_bar.pdf"), p_deaths_by_delay_bar, width = 8, height = 6)

## Proportion -- DELAY IN THE FUTURE
p_delay_prop_by_occurrence <- ggplot(deaths_prop_occ, aes(x = factor(year),
                            y = prop_deaths,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of occurrence",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for Deaths by Year of Occurrence",
    subtitle = "Each bar shows the proportion of deaths by how many years later they were registered"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3)
  )
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_delay_prop_by_occurrence.pdf"), p_delay_prop_by_occurrence, width = 8, height = 6)



## Proportion -- DELAY IN THE PAST
p_delay_prop_by_registration <- ggplot(deaths_prop_reg, aes(x = factor(year_reg),
                            y = prop_deaths,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year of registration",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for Deaths by Year of Registration",
    subtitle = "Each bar shows the proportion of registered deaths by how many years earlier the death occurred"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80")
  )
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_delay_prop_by_registration.pdf"), p_delay_prop_by_registration, width = 8, height = 6)


################################################################################

#----------------------- GROUP BY THE NEW MUNICIPALITIES -----------------------

################################################################################

# --------- GROUP BY THE GROUPED MUNICIPALITIES (R/grouped_mun.R) ---------------
deaths_grouped_mun <- aggregate_to_grouped_mun(deaths, "deaths", out_col = "tot_deaths") |>
  filter(!(sex == "male" & is.na(age)))

deaths_grouped_mun <- deaths_grouped_mun |>
  filter(year >= 1990)

saveRDS(deaths_grouped_mun, "input-data-processed/deaths_grouped_mun.RDS")
################################################################################

#---------------------------------- GEO INFO ----------------------------------

################################################################################
geo_info <- readRDS("input-data-processed/geo_info.RDS")
geo_info_grouped_mun <- readRDS("input-data-processed/geo_info_grouped_mun.RDS")


################################################################################

#--------------------------------- POPULATION ----------------------------------

################################################################################
population_grouped_mun <- readRDS("input-data-processed/population_grouped_mun.RDS")
mort <- left_join(deaths_grouped_mun %>% dplyr::select(group_id, year, sex, age, tot_deaths, year_reg), 
                  population_grouped_mun, 
                  by = c("group_id", "year", "sex", "age"))

mort$tot_deaths[is.na(mort$tot_deaths)] <- 0


mort <- mort |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))
mort <- mort |>
  filter(!(sex == "male" & age == "80-84"))

mort<- mort %>%
  add_delay(bucket = FALSE)

mort <- mort |>
  filter(!age %in% 1:15)
saveRDS(mort, file = "input-data-processed/mort_by_grouped_mun.RDS")
################################################################################

#------------------------------------- MPI -------------------------------------

################################################################################
index_marg <- readRDS("input-data-processed/marg_index.RDS")
library(dplyr)
library(tidyr)

# Ensure year is numeric
index_marg <- index_marg |>
  mutate(year = as.integer(year))

# Get 2010 values for all group_ids
imn_2010 <- index_marg |>
  filter(year == 2010) |>
  mutate(year = list(1990:2009)) |>
  unnest(year)

# Get 2020 values for all group_ids
imn_2020 <- index_marg |>
  filter(year == 2020) |>
  mutate(year = list(2021:2023)) |>
  unnest(year)

# Combine everything
index_marg <- bind_rows(imn_2010, index_marg, imn_2020) |>
  arrange(group_id, year)


df_2023_mort <- mort |> filter(year==2020) |> left_join(index_marg, by = c("group_id", "year")) 
df_2023_mort$mort_rate <- df_2023_mort$tot_deaths / df_2023_mort$population
df_2023_mort <- df_2023_mort |> drop_na()

mort <- mort |>
  filter(year >= 1990 & year <=2023)
df_mort <- left_join(mort, index_marg, by=c("group_id", "year"))

df_mort <- df_mort |>
  mutate(
    poverty_quintile = ntile(IMN, 5)  # 5 = number of quantile groups
  )

df_mort <- df_mort |>
  mutate(
    mort_rate = (tot_deaths/population) 
  )

df_mort <- df_mort |>
  filter(year >= 2010 & year <=2020)

library(dplyr)
library(ggplot2)

# Summarize: average mortality rate per year-sex-quintile
df_summary <- df_mort |>
  filter(!is.na(poverty_quintile)) |>
  group_by(year, sex, poverty_quintile) |>
  summarise(
    mortality_rate = mean(mort_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Plot trends
p_mortality_trends_quintile <- ggplot(df_summary, aes(x = year, y = mortality_rate, color = as.factor(poverty_quintile), group = poverty_quintile)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ sex) +
  theme_minimal() +
  labs(
    title = "Mortality Trends Over Time",
    x = "Year",
    y = "Average Mortality Rate",
    color = "Poverty Quintile"
  )
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_mortality_trends_quintile.pdf"), p_mortality_trends_quintile, width = 8, height = 6)

df_mort_std <- compute_std_mort_rate(df_mort)
df_mort_std <- left_join(df_mort_std, index_marg, by=c("group_id", "year"))
df_mort_std <- df_mort_std |>
  mutate(
    poverty_quintile = ntile(IMN, 5)  # 5 = number of quantile groups
  )

df_mort_std <- df_mort_std|>
  filter(!is.na(poverty_quintile)) |>
  group_by(year, sex, poverty_quintile) |>
  summarise(
    std_mort_rate = mean(std_mort_rate, na.rm = TRUE),
    .groups = "drop"
  )


p_std_mortality_trends_quintile <- ggplot(df_mort_std, aes(x = year, y = std_mort_rate, color = as.factor(poverty_quintile), group = poverty_quintile)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ sex) +
  theme_minimal() +
  labs(
    title = "Mortality Trends Over Time",
    x = "Year",
    y = "Average Standardized Mortality Rate",
    color = "Poverty Quintile"
  )
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_std_mortality_trends_quintile.pdf"), p_std_mortality_trends_quintile, width = 8, height = 6)




################################################################################

#----------------------- Standardized mortality rate ---------------------------

################################################################################
std_raw <- compute_std_mort_rate(df_2023_mort)
std_raw <- std_raw %>% 
  left_join(y = geo_info_grouped_mun |> dplyr::select(group_id, capital), by = "group_id") %>% 
  left_join(y = index_marg, by = c("group_id", "year")) %>% 
  mutate(capital = factor(capital))
std_raw <- std_raw %>% rename("mpi"="IMN")
std_raw$mpi <- as.numeric(std_raw$mpi)

# ---------------------Plot mortality rate against mpi -------------------------
p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (mortality)")
print(p_raw_pts)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_std_rate_vs_mpi.pdf"), p_raw_pts, width = 8, height = 6)


################################################################################

#------------------------- National mortality rate -----------------------------

################################################################################
mort <- readRDS("input-data-processed/mort_by_grouped_mun.RDS")
mort <- mort %>%
  filter(year >= 2000)

mort <- mort |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))
mort <- mort |>
  filter(!(sex == "male" & age == "80-84"))


mort_national <- mort |> 
  group_by(year, age, sex)|> 
  summarise(deaths = sum(tot_deaths, na.rm = TRUE), population = sum(population, na.rm = TRUE), .groups = "drop")

mort_national <- mort_national |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))
mort_national <- mort_national |>
  filter(!(sex == "male" & age == "80-84"))

mort_national$mort_rate <- mort_national$deaths / mort_national$population

mort_national <- mort_national |> drop_na()


# Second: calculate the standardized deaths rate
std_age_sex <- compute_national_std_rate_age_gender(mort_national)
std_age_sex <- std_age_sex %>% arrange(year, sex, age) %>% filter(year>=1990)

# 2. Plot with ggplot
p <- ggplot(std_age_sex, aes(x = age, y = std_rate, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ year) + 
  labs(
    title = "Standardized Mortality Rate by Age and Sex",
    x = "Age Group",
    y = "Standardized Mortality Rate"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_text(size = 8)
  )


print(p)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_std_rate_age_sex.pdf"), p, width = 12, height = 8)


std_age_sex <- std_age_sex %>% filter(year == 2023)


#Plot
p_std_rate_age_2023 <- ggplot(std_age_sex, aes(x = age, y = std_rate, color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_point()+
  facet_wrap(~ sex, nrow = 1) +
  labs(title = "Standardized Mortality Rate by Age Group",
       x = "Age Group",
       y = "Standardized Mortality Rate") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_std_rate_age_2023.pdf"), p_std_rate_age_2023, width = 8, height = 6)


################################################################################

#----------------------- ADDITIONAL EXPLORATORY ANALYSES -----------------------

################################################################################

# Load required libraries for additional visualizations
library(viridis)
library(scales)

################################################################################
# 1. TIME SERIES: Mortality trends by poverty quintiles
################################################################################

# Prepare data for time series analysis
mort_trends <- df_mort %>%
  filter(!is.na(poverty_quintile), year >= 2010, year <= 2020) %>%
  group_by(year, poverty_quintile) %>%
  summarise(
    avg_mortality_rate = mean(mort_rate, na.rm = TRUE),
    #std_mortality_rate = mean(std_mort_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Time series plot - Raw mortality rates
p_timeseries_raw <- ggplot(mort_trends, aes(x = year, y = avg_mortality_rate, 
                                           color = as.factor(poverty_quintile), 
                                           group = poverty_quintile)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  #facet_wrap(~ sex, scales = "free_y") +
  scale_color_viridis_d(name = "Poverty\nQuintile", 
                        labels = c("1 (Poorest)", "2", "3", "4", "5 (Richest)")) +
  labs(
    title = "Mortality Trends by Poverty Quintiles (2010-2020)",
    subtitle = "Average mortality rates across municipalities",
    x = "Year",
    y = "Average Mortality Rate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_timeseries_raw)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_timeseries_raw_quintile.pdf"), p_timeseries_raw, width = 8, height = 6)

df_mort_std_plot <- df_mort_std |>
  group_by(year, poverty_quintile) |>
  summarise(std_mort_rate = mean(std_mort_rate, na.rm = T))
# Time series plot - Standardized mortality rates  
p_timeseries_std <- ggplot(df_mort_std_plot, aes(x = year, y = std_mort_rate, 
                                           color = as.factor(poverty_quintile), 
                                           group = poverty_quintile)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  #facet_wrap(~ sex, scales = "free_y") +
  scale_color_viridis_d(name = "Poverty\nQuintile", 
                        labels = c("1 (Poorest)", "2", "3", "4", "5 (Richest)")) +
  labs(
    title = "Standardized Mortality Trends by Poverty Quintiles (2010-2020)",
    subtitle = "Age-standardized mortality rates across municipalities",
    x = "Year",
    y = "Standardized Mortality Rate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_timeseries_std)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_timeseries_std_quintile.pdf"), p_timeseries_std, width = 8, height = 6)

################################################################################
# 2. SCATTER PLOTS: MPI vs. standardized mortality rates
################################################################################

# Prepare data for scatter plots
scatter_data <- std_raw %>%
  filter(!is.na(mpi)) %>%
  mutate(
    capital_label = ifelse(capital == 1, "Capital", "Non-Capital"),
    poverty_level = case_when(
      mpi <= quantile(mpi, 0.2, na.rm = TRUE) ~ "Low Poverty",
      mpi <= quantile(mpi, 0.4, na.rm = TRUE) ~ "Low-Medium Poverty", 
      mpi <= quantile(mpi, 0.6, na.rm = TRUE) ~ "Medium Poverty",
      mpi <= quantile(mpi, 0.8, na.rm = TRUE) ~ "Medium-High Poverty",
      TRUE ~ "High Poverty"
    )
  )

# Scatter plot by sex
p_scatter_sex <- ggplot(scatter_data, aes(x = mpi, y = std_mort_rate, color = sex)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c("female" = "#E69F00", "male" = "#56B4E9")) +
  labs(
    title = "MPI vs. Standardized Mortality Rates by Sex (2023)",
    subtitle = "Each point represents a municipality group",
    x = "Multidimensional Poverty Index (MPI)",
    y = "Standardized Mortality Rate",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_scatter_sex)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_mpi_vs_mortality_sex.pdf"), p_scatter_sex, width = 8, height = 6)

# Scatter plot by capital status
p_scatter_capital <- ggplot(scatter_data, aes(x = mpi, y = std_mort_rate, color = capital_label)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  facet_wrap(~ sex) +
  scale_color_manual(values = c("Capital" = "#D55E00", "Non-Capital" = "#009E73")) +
  labs(
    title = "MPI vs. Standardized Mortality Rates by Capital Status",
    subtitle = "Comparison between capital and non-capital municipalities",
    x = "Multidimensional Poverty Index (MPI)",
    y = "Standardized Mortality Rate",
    color = "Municipality Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_scatter_capital)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_mpi_vs_mortality_capital.pdf"), p_scatter_capital, width = 8, height = 6)

################################################################################
# 3. BOX PLOTS: Delay distributions by demographic groups  
################################################################################

# Prepare delay data for box plots
delay_data <- mort %>%
  filter(!is.na(delay), delay >= 0, delay <= 10) %>%  # Filter extreme delays
  left_join(index_marg, by = c("group_id", "year")) %>%
  filter(!is.na(IMN)) %>%
  mutate(
    poverty_quintile = ntile(IMN, 5),
    poverty_label = case_when(
      poverty_quintile == 1 ~ "1 (Poorest)",
      poverty_quintile == 2 ~ "2",
      poverty_quintile == 3 ~ "3", 
      poverty_quintile == 4 ~ "4",
      poverty_quintile == 5 ~ "5 (Richest)"
    ),
    age_group_broad = case_when(
      age %in% c("15-19", "20-24", "25-29", "30-34") ~ "Young Adults (15-34)",
      age %in% c("35-39", "40-44", "45-49", "50-54") ~ "Middle Age (35-54)",
      age %in% c("55-59", "60-64", "65-69", "70-74") ~ "Older Adults (55-74)",
      TRUE ~ "Elderly (75+)"
    )
  ) %>%
  # Create weighted observations for proper delay distribution
  uncount(tot_deaths)

# Box plot by sex and poverty quintile
p_box_sex_poverty <- ggplot(delay_data, aes(x = poverty_label, y = delay, fill = sex)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("female" = "#E69F00", "male" = "#56B4E9")) +
  labs(
    title = "Reporting Delay Distribution by Sex and Poverty Level",
    subtitle = "Years between death occurrence and registration",
    x = "Poverty Quintile",
    y = "Reporting Delay (Years)",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_box_sex_poverty)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_delay_box_sex_poverty.pdf"), p_box_sex_poverty, width = 8, height = 6)

# Box plot by age groups
p_box_age <- ggplot(delay_data, aes(x = age_group_broad, y = delay, fill = age_group_broad)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~ sex) +
  scale_fill_viridis_d() +
  labs(
    title = "Reporting Delay Distribution by Age Group and Sex",
    subtitle = "Years between death occurrence and registration",
    x = "Age Group",
    y = "Reporting Delay (Years)",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_box_age)
ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_delay_box_age.pdf"), p_box_age, width = 8, height = 6)

################################################################################
# 4. CHOROPLETH MAPS: Geographic patterns of delays and mortality
################################################################################

# Load spatial data if available
if(exists("geo_info_grouped_mun") && "sf" %in% class(geo_info_grouped_mun)) {
  
  # Prepare geographic data for delays
  delay_geo_data <- delay_data %>%
    group_by(group_id) %>%
    summarise(
      avg_delay = mean(delay, na.rm = TRUE),
      median_delay = median(delay, na.rm = TRUE),
      prop_delayed = mean(delay > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(geo_info_grouped_mun, by = "group_id") %>%
    st_as_sf()
  
  # Choropleth map - Average delay
  p_map_delay <- ggplot(delay_geo_data) +
    geom_sf(aes(fill = avg_delay), color = "white", size = 0.1) +
    scale_fill_viridis_c(name = "Average\nDelay\n(Years)", na.value = "grey90") +
    labs(
      title = "Geographic Distribution of Average Reporting Delays",
      subtitle = "Average years between death occurrence and registration by municipality group"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  print(p_map_delay)
  ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_map_avg_delay.pdf"), p_map_delay, width = 8, height = 8)

  # Prepare geographic data for mortality
  mortality_geo_data <- std_raw %>%
    group_by(group_id) %>%
    summarise(
      avg_std_mortality = mean(std_mort_rate, na.rm = TRUE),
      avg_mpi = mean(mpi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(geo_info_grouped_mun, by = "group_id") %>%
    st_as_sf()
  
  # Choropleth map - Standardized mortality
  p_map_mortality <- ggplot(mortality_geo_data) +
    geom_sf(aes(fill = avg_std_mortality), color = "white", size = 0.1) +
    scale_fill_viridis_c(name = "Std.\nMortality\nRate", na.value = "grey90",
                         option = "plasma") +
    labs(
      title = "Geographic Distribution of Standardized Mortality Rates",
      subtitle = "Age-standardized mortality rates by municipality group (2023)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  print(p_map_mortality)
  ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_map_std_mortality.pdf"), p_map_mortality, width = 8, height = 8)

  # Choropleth map - MPI
  p_map_mpi <- ggplot(mortality_geo_data) +
    geom_sf(aes(fill = avg_mpi), color = "white", size = 0.1) +
    scale_fill_viridis_c(name = "MPI\nScore", na.value = "grey90",
                         option = "cividis") +
    labs(
      title = "Geographic Distribution of Multidimensional Poverty Index",
      subtitle = "Higher values indicate higher poverty levels"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  print(p_map_mpi)
  ggsave(file.path(fig_dir, "ch3_010_by_grouped_mun_map_mpi.pdf"), p_map_mpi, width = 8, height = 8)

} else {
  print("Note: Geographic data (geo_info_grouped_mun) not available as sf object for choropleth maps")
  print("Please ensure the spatial data is loaded and has geometry information")
}

print("Exploratory Data Analysis plots completed!")

