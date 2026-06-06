# =============================================================================
# ch3_040_delays_analysis.R  ·  Chapter 3 — Registration-delay structure
# Characterise birth/death reporting delays (delay distributions, cumulative
# registration curves) and how they vary by marginalization and area type.
#
# Reads : input-data-processed/{deaths, grouped_municipality_50000,
#         aggregated_muni_50000, mpi_imputed, index_new_mun}.RDS
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
source("../R/utils.R")

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

deaths_new_mun <- deaths |>
  group_by(group_id, year, sex, age, year_reg)  |>
  summarise(tot_deaths = sum(deaths), .groups = "drop") |>
  filter(!(sex == "male" & is.na(age)))

deaths_new_mun <- left_join(
  deaths_new_mun, 
  aggregated_muni_50000 %>% dplyr::select(group_id, geometry),
  by="group_id")  %>%
  st_as_sf()


deaths_new_mun <- deaths_new_mun |>
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
ggplot(deaths_summary |> filter(year >=1990), aes(x = factor(year_reg), y = (total_deaths), fill = factor(delay))) +
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


################################################################################

#------------- Interaction between delays and other covariates -----------------

################################################################################
mpi <- readRDS("input-data-processed/mpi_imputed.RDS")
mpi_classified <- readRDS("input-data-processed/index_new_mun.RDS")

delay_df <- deaths_new_mun %>%
  mutate(
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
    avg_delay = mean(delay, na.rm = TRUE),
    .groups = "drop"
  )

delay_df_GM <- delay_df_GM %>%
  mutate(
    GM = case_when(
      IMN >= thresholds$min_value[thresholds$GM == "Muy alto"] &
        IMN <= thresholds$max_value[thresholds$GM == "Muy alto"] ~ "Very High",
      IMN >= thresholds$min_value[thresholds$GM == "Alto"] &
        IMN <= thresholds$max_value[thresholds$GM == "Alto"] ~ "High",
      IMN >= thresholds$min_value[thresholds$GM == "Medio"] &
        IMN <= thresholds$max_value[thresholds$GM == "Medio"] ~ "Medium",
      IMN >= thresholds$min_value[thresholds$GM == "Bajo"] &
        IMN <= thresholds$max_value[thresholds$GM == "Bajo"] ~ "Low",
      IMN >= thresholds$min_value[thresholds$GM == "Muy bajo"] &
        IMN <= thresholds$max_value[thresholds$GM == "Muy bajo"] ~ "Very Low",
      TRUE ~ NA_character_
    )
  )


# Ensure GM is an ordered factor
delay_df_GM <- delay_df_GM %>%
  mutate(
    GM = factor(GM, levels = c("Very Low", "Low", "Medium", "High", "Very High"))
  )
# DEGREE OF MARGINALIZATION
ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
  geom_boxplot(fill = "#69b3a2", color = "black") +
  theme_minimal() +
  labs(
    x = "Marginalization Category",
    y = "Average Delay (days)",
    title = "Registration Delay by Marginalization Category"
  )


# Boxplot faceted by year
ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
  geom_boxplot(fill = "#69b3a2", color = "black") +
  facet_wrap(~ year) +
  theme_minimal() +
  labs(
    x = "Marginalization Category",
    y = "Average Delay (days)",
    title = "Registration Delay by Marginalization Category and Year"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#---------------------------------------------------------------------------------
ggplot(delay_df_GM, aes(x = IMN, y = avg_delay)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Marginalization Index (IMN)",
    y = "Average Delay (days)",
    title = "Delay vs. Marginalization Index"
  ) +
  theme_minimal()


ggplot(delay_df_GM, aes(x = GM, y = avg_delay)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    x = "Marginalization Category",
    y = "Average Delay (days)",
    title = "Average Delay by Marginalization Category"
  ) +
  theme_minimal()

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
    poverty_index = first(IMN)  # Assuming it's constant per municipality
  )

ggplot(municipality_summary, aes(x = poverty_index, y = avg_delay)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Average Delay vs. Poverty Index by Municipality",
    x = "Poverty Index",
    y = "Average Registration Delay (years)"
  ) +
  theme_minimal()

# Choose method depending on distribution/linearity
cor_test_result <- cor.test(
  municipality_summary$poverty_index,
  municipality_summary$avg_delay,
  method = "pearson"  # Use "pearson" if linear
)

print(cor_test_result)

model <- lm(avg_delay ~ poverty_index, data = municipality_summary)

summary(model)



#Histogram of Reporting Delays by Gender
library(ggplot2)
# Boxplot: Delay by Gender
ggplot(delay_df, aes(x = delay, weight = tot_deaths)) +
  geom_histogram(bins = 10, fill = "#69b3a2") +
  facet_wrap(~ sex) +
  labs(
    title = "Delay Distribution by Gender",
    x = "Delay (days)",
    y = "Weighted Count"
  ) +
  theme_minimal()



ggplot(delay_df, aes(x = delay, fill = sex, weight = tot_deaths)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~age) +
  labs(title = "Distribution of Reporting Delays by Gender and Age Group",
       x = "Reporting Delay (Days)",
       y = "Count") +
  theme_minimal()

#  Boxplot of Delay by Age Group and Gender
ggplot(delay_df, aes(x = age, y = delay, fill = sex)) +
  geom_boxplot() +
  labs(title = "Reporting Delays by Age Group and Gender",
       x = "Age Group",
       y = "Delay (Days)") +
  theme_minimal()


library(dplyr)
mean_delay_year <- delay_df %>%
  group_by(year, sex) %>%
  summarize(mean_delay = mean(delay, na.rm = TRUE))

ggplot(mean_delay_year, aes(x = year, y = mean_delay, color = sex)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Mean Reporting Delay Over Time by Gender",
       x = "Year",
       y = "Mean Delay (Days)") +
  theme_minimal()


delay_by_year <- delay_df |>
  group_by(year, group_id) |>
  summarise(
    avg_delay = weighted.mean(delay, tot_deaths, na.rm = TRUE)
  )

municipality_yearly_map <- municipality_shapes |>
  left_join(delay_by_year, by = "municipality_id")

ggplot(municipality_yearly_map) +
  geom_sf(aes(fill = avg_delay)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c() +
  theme_minimal()

ggplot() +
  geom_sf(data = new_mun_50000, fill = NA, color = "grey70", size = 0.1) +
  geom_sf(data = aggregated_muni_50000, fill = NA, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Group Boundaries Overlaid on Original Municipalities")

#------------------
# Average delay per municipality
delay_summary <- your_data |>
  group_by(municipality_id) |>
  summarise(
    avg_delay = weighted.mean(registration_delay, deaths, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE)
  )


municipality_map <- municipality_shapes |>
  left_join(delay_summary, by = "municipality_id")


ggplot(municipality_map) +
  geom_sf(aes(fill = avg_delay)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    fill = "Avg. Delay (days)",
    title = "Average Death Registration Delay by Municipality"
  )


delay_map <- left_join(
  delay_df_GM, 
  aggregated_muni_50000, 
  by="group_id")  %>%
  st_as_sf()

ggplot(data = delay_map) +
  geom_sf(aes(fill = avg_delay)) +
  scale_fill_viridis_c(option = "viridis", name = "Delay") +
  theme_minimal() 

library(tmap)

tmap_mode("view")

tm_shape(delay_map) +
  tm_polygons(
    "avg_delay",
    palette = "viridis",
    id = "group_id",
    popup.vars = c(
      "Delay (days)" = "avg_delay",
      "Marginalization Index" = "IMN",
      "Marginalization Category" = "GM"  
    )
  ) +
  tm_basemap("CartoDB.Positron")


library(tmap)

tmap_mode("view")

tm_shape(delay_map) +
  tm_polygons(
    "avg_delay",
    palette = "viridis",
    id = "group_id",
    popup.vars = c(
      "Delay (days)" = "avg_delay",
      "Marginalization Index" = "IMN",
      "Marginalization Category" = "GM"
    )
  ) +
  tm_facets(pages = "year", drop.units = TRUE, free.coords = FALSE) +
  tm_basemap("CartoDB.Positron")


tmap_mode("view")

tm_shape(delay_map) +
  tm_polygons(
    "avg_delay",
    palette = "viridis",
    id = "group_id",
    popup.vars = c(
      "Delay (days)" = "avg_delay",
      "Marginalization Index" = "IMN",
      "Marginalization Category" = "GM"
    )
  ) +
  tm_facets(by = "year", drop.units = TRUE, free.coords = FALSE) +
  tm_basemap("CartoDB.Positron")


