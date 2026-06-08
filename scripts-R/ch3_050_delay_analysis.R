# =============================================================================
# ch3_050_delay_analysis.R  ·  Chapter 3 — Delay vs rates
# Relate registration delays to mortality/fertility rates and marginalization
# across municipalities.
#
# Reads : input-data-processed/{deaths, births, grouped_municipality_50000,
#         marg_index}.RDS
# Writes: output/ch3/ (figures)
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
source("R/grouped_mun.R")

fig_dir <- "output/ch3"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################

#-------------------------------- Mortality rate -------------------------------

################################################################################
# DATASET CONTAINING THE DEATHS PER YEAR, SEX, AGE AND MUNICIPALITY
deaths <- readRDS("input-data-processed/deaths.RDS")
deaths <- deaths |>
  filter(!age %in% as.character(0:15))
# GROUP BY THE GROUPED MUNICIPALITIES (R/grouped_mun.R)
deaths_grouped_mun <- aggregate_to_grouped_mun(deaths, "deaths")

# CALCOLATE THE DELAY
delay_df_mort <- deaths_grouped_mun |>
  mutate(delay = as.numeric(year_reg)- as.numeric(year))


deaths_summary <- delay_df_mort %>%
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


delay_levels <- levels(factor(deaths_summary$delay))

# Generate colors and assign names
library(Polychrome)
seed_colors <- c("#FF0000", "#0000FF", "#00FF00")
my_colors <- createPalette(length(delay_levels), seedcolors = seed_colors)
names(my_colors) <- delay_levels

#---------- BARPLOT ------------
## Proportion -- DELAY IN THE FUTURE
p_deaths_delay_by_occurrence <- ggplot(deaths_prop_occ, aes(x = factor(year),
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
    title = "Reporting Delays for deaths by Year of Occurrence",
    subtitle = "Each bar shows the proportion of deaths by how many years later they were registered"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3)
  )
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_occurrence.pdf"), p_deaths_delay_by_occurrence, width = 8, height = 6)



## Proportion -- DELAY IN THE PAST
p_deaths_delay_by_registration <- ggplot(deaths_prop_reg, aes(x = factor(year_reg),
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
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_registration.pdf"), p_deaths_delay_by_registration, width = 8, height = 6)



# JOIN WITH THE MPI
marg_index <- readRDS(file = "input-data-processed/marg_index.RDS")

marg_index <- marg_index |>
  mutate(
    marginalization_quintile = ntile(-as.numeric(IMN), 5),  
    mpi_label = case_when(
      marginalization_quintile == 1 ~ "Very High",
      marginalization_quintile == 2 ~ "High",
      marginalization_quintile == 3 ~ "Medium",
      marginalization_quintile == 4 ~ "Low",
      marginalization_quintile == 5 ~ "Very Low"
    )
  ) |>
  select(-c(GM, marginalization_quintile))

delay_df_mort <- delay_df_mort |>
  filter(year >=2010 & year <=2020) |>
  left_join(marg_index, by = c("group_id", "year"))|>
  filter(!(sex == "male" & is.na(age)))


# INTERACTION WITH SEX
p_deaths_delay_by_sex <- ggplot(delay_df_mort, aes(x = delay, fill = sex, weight = deaths)) +
  geom_bar(position = "dodge") +
  labs(title = "Delay Distribution by Sex", x = "Delay (years)", y = "Deaths") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_sex.pdf"), p_deaths_delay_by_sex, width = 8, height = 6)

# INTERACTION WITH AGE
p_deaths_delay_by_age <- ggplot(delay_df_mort, aes(x = delay, weight = deaths)) +
  geom_bar(fill = "#69b3a2") +
  facet_wrap(~ age) +
  labs(title = "Delay Distribution by Age Group", x = "Delay (years)", y = "Deaths") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_age.pdf"), p_deaths_delay_by_age, width = 8, height = 6)

# INTERACTION WITH MPI
p_deaths_delay_vs_imn <- ggplot(delay_df_mort, aes(x = as.numeric(IMN), y = delay)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Delay vs Marginalization Index", x = "MPI", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_vs_imn.pdf"), p_deaths_delay_vs_imn, width = 8, height = 6)

p_deaths_delay_by_mpi_category <- ggplot(delay_df_mort, aes(x = mpi_label, y = delay, weight = deaths)) +
  geom_boxplot() +
  labs(title = "Delay by GM Category", x = "GM", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_mpi_category.pdf"), p_deaths_delay_by_mpi_category, width = 8, height = 6)

################################################################################

#-------------------------------- Fertility rate -------------------------------

################################################################################


# DATASET CONTAINING THE DEATHS PER YEAR, SEX, AGE AND MUNICIPALITY
births <- readRDS("input-data-processed/births.RDS")

# GROUP BY THE GROUPED MUNICIPALITIES (R/grouped_mun.R)
births_grouped_mun <- aggregate_to_grouped_mun(births, "births")

# CALCOLATE THE DELAY
delay_df <- births_grouped_mun |>
  mutate(delay = as.numeric(year_reg)- as.numeric(year))

births_summary <- delay_df %>%
  group_by(year, year_reg) %>%
  summarise(total_births = n(), .groups = "drop") %>%
  mutate(
    delay = as.numeric(year_reg) - as.numeric(year),
    delay = ifelse(delay > 5, "5+", as.character(delay)),
    delay = factor(delay, levels = c("0", "1", "2", "3", "4", "5", "5+"))
  )


births_prop_reg <- births_summary %>%
  group_by(year_reg) %>%
  mutate(
    prop_births = total_births / sum(total_births)
  ) %>%
  ungroup()

births_prop_occ <- births_summary %>%
  group_by(year) %>%
  mutate(
    prop_births = total_births / sum(total_births)
  ) %>%
  ungroup()


delay_levels <- levels(factor(births_summary$delay))

# Generate colors and assign names
library(Polychrome)
seed_colors <- c("#FF0000", "#0000FF", "#00FF00")
my_colors <- createPalette(length(delay_levels), seedcolors = seed_colors)
names(my_colors) <- delay_levels

#---------- BARPLOT ------------
## Proportion -- DELAY IN THE FUTURE
p_births_delay_by_occurrence <- ggplot(births_prop_occ, aes(x = factor(year),
                            y = prop_births,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of occurrence",
    y = "Proportion of births",
    fill = "Delay (year)",
    title = "Reporting Delays for births by Year of Occurrence",
    subtitle = "Each bar shows the proportion of births by how many years later they were registered"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3)
  )
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_occurrence.pdf"), p_births_delay_by_occurrence, width = 8, height = 6)



## Proportion -- DELAY IN THE PAST
p_births_delay_by_registration <- ggplot(births_prop_reg, aes(x = factor(year_reg),
                            y = prop_births,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year of registration",
    y = "Proportion of births",
    fill = "Delay (year)",
    title = "Reporting Delays for births by Year of Registration",
    subtitle = "Each bar shows the proportion of registered births by how many years earlier the death occurred"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80")
  )
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_registration.pdf"), p_births_delay_by_registration, width = 8, height = 6)


# JOIN WITH THE MPI
marg_index <- readRDS(file = "input-data-processed/marg_index.RDS")

marg_index <- marg_index |>
  mutate(
    marginalization_quintile = ntile(-as.numeric(IMN), 5),  
    mpi_label = case_when(
      marginalization_quintile == 1 ~ "Very High",
      marginalization_quintile == 2 ~ "High",
      marginalization_quintile == 3 ~ "Medium",
      marginalization_quintile == 4 ~ "Low",
      marginalization_quintile == 5 ~ "Very Low"
    )
  ) |>
  select(-c(GM, marginalization_quintile))

delay_df <- delay_df |>
  filter(year >=2010 & year <=2020) |>
  left_join(marg_index, by = c("group_id", "year"))|>
  filter(!(sex == "male" & is.na(age)))


# INTERACTION WITH SEX
p_births_delay_by_sex <- ggplot(delay_df, aes(x = delay, fill = sex, weight = births)) +
  geom_bar(position = "dodge") +
  labs(title = "Delay Distribution by Sex", x = "Delay (years)", y = "Births") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_sex.pdf"), p_births_delay_by_sex, width = 8, height = 6)

# INTERACTION WITH AGE
p_births_delay_by_age <- ggplot(delay_df, aes(x = delay, weight = births)) +
  geom_bar(fill = "#69b3a2") +
  facet_wrap(~ age) +
  labs(title = "Delay Distribution by Age Group", x = "Delay (years)", y = "Births") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_age.pdf"), p_births_delay_by_age, width = 8, height = 6)

# INTERACTION WITH MPI
p_births_delay_vs_imn <- ggplot(delay_df, aes(x = as.numeric(IMN), y = delay, size = births)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Delay vs Marginalization Index", x = "MPI", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_vs_imn.pdf"), p_births_delay_vs_imn, width = 8, height = 6)

p_births_delay_by_mpi_category <- ggplot(delay_df, aes(x = mpi_label, y = delay, weight = births)) +
  geom_boxplot() +
  labs(title = "Delay by GM Category", x = "GM", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_mpi_category.pdf"), p_births_delay_by_mpi_category, width = 8, height = 6)



