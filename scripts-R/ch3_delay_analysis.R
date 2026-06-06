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

################################################################################

#-------------------------------- Mortality rate -------------------------------

################################################################################
# DATASET CONTAINING THE DEATHS PER YEAR, SEX, AGE AND MUNICIPALITY
deaths <- readRDS("../datasets/deaths.RDS")
deaths <- deaths |>
  filter(!age %in% as.character(0:15))
# GROUP BY THE NEW MUNICIPALITIES
new_mun <- readRDS("../datasets/grouped_municipality_50000.RDS")
deaths_new_mun <- deaths |>
  left_join(new_mun |> dplyr::select(mun, group_id), by="mun") |>
  group_by(group_id, year, year_reg, sex, age) |>
  summarise(deaths = sum(deaths), .groups = "drop")

# CALCOLATE THE DELAY
delay_df_mort <- deaths_new_mun |>
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
ggplot(deaths_prop_occ, aes(x = factor(year),
                            y = prop_deaths,
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



## Proportion -- DELAY IN THE PAST
ggplot(deaths_prop_reg, aes(x = factor(year_reg),
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



# JOIN WITH THE MPI
marg_index <- readRDS(file = "../datasets/marg_index.RDS")

marg_index <- marg_index |>
  mutate(
    marginalization_quintile = ntile(-IMN, 5),  
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
ggplot(delay_df_mort, aes(x = delay, fill = sex, weight = deaths)) +
  geom_bar(position = "dodge") +
  labs(title = "Delay Distribution by Sex", x = "Delay (days)", y = "Deaths") +
  theme_minimal()

# INTERACTION WITH AGE
ggplot(delay_df_mort, aes(x = delay, weight = deaths)) +
  geom_bar(fill = "#69b3a2") +
  facet_wrap(~ age) +
  labs(title = "Delay Distribution by Age Group", x = "Delay (days)", y = "Deaths") +
  theme_minimal()

# INTERACTION WITH MPI
ggplot(delay_df_mort, aes(x = IMN, y = delay)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Delay vs Marginalization Index", x = "MPI", y = "Delay (days)") +
  theme_minimal()

ggplot(delay_df_mort, aes(x = mpi_label, y = delay, weight = deaths)) +
  geom_boxplot() +
  labs(title = "Delay by GM Category", x = "GM", y = "Delay (days)") +
  theme_minimal()

################################################################################

#-------------------------------- Fertility rate -------------------------------

################################################################################


# DATASET CONTAINING THE DEATHS PER YEAR, SEX, AGE AND MUNICIPALITY
births <- readRDS("../datasets/births.RDS")

# GROUP BY THE NEW MUNICIPALITIES
new_mun <- readRDS("../datasets/grouped_municipality_50000.RDS")
births_new_mun <- births |>
  left_join(new_mun |> dplyr::select(mun, group_id), by="mun") |>
  group_by(group_id, year, year_reg, sex, age) |>
  summarise(births = sum(births), .groups = "drop")

# CALCOLATE THE DELAY
delay_df <- births_new_mun |>
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
ggplot(births_prop_occ, aes(x = factor(year),
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



## Proportion -- DELAY IN THE PAST
ggplot(births_prop_reg, aes(x = factor(year_reg),
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


# JOIN WITH THE MPI
marg_index <- readRDS(file = "../datasets/marg_index.RDS")

marg_index <- marg_index |>
  mutate(
    marginalization_quintile = ntile(-IMN, 5),  
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
ggplot(delay_df, aes(x = delay, fill = sex, weight = deaths)) +
  geom_bar(position = "dodge") +
  labs(title = "Delay Distribution by Sex", x = "Delay (days)", y = "Deaths") +
  theme_minimal()

# INTERACTION WITH AGE
ggplot(delay_df, aes(x = delay, weight = deaths)) +
  geom_bar(fill = "#69b3a2") +
  facet_wrap(~ age) +
  labs(title = "Delay Distribution by Age Group", x = "Delay (days)", y = "Deaths") +
  theme_minimal()

# INTERACTION WITH MPI
ggplot(delay_df, aes(x = IMN, y = delay, size = deaths)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Delay vs Marginalization Index", x = "MPI", y = "Delay (days)") +
  theme_minimal()

ggplot(delay_df, aes(x = mpi_label, y = delay, weight = deaths)) +
  geom_boxplot() +
  labs(title = "Delay by GM Category", x = "GM", y = "Delay (days)") +
  theme_minimal()



