# =============================================================================
# ch1_040_clean_mort.R  ·  Chapter 1 — Mortality cleaning
# Clean and harmonise the per-year INEGI registered-death files into death
# counts by municipality x sex x 5-year age group x year; build the population
# and geographic-lookup tables used downstream.
#
# Loads the supplied per-year mortality (input-data-processed/mortality datasets/),
# joins population (R/load_population) + IMM_2020 + the ch2 grouping.
# Reads : input-data-processed/mortality datasets/*.RDS, grouped_municipality_50000.RDS (ch2),
#         input-data-raw/population/...1_Grupo_Quinq..., input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/{mort.RDS, population.RDS, geo_info.RDS}
# Run after: ch1_005, ch2_010
# =============================================================================

#############################################

#Data pre-processing

#############################################

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
source("R/preprocess_mortality.R"); source("R/rates.R"); source("R/plots.R")
source("R/load_population.R")
source("R/marginalization.R")
source("R/load_year_panels.R")

# Load the SUPPLIED per-year mortality datasets into mort_YYYY objects. These
# are the output of preprocess_mortality() on the raw INEGI files; loading them
# here lets the pipeline start from input-data-processed/ without the raw files.
# (To regenerate from raw instead, loop the INEGI files through
# preprocess_mortality() as in ch1_005 / the git history.)
# Run ch1_005_bootstrap_from_processed.R first.
mort_panel <- load_year_panels("mortality datasets")


## BARPLOT 
# Get all objects whose names start with "mort_"
all_mort <- mort_panel

# Summarize data: count occurrences of each 'year' within each 'year_reg'
data_summary <- all_mort %>%
  filter(year >= as.numeric(year_reg) - 20) %>%  # Keep only deaths in the last 20 years before registration
  group_by(year_reg, year) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year_reg) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ungroup()


library(RColorBrewer)
length(unique(data_summary$year))
color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(data_summary$year)))  # Generate 101 colors

# Plot the data with the custom color palette
p <- ggplot(data_summary, aes(x = factor(year_reg), y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3) +
  labs(x = "Registration Year",
       y = "Total Deaths Count",
       fill = "Death Year",
       title = "Registered Deaths by Year and Actual Death Year") +
  scale_fill_manual(values = color_palette) +  # Apply the generated color palette
  theme_minimal() +
  facet_wrap(~year_reg, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.box.spacing = unit(0.5, "cm"))

print(p)


# Convert the data to a proportion for pie charts
data_summary_pie <- data_summary %>%
  group_by(year_reg) %>%
  mutate(perc = round(count / sum(count) * 100, 2))

data_summary_pie <- data_summary_pie %>%
  group_by(year_reg) %>%
  mutate(is_max = perc == max(perc)) %>%
  ungroup()

p_pie <- ggplot(data_summary_pie, aes(x = "", y = perc, fill = factor(year))) +
  geom_bar(stat = "identity", width = 0.5, color = "white", size = 0.1) +
  geom_text(data = subset(data_summary_pie, is_max), 
            aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~year_reg) +
  theme_void() + 
  labs(title = "Death Year Distribution per Registration Year") +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  scale_fill_manual(values = rainbow(length(unique(data_summary_pie$year))))

print(p_pie)


# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
deaths <- mort_panel |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop")

deaths <- deaths |> filter(year >= 1990 & year < 2024)
grouped_municipality <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
deaths <- left_join(deaths, grouped_municipality |> select(state_name, mun_name, mun, group_id), by ="mun")

# --------- GROUP BY THE NEW MUNICIPALITIES ---------------
deaths <- deaths |>
  group_by(group_id, year, sex, age) |>
  mutate(tot_deaths = sum(deaths))

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
ggplot(deaths_summary, aes(x = factor(year_reg), y = log(total_deaths), fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of Registration",
    y = "Total Deaths",
    fill = "Delay (Years)",
    title = "Total Deaths by Occurrence Year and Reporting Year"
  ) +
  theme_minimal()

## Proportion -- DELAY IN THE FUTURE
ggplot(deaths_prop_occ, aes(x = factor(year), y = (prop_deaths), fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023))+
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of occurrence",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for Deaths by Year of Occurrence",
    subtitle = "Each bar shows the proportion of deaths by how many years later they were registered"
  ) +
  theme_minimal()


## Proportion -- DELAY IN THE PAST
ggplot(deaths_prop_reg, aes(x = factor(year_reg), y = prop_deaths, fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of registration",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for Deaths by Year of Registration",
    subtitle = "Each bar shows the proportion of registered deaths by how many years earlier the death occurred"
  ) +
  theme_minimal()
#------

ggplot(deaths_prop_reg, aes(x = factor(year_reg), y = prop_deaths, fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year of Occurrence",
    y = "Percentage of Deaths",
    fill = "Year Reported",
    title = "Proportion of Deaths by Occurrence Year and Reporting Delay"
  ) +
  theme_minimal()




##############################################

# Population
population <- load_population(keep_child = FALSE)

saveRDS(population, "input-data-processed/population.RDS")
mort <- full_join(deaths |> select(mun, group_id, year, sex, age, year_reg, deaths, tot_deaths), population, by = c("mun", "year", "sex", "age"))

mort$tot_deaths[is.na(mort$tot_deaths)] <- 0
mort$deaths[is.na(mort$deaths)] <- 0

#marginality index

index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)

index <- index[-1,]

#IM - marginalization index
#IMN - normalized marginalization index
# GM- degree of marginalization

im.tmp <- index |> dplyr:: select(mun, IM, IMN, illiterate_pct, no_basic_edu_pct, 
                                  no_drainage_pct, no_electricity_pct, no_piped_water_pct, 
                                  dirt_floors_pct, overcrowding_pct, small_towns_pct, low_income_pct)
im.tmp$mun <- as.factor(im.tmp$mun)
im.tmp$IM <- as.numeric(im.tmp$IM)
im.tmp$IMN <- as.numeric(im.tmp$IMN)

mort <- mort |> full_join(im.tmp, by = c("mun")) 

##some municipalities don't have IM information (??)
##some municipalities have population of 0

mort$mort_rate <- mort$deaths / mort$population

mort <- mort |> drop_na()


saveRDS(mort, file = "input-data-processed/mort.RDS")



geo_info <- build_geo_info(index)


saveRDS(geo_info, file = "input-data-processed/geo_info.RDS")

#----------------------- Standardized mortality rate ---------------------------
std_raw <- compute_std_rate(mort); std_raw <- std_raw %>% filter(year == 2023) %>% dplyr::select(-year)
std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = index, by = "mun") %>% mutate(capital = factor(capital))
std_raw$overcrowding_pct <- min_max_norm(std_raw$overcrowding_pct)
std_raw <- std_raw %>% rename("mpi"="IMN")
std_raw$mpi <- as.numeric(std_raw$mpi)

# ---------------------Plot mortality rate against mpi -------------------------
p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (mortality)")
print(p_raw_pts)

#------------------------ Iterate over the years -------------------------------
years <- seq(1990, 2023, 1)

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each year
for (yr in years) {
  std_raw <- compute_std_rate(mort); std_raw <- std_raw %>% filter(year == yr) %>% dplyr::select(-year)
  std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = index, by = "mun") %>% mutate(capital = factor(capital))
  #std_raw$overcrowding_pct <- min_max_norm(std_raw$overcrowding_pct)
  std_raw <- std_raw %>% rename("mpi"="IMN")
  std_raw$mpi <- as.numeric(std_raw$mpi)
  
  p_raw_pts <- plot_std_rate(data = std_raw, tt= paste("Raw data (mortality) - Year", yr))
  print(p_raw_pts)
}

# Arrange all the plots in a 3x6 grid (3 rows, 6 columns)
grid.arrange(grobs = plot_list, nrow = 3, ncol = 3)



std_raw_all <- compute_std_rate(mort) %>%
  filter(year %in% 2005:2023) %>% 
  left_join(geo_info[, c("mun", "capital")], by = "mun") %>% 
  left_join(index, by = "mun") %>% 
  mutate(capital = factor(capital)) %>%
  rename(mpi = IMN) %>% 
  mutate(mpi = as.numeric(mpi))


p_raw_pts <- plot_std_rate(data = std_raw_all, tt = "Raw data (mortality) - Years 2012 to 2023") +
  facet_wrap(~ year)

print(p_raw_pts)
#------------------------------------------------------------------------------
######
#First: group by year, sex and age
mort_national <- mort |> group_by(year, age, sex)|> summarise(deaths = sum(deaths), population = sum(population), .groups = "drop")
mort_national <- mort_national |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))
mort_national$mort_rate <- mort_national$deaths / mort_national$population

mort_national <- mort_national |> drop_na()


# Second: calculate the standardized deaths rate
std_age_sex <- compute_national_std_rate_age_gender(mort_national)
std_age_sex <- std_age_sex %>% filter(year == 2023)


#Plot 
ggplot(std_age_sex, aes(x = age, y = std_rate, color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_point()+
  facet_wrap(~ sex, nrow = 1) +
  labs(title = "Standardized Mortality Rate by Age Group",
       x = "Age Group",
       y = "Standardized Mortality Rate") +
  theme_minimal() 


std_age_sex <- std_age_sex %>% arrange(year, sex, age)

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





