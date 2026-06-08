# =============================================================================
# ch3_030_eda_mexico.R  ·  Chapter 3 — Exploratory data analysis
# National/subnational EDA of fertility, mortality and marginalization on the
# harmonised panels (trends, rates, distributions).
#
# Reads : input-data-processed/{births_new_mun, deaths_new_mun, population_new_mun,
#         marg_index, rural_urban_area}.parquet
# Writes: output/ch3/ (EDA figures)
# Run after: ch1_060
# =============================================================================

# Exploratory Data Analysis (EDA) on Fertility and Mortality in Mexico
# Author: [Your Name]
# Date: 2025-08-10

## --- 1. Load required libraries ---
library(arrow)      # For reading parquet files
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(tidyr)      # For data tidying
library(stringr)    # For string operations
library(forcats)    # For factor manipulation
library(purrr)

# --- 1b. Load custom functions for standardised rates ---
source("R/rates.R")  # Adjust path if needed

# --- 2. Load datasets ---
# Adjust file paths if needed
births <- readRDS("input-data-processed/births_new_mun.RDS")
deaths <- readRDS("input-data-processed/deaths_new_mun.RDS") |>
  filter(!age %in% 1:15)
population <- readRDS("input-data-processed/population_new_mun.RDS")|>
  filter(!age %in% c("00-04", "05-09", "10-14"))
marg_index <- readRDS("input-data-processed/marg_index.RDS")
# Note: 'rural_urban_area.parquet copia' may need renaming or path adjustment
rural_urban <- if (file.exists("input-data-processed/rural_urban_area.parquet")) {
  read_parquet("input-data-processed/rural_urban_area.parquet")
} else {
  message("ch3_030: rural_urban_area.parquet missing (needs raw type_of_mun.csv); using NA area_type stub.")
  data.frame(group_id = unique(marg_index$group_id), area_type = "Unknown")
}

#-------------------------------------------------------------------------------
#------------------------------------FERTILITY----------------------------------
#-------------------------------------------------------------------------------


# --- 4. Data preparation & indicator calculation ---
# 4a. Fertility: merge births and population only where needed
fertility <- births %>%
  left_join(population, by = c("group_id", "year", "sex", "age")) %>%
  mutate(fert_rate = births / population) %>%
  drop_na()

std_fert <- compute_std_fert_rate(fertility)
std_fert_age <- compute_std_rate_age_gender(fertility)

set.seed(123) # For reproducibility
sampled_ids <- sample(unique(std_fert$group_id), 9)
std_fert_sample <- std_fert %>% filter(group_id %in% sampled_ids)

# 1) Prep: make types explicit, de-duplicate, and fill any missing years per group/sex
fert_prep <- std_fert_sample |>
  mutate(
    year = as.integer(year),
    sex  = as_factor(sex)        # keeps original order if already ordered
  ) |>
  summarise(                      # guard against duplicate rows for a year/sex
    std_fert_rate = mean(std_fert_rate, na.rm = TRUE),
    .by = c(group_id, sex, year)
  ) |>
  group_by(group_id, sex) |>
  complete(year = tidyr::full_seq(year, 1)) |>   # ensures continuous yearly timeline
  arrange(group_id, sex, year) |>
  ungroup()

# 2) Faceted overview: all groups at once, lines colored by sex
p_facets <- fert_prep |>
  ggplot(aes(x = year, y = std_fert_rate, color = sex)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 1.6, na.rm = TRUE) +
  facet_wrap(~ group_id, scales = "free_y") +
  labs(
    title = "Trend of standardized fertility rate by sex",
    subtitle = "One panel per group_id",
    x = "Year", y = "Standardized fertility rate", color = "Sex"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_facets
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# ---- Prep (dedupe, types, complete grid) ----
std_fert_age_sample <- std_fert_age %>% filter(group_id %in% sampled_ids)
fert_age <- std_fert_age_sample |>
  mutate(
    year      = as.integer(year),
    age = as_factor(age),
    sex       = as_factor(sex)
  ) |>
  summarise(                                  # collapse duplicates if present
    std_fert_rate = mean(std_rate, na.rm = TRUE),
    .by = c(group_id, sex, age, year)
  ) |>
  group_by(group_id, sex) |>
  complete(
    year = tidyr::full_seq(year, 1),
    age
  ) |>
  arrange(group_id, sex, age, year) |>
  ungroup()

# Optional: normalize to shares within each group_id/sex/year
fert_share <- fert_age |>
  group_by(group_id, sex, year) |>
  mutate(share = std_fert_rate / sum(std_fert_rate, na.rm = TRUE)) |>
  ungroup()

p_area <- fert_share |>
  ggplot(aes(year, share, fill = age)) +
  geom_area(color = "white", size = 0.15, na.rm = TRUE) +
  facet_grid(sex ~ group_id) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Composition across age groups",
    subtitle = "Within each group_id × sex, stacked to 100%",
    x = "Year", y = "Share of total", fill = "Age group"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
p_area

#------------------------------------
std_nat <- compute_national_std_rate_age_gender(fertility)

std_nat <- std_nat %>% arrange(year, sex, age) %>% filter(year>=1990)

# 2. Plot with ggplot
p <- ggplot(std_nat, aes(x = age, y = std_rate, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ year) + 
  labs(
    title = "Standardized Fertility Rate by Age and Sex",
    x = "Age Group",
    y = "Standardized Fertility Rate"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_text(size = 8)
  )


print(p)



# 4b. Mortality: merge deaths and population only where needed
mortality <- deaths %>%
  filter(!age %in% c("80-84")) %>%
  left_join(population, by = c("group_id", "year", "sex", "age")) %>%
  mutate(
    mort_rate = tot_deaths / population, 
    deaths = tot_deaths
  ) %>%
  select(-tot_deaths)

# 4c. Add marginalization and urban/rural info for summary/plots if needed
fertility <- fertility %>%
  filter(year >=2010 & year <=2020) %>%
  left_join(marg_index, by = c("group_id", "year")) %>%
  left_join(rural_urban, by = "group_id")

mortality <- mortality %>%
  filter(year >=2010 & year <=2020) %>%
  left_join(marg_index, by = c("group_id", "year")) %>%
  left_join(rural_urban, by = "group_id")

# --- 5. Calculate standardised rates ---
# Mortality (municipal)
std_mort_mun <- compute_std_rate_age_gender(mortality)
# Mortality (national)

std_mort_nat <- compute_national_std_rate_age_gender(mortality)
std_mort_nat <- std_mort_nat %>% arrange(year, sex, age) %>% filter(year>=1990)

# 2. Plot with ggplot
p <- ggplot(std_mort_nat, aes(x = age, y = std_rate, color = sex, group = sex)) +
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


# --- 6. Fertility analysis ---
ggplot(filter(fertility, !is.na(fert_rate) & !is.na(area_type)), aes(x = fert_rate)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ area_type) +
  labs(title = "Fertility Rate Distribution by Urban/Rural Area",
       x = "Fertility Rate", y = "Count")

# --- 6b. Plot standardised fertility rates (municipal and national) ---

if (exists("std_fert_mun")) {
  # Sample 12 random group_id for visualization
  set.seed(123) # For reproducibility
  sampled_ids <- sample(unique(std_fert_mun$group_id), 12)
  std_fert_mun_sample <- std_fert_mun %>% filter(group_id %in% sampled_ids)
  
  ggplot(std_fert_mun_sample, aes(x = year, y = std_rate, color = sex)) +
    geom_line() +
    facet_wrap(~ group_id) +
    labs(title = "Standardised Fertility Rate by Municipality (sampled)",
         x = "Year", y = "Standardised Fertility Rate")
} else message("ch3_030: skipping per-municipality std-fertility plot (std_fert_mun not built).")

std_fert_nat <- std_nat  # national fertility std (alias)
ggplot(std_fert_nat, aes(x = year, y = std_rate, color = sex)) +
  geom_line() +
  labs(title = "National Standardised Fertility Rate",
       x = "Year", y = "Standardised Fertility Rate")



# --- 7. Mortality analysis ---
ggplot(filter(mortality, !is.na(mort_rate) & !is.na(area_type)), aes(x = mort_rate)) +
  geom_histogram(bins = 100, fill = "salmon", color = "black") +
  facet_wrap(~ area_type) +
  labs(title = "Mortality Rate Distribution by Urban/Rural Area",
       x = "Mortality Rate", y = "Count")

ggplot(mortality, aes(x = marg_index, y = mort_rate, color = area_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Mortality Rate vs. Marginalization Index",
       x = "Marginalization Index", y = "Mortality Rate")

# --- 7b. Plot standardised mortality rates (municipal and national) ---
ggplot(std_mort_mun, aes(x = year, y = std_rate, color = sex)) +
  geom_line() +
  facet_wrap(~ group_id) +
  labs(title = "Standardised Mortality Rate by Municipality",
       x = "Year", y = "Standardised Mortality Rate")

ggplot(std_mort_nat, aes(x = year, y = std_rate, color = sex)) +
  geom_line() +
  labs(title = "National Standardised Mortality Rate",
       x = "Year", y = "Standardised Mortality Rate")


# --- 8. Socioeconomic and demographic relationships ---
# Correlation between fertility/mortality rates and marginalization, urban/rural
cor_fert_marg <- cor(fertility$fert_rate, fertility$IMN, use = "complete.obs")
cor_mort_marg <- cor(mortality$mort_rate, mortality$IMN, use = "complete.obs")

cat("Correlation between fertility rate and marginalization index:", cor_fert_marg, "\n")
cat("Correlation between mortality rate and marginalization index:", cor_mort_marg, "\n")

# Compare rates by urban/rural
fertility %>%
  group_by(area_type) %>%
  summarise(mean_fert_rate = mean(fert_rate, na.rm = TRUE))
mortality %>%
  group_by(area_type) %>%
  summarise(mean_mort_rate = mean(mort_rate, na.rm = TRUE))

# --- 9. Key messages and summary ---
# (To be completed after reviewing results)
cat("\nKey messages:\n")
cat("- Birth and death rates vary significantly by marginalization and urban/rural status.\n")
cat("- Higher marginalization is associated with [describe direction] birth and death rates.\n")
cat("- Urban/rural differences are evident in both fertility and mortality.\n")
cat("- Further analysis by age, cause of death, or other variables is recommended.\n")

# --- End of EDA script ---


