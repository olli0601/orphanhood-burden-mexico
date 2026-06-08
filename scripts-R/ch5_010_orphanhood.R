# =============================================================================
# ch5_010_orphanhood.R  ·  Chapter 5 — Orphanhood estimation
# Minimal orphanhood calculation from mortality and fertility (prototype of the full engine).
# Reads input-data-processed/{mort,fert}.RDS.
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)

# --- 1. Load Data ---
# Assume these CSVs have columns:
# deaths: year, age, sex, deaths
# fertility: year, age, sex, fertility_rate

deaths_df <- readRDS("input-data-processed/mort_by_grouped_mun.RDS")        # age: 15-94
fertility_df <- readRDS("input-data-processed/fert_by_grouped_mun.RDS")

# --- 2. Define Parameters ---
max_child_age <- 17
min_parent_age <- 15
max_year <- max(deaths_df$year)
min_year <- min(deaths_df$year)

# --- 3. Estimate Orphanhood Incidence ---
incidence_df <- bind_rows(lapply(0:max_child_age, function(b) {
  year_list <- (min_year + b):max_year
  bind_rows(lapply(year_list, function(y) {
    child_birth_year <- y - b
    
    df <- deaths_df %>%
      filter(year == y) %>%
      mutate(parent_age_at_birth = age - b,
             fertility_year = child_birth_year) %>%
      left_join(fertility_df, 
                by = c("fertility_year" = "year", 
                       "parent_age_at_birth" = "age", 
                       "sex" = "sex")) %>%
      mutate(expected_orphans = fertility_rate * tot_deaths,
             year = y,
             child_age = b) %>%
      select(year, child_age, expected_orphans)
  }))
}))

# Aggregate total orphans per year and child age
incidence_summary <- incidence_df %>%
  group_by(year, child_age) %>%
  summarise(orphans = sum(expected_orphans, na.rm = TRUE), .groups = "drop")

# --- 4. Estimate Orphanhood Prevalence ---
prevalence_df <- bind_rows(lapply(unique(incidence_summary$year), function(y) {
  bind_rows(lapply(0:max_child_age, function(b) {
    prev <- sum(incidence_summary %>%
                  filter(year <= y, child_age <= b, year >= y - b) %>%
                  pull(orphans), na.rm = TRUE)
    data.frame(year = y, child_age = b, prevalence = prev)
  }))
}))
