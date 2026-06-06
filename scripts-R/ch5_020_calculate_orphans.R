# =============================================================================
# ch5_020_calculate_orphans.R  ·  Chapter 5 — Orphanhood estimation
# Compute expected surviving children per adult and raw orphaning events (band-first approach).
# Reads input-data-processed/{geo_info,population_new_mun,fert,deaths,mort_df}.RDS (prefix g).
# =============================================================================

# ──────────────────────────────────────────────────────────────────────────────
#  Orphanhood Pipeline – band‑first, ≤ 79 y  (rev‑4)
#  ------------------------------------------------
#  ‣ Adult age bands top out at 75‑79 (nothing ≥80 enters the pipeline).
#  ‣ **FIX:** `to_band()` now handles `NA`, <15, and ≥80 gracefully so the
#    `sapply()` call inside `C_band_df` can never throw the
#    “missing value where TRUE/FALSE needed” error.
#  ‣ All other logic unchanged from rev‑3.
#
#  Date: 2025‑06‑04
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(tibble); library(data.table); library(sf); library(ggplot2)
})

# -----------------------------------------------------------------------------
#  Helper – single age → 5‑year band label  (returns NA if <15 or ≥80 or NA)
# -----------------------------------------------------------------------------
to_band <- function(a) {
  if (is.na(a) || a < 15 || a >= 80) return(NA_character_)
  lo <- floor(a / 5) * 5
  hi <- lo + 4
  sprintf("%02d-%02d", lo, hi)
}

# =============================================================================
#  1.  Read data
# =============================================================================


g <- "input-data-processed/"
geo_info      <- readRDS(paste0(g, "geo_info.RDS"))
population_df <- readRDS(paste0(g, "population_new_mun.RDS"))
fr_raw        <- readRDS(paste0(g, "fert.RDS"))
deaths_raw    <- readRDS(paste0(g, "deaths.RDS"))
mort_df       <- readRDS(paste0(g, "mort_df.RDS"))

# =============================================================================
#  2.  Parent population  (bands ≤79)
# =============================================================================

population_band <- population_df %>%
  filter(str_detect(age, "\\d+-\\d+")) %>%
  mutate(upper = as.numeric(str_extract(age, "\\d+$"))) %>%
  filter(upper <= 79) %>%
  select(group_id, year, sex, parent_band = age, parent_pop = population)

# =============================================================================
#  3.  Fertility – births / parent_pop in same band
# =============================================================================

# ── Fertility table with the parent population kept as `parent_pop` ───────────
fr_df <- fr_raw %>%                        # single-age births file(s)
  filter(year >= 1990) %>%
  select(group_id, year, sex, age, births = tot_births) %>%
  left_join(
    population_df %>%                      # band-level population
      select(group_id, year, sex, age, parent_pop = population),
    by = c("group_id", "year", "sex", "age")
  ) %>%
  group_by(group_id, year, sex, age) %>%   # age is the 5-year band
  summarise(
    births       = sum(births, na.rm = TRUE),
    parent_pop   = sum(parent_pop, na.rm = TRUE),
    fertility_rate = births / parent_pop,
    .groups = "drop"
  )

## --- TEST THE CORRECTNESS OF THE DATA ----------------------------------------
fr_plot <- fr_df %>%                        
  group_by(age, sex) %>%
  summarise(
    mean_fr = weighted.mean(fertility_rate, w = parent_pop, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(fr_plot, aes(age, mean_fr, colour = sex, group = sex)) +
  geom_line(size = 1.2) + geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "Population-weighted fertility rate by parent age band (1990-2023)",
       y = "Births per adult", x = "Parent age band")



# =============================================================================
#  4.  Child survival probabilities (0–17, single age)
# =============================================================================

population_children <- population_df %>%
  mutate(age_start = as.numeric(str_extract(age, "^\\d+")),
         age_end   = as.numeric(str_extract(age, "\\d+$"))) %>%
  rowwise() %>% mutate(age = list(seq(age_start, age_end))) %>%
  ungroup() %>% unnest(age) %>%
  filter(age <= 17) %>%
  mutate(population = round(population / (age_end - age_start + 1))) %>%
  select(group_id, year, sex, age, population)

survival_df <- mort_df %>%
  mutate(age = as.numeric(age)) %>%
  left_join(geo_info %>% select(mun, group_id), by = "mun") %>%
  group_by(group_id, year, sex, age) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  left_join(population_children,
            by = c("group_id", "year", "sex", "age")) %>%
  mutate(hazard_1yr = deaths / population,
         survival_prob = 1 - hazard_1yr) %>%
  group_by(group_id, year, age) %>%
  summarise(survival_prob = weighted.mean(survival_prob,
                                          w = population, na.rm = TRUE),
            .groups = "drop")


# =============================================================================
#  5.  Age‑band lookup (15‑79)
# =============================================================================

age_band_df <- tibble(age_lower = seq(15, 75, 5)) %>%
  mutate(age_upper = age_lower + 4,
         parent_band = paste0(age_lower, "-", age_upper),
         midpoint_age = (age_lower + age_upper) / 2)

# =============================================================================
#  6.  Expected surviving children per parent  (C_band_df)
# =============================================================================

child_ages      <- 0:17
fertility_years <- range(fr_df$year,      na.rm = TRUE)
survival_years  <- range(survival_df$year, na.rm = TRUE)
survival_ages   <- range(survival_df$age,  na.rm = TRUE)

# helper: single age → 5-year band, returns NA for <15 or ≥80
to_band <- function(a) {
  if (is.na(a) || a < 15 || a >= 80) return(NA_character_)
  lo <- floor(a/5)*5
  hi <- lo + 4
  sprintf("%02d-%02d", lo, hi)
}

C_band_df <- fr_df %>%
  rename(parent_pop_curr = parent_pop) %>%
  crossing(child_age = 0:17) %>%
  mutate(
    # Calculate birth year of each child
    birth_year = year - child_age,
    
    # Survival is for age + 1 because child must survive a full year
    survival_age = child_age + 1,
    
    # Robust extraction of midpoint age from band string like "15-19"
    midpoint_curr = (
      as.numeric(str_extract(age, "^\\d+")) +
        as.numeric(str_extract(age, "\\d+$"))
    ) / 2,
    
    # Age of parent at child's birth
    parent_age_at_birth = midpoint_curr - child_age,
    
    # Convert parent age at birth to band
    birth_parent_band = sapply(parent_age_at_birth, to_band)
  ) %>%
  filter(!is.na(birth_parent_band)) %>%
  
  # Join fertility rate for birth year and band
  left_join(
    fr_df %>%
      select(
        birth_year = year,
        birth_parent_band = age,
        sex,
        group_id,
        fr_birth = fertility_rate
      ),
    by = c("birth_year", "birth_parent_band", "sex", "group_id")
  ) %>%
  
  # Join child survival probability
  left_join(
    survival_df,
    by = c("group_id", "birth_year" = "year", "survival_age" = "age")
  ) %>%
  
  # Calculate raw expected children (unweighted)
  mutate(expected_children_raw = fr_birth * survival_prob) %>%
  
  # Collapse to weighted mean by current parent population
  group_by(year, age, sex, child_age, group_id) %>%
  summarise(
    expected_children = weighted.mean(
      expected_children_raw,
      w = parent_pop_curr,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

summary(C_band_df$expected_children)
any(is.na(C_band_df$expected_children))

#### CHECK-------------------
# merge in the current-year parent population so we can weight
diag_children <- C_band_df %>%
  left_join(
    fr_df %>% select(year, age, sex, group_id, parent_pop),
    by = c("year", "age", "sex", "group_id")
  ) %>%
  group_by(year, sex, age) %>%
  summarise(
    children_per_adult =
      weighted.mean(expected_children, w = parent_pop, na.rm = TRUE),
    .groups = "drop"
  )

diag_children <- C_band_df %>% 
  left_join(
    fr_df %>% select(year, age, sex, group_id, parent_pop),
    by = c("year", "age", "sex", "group_id")
  ) %>%
  group_by(year, sex, age) %>%               # <- child_age *not* in the grouping
  summarise(children_per_adult =
              weighted.mean(expected_children, 
                            w = parent_pop, na.rm = TRUE),
            .groups = "drop")


latest_year <- max(diag_children$year)

ggplot(diag_children %>% filter(year == latest_year),
       aes(age, children_per_adult, colour = sex, group = sex)) +
  geom_line(size = 1.1) + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = paste("Expected surviving children per adult –", latest_year),
       y = "children / adult", x = "Parent age band") +
  theme_minimal()



# =============================================================================
#  7.  Parental deaths (≤79)
# =============================================================================

deaths_df <- deaths_raw %>%
  filter(year >= 1990) %>%
  left_join(geo_info %>% select(mun, group_id), by = "mun") %>%
  group_by(group_id, year, sex, age) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  mutate(upper = as.numeric(str_extract(age, "\\d+$"))) %>%
  filter(upper <= 79) %>%
  rename(parent_band = age)

# =============================================================================
#  8.  Unadjusted orphan events (O_death)
# =============================================================================

O_death_df <- C_band_df %>%
  inner_join(deaths_df, by = c("year", "age" = "parent_band", "sex", "group_id")) %>%
  left_join(age_band_df, by = c("age" = "parent_band")) %>%
  mutate(O_death = expected_children * deaths)

# =============================================================================
#  9.  Adult 5‑year hazard (≤79) – population‑weighted
# =============================================================================

deaths_single <- readRDS(paste0(g, "deaths_df_long.RDS")) %>%
  left_join(geo_info %>% select(mun, group_id), by = "mun") %>%
  filter(age < 80)

deaths_single <- deaths_single %>%
  group_by(group_id, year, sex, age) %>%
  summarise(deaths = sum(deaths), .groups = "drop")

population_single <- population_df %>%
  mutate(age_start = as.numeric(str_extract(age, "^\\d+")),
         age_end   = as.numeric(str_extract(age, "\\d+$"))) %>%
  rowwise() %>% mutate(age = list(seq(age_start, age_end))) %>%
  ungroup() %>% unnest(age) %>%
  filter(age < 80) %>%
  mutate(population = round(population / (age_end - age_start + 1))) %>%
  select(group_id, year, sex, age, population)

mortality_df <- deaths_single %>%
  left_join(population_single, by = c("group_id", "year", "sex", "age"))

hazard5yr_df <- mortality_df %>%
  mutate(mx = deaths / population,
         hazard_5yr = 1 - exp(-5 * mx)) %>%
  select(group_id, year, sex, age, hazard_5yr)

to_band <- function(a) {
  # vectorised with ifelse(), works for scalars or whole columns
  ifelse(is.na(a) | a < 15 | a >= 80,
         NA_character_,
         sprintf("%02d-%02d",
                 floor(a/5) * 5,
                 floor(a/5) * 5 + 4))
}

haz_weighted <- hazard5yr_df %>%
  left_join(population_single, by = c("group_id", "year", "sex", "age")) %>%
  mutate(band = to_band(age)) %>%
  filter(!is.na(band)) %>%
  group_by(group_id, year, sex, band) %>%
  summarise(hazard_band = weighted.mean(hazard_5yr, w = population, na.rm = TRUE),
            .groups = "drop") %>%
  rename(age = band)

# =============================================================================
# 10.  Double‑orphan (O_double) and prior‑orphan (O_prev)
# =============================================================================

O_death_dt <- as.data.table(O_death_df)
setDT(haz_weighted)

O_death_dt[ , opposite_sex := ifelse(sex == "male", "female", "male")]
setkey(O_death_dt, year, age, opposite_sex, group_id)
setkey(haz_weighted, year, age, sex,          group_id)

O_death_dt <- haz_weighted[O_death_dt,
                           on = .(year, age, sex = opposite_sex, group_id)]
O_death_dt[is.na(hazard_band), hazard_band := 0]
O_death_dt[ , O_double := hazard_band * expected_children * deaths]

# ---- prior‑orphan expansion --------------------------------------------------
O_death_dt[ , row_id := .I]
setDT(age_band_df)           
setkey(age_band_df, parent_band)

aux <- O_death_dt[child_age > 0,
                  .(row_id, year, child_age, midpoint_age,
                    sex, group_id,
                    expected_children, deaths)]
aux <- aux[ , .(i = 1:child_age), by = .(row_id, year, child_age, midpoint_age,
                                         sex, group_id, expected_children, deaths)]
aux[ , `:=`(year_lookup = year - i,
            age_lookup  = midpoint_age - i)]
aux <- aux[age_lookup >= 15]
aux[ , age_band_lookup := sapply(age_lookup, to_band)]

setkey(aux, year_lookup, age_band_lookup, sex, group_id)
setkey(haz_weighted, year, age, sex, group_id)
aux <- haz_weighted[aux,
                    on = .(year = year_lookup, age = age_band_lookup, sex, group_id)]
aux[is.na(hazard_band), hazard_band := 0]
aux[ , contrib := hazard_band * expected_children * deaths]

O_prev_dt <- aux[ , .(O_prev = sum(contrib)), by = row_id]

O_death_dt <- merge(O_death_dt, O_prev_dt, by = "row_id", all.x = TRUE)
O_death_dt[is.na(O_prev), O_prev := 0]

# =============================================================================
# 11.  New orphan incidence (O_new)
# =============================================================================

O_new_df <- O_death_dt %>%
  as_tibble() %>%
  group_by(year, child_age, sex, group_id) %>%
  summarise(sum_death  = sum(O_death),
            sum_double = sum(O_double),
            sum_prev   = sum(O_prev), .groups = "drop") %>%
  mutate(O_new = sum_death - 0.5 * sum_double - sum_prev)

# =============================================================================
# 12.  Lifetime prevalence (O_lifetime) – identical logic, new age math
# =============================================================================

O_new_dt <- as.data.table(O_new_df)

# child hazard for survival multiplier ---------------------------------------
child_hazard_dt <- mort_df %>%
  left_join(geo_info %>% select(mun, group_id), by = "mun") %>%
  group_by(group_id, year, sex, age) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  left_join(population_children, by = c("group_id", "year", "sex", "age")) %>%
  mutate(hazard_1yr = deaths / population) %>%
  select(group_id, year, age, sex, hazard_1yr) %>%
  as.data.table()
setkey(child_hazard_dt, group_id, year, age, sex)

child_ages <- 0:17
all_years  <- sort(unique(O_new_dt$year))
all_sex    <- unique(O_new_dt$sex)
all_groups <- unique(O_new_dt$group_id)

grid <- CJ(year = all_years,
           child_age = child_ages,
           sex = all_sex,
           group_id = all_groups,
           i = seq_len(max(child_ages)))

grid[ , `:=`(year_lookup = year - (i - 1),
             age_lookup  = child_age - (i - 1))]

grid <- grid[age_lookup >= 0 & year_lookup >= min(O_new_dt$year)]

setkey(O_new_dt, year, child_age, sex, group_id)
grid <- O_new_dt[grid,
                 on = .(year = year_lookup, child_age = age_lookup, sex, group_id)]
grid[is.na(O_new), O_new := 0]

grid[ , step_id := .I]

surv_paths <- grid[i > 1,
                   .(step_id, group_id, sex,
                     j = 1:(i - 1),
                     year_surv = year - j,
                     age_surv  = child_age - j)]

setkey(surv_paths, group_id, year_surv, age_surv, sex)
surv_paths <- child_hazard_dt[surv_paths,
                              on = .(group_id, year = year_surv, age = age_surv, sex)]

surv_paths[is.na(hazard_1yr), hazard_1yr := 0]
surv_paths[ , surv_step := 1 - hazard_1yr]

mult_dt <- surv_paths[ , .(surv_mult = prod(surv_step)), by = step_id]

grid <- merge(grid, mult_dt, by = "step_id", all.x = TRUE)
grid[is.na(surv_mult), surv_mult := 1]

grid[ , contrib := O_new * surv_mult]

prevalence_df <- grid[ , .(O_lifetime = sum(contrib)),
                       by = .(year, child_age, sex, group_id)]

# =============================================================================
# 13.  Quick diagnostic – expected children per adult (latest year)
# =============================================================================

if (interactive()) {
  diag_children <- C_band_df %>%
    filter(year == max(year)) %>%
    group_by(age, sex) %>%
    summarise(children_per_adult = mean(expected_children, na.rm = TRUE),
              .groups = "drop")
  ggplot(diag_children, aes(age, children_per_adult, colour = sex)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(title = "Diagnostic – expected children per adult", y = "children / adult") +
    theme_minimal()
}

# ──────────────────────────────────────────────────────────────────────────────
#  End of file
# ──────────────────────────────────────────────────────────────────────────────
