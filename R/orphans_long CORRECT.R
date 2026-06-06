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
setwd("/Users/elsafarinella/Desktop/Orphanhood Mexico/R code")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(tibble); library(data.table); library(sf); library(ggplot2)
})

source("utils.R")


# =============================================================================
#  1.  Read data
# =============================================================================

g <- "../datasets/"

geo_info      <- readRDS(paste0(g, "geo_info.RDS"))
population_df <- readRDS(paste0(g, "population_new_mun.RDS"))
fr_raw        <- readRDS(paste0(g, "birth_data_all.RDS"))
deaths_raw    <- readRDS(paste0(g, "deaths.RDS"))
mort_df       <- readRDS(paste0(g, "mort_df.RDS"))
fertility_df <- readRDS(paste0(g, "births_long.RDS"))

# =============================================================================
#  2.  Parent population long
# =============================================================================

population_df_long <- population_df |>
  mutate(
    age_band  = str_replace_all(age, "[^0-9]+", "-"), 
    age_start = as.numeric(str_extract(age_band, "^\\d+")),
    age_end   = as.numeric(str_extract(age_band, "\\d+$"))
  )|>
  filter(age_end >= 15) |>
  rowwise() |>
  mutate(age = list(seq(age_start, age_end))) |>
  ungroup() |>
  unnest(age) |> 
  filter(age >= 15) |>
  mutate(
    population = round(population / (age_end - age_start + 1))
  ) |>
  select(group_id, year, sex, age, population) %>% 
  arrange(group_id, year, sex, age) %>% 
  filter(
    (sex == "female" & age <= 50) |
    (sex == "male"   & age <= 60)
  )

# =============================================================================
#  3.  Fertility – births / parent_pop in same band
# =============================================================================

fertility_df <- fertility_df |>
  left_join(geo_info |> select(mun, group_id), by ="mun")|>
  group_by(group_id, year, sex, age) |>
  summarise(births = sum(births), .groups = "drop") |>
  filter(age < 80) |>
  left_join(population_df_long, by=c("group_id", "sex", "age", "year")) |>
  mutate(fertility_rate = births/population) |>
  drop_na()


## --- TEST THE CORRECTNESS OF THE DATA ----------------------------------------
fr_plot <- fertility_df %>%                        
  group_by(age, sex) %>%
  summarise(
    mean_fr = weighted.mean(fertility_rate, w = population, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(fr_plot, aes(age, mean_fr, colour = sex, group = sex)) +
  geom_line(linewidth = 1.2) + geom_point() +
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
#  5.  Expected ration of actual children per parent  (C_df)
# =============================================================================
saveRDS(fertility_df, "../datasets/fertility_df.RDS")
saveRDS(survival_df, "../datasets/survival_df.RDS")

fertility_df <- readRDS("../datasets/fertility_df.RDS")
survival_df <- readRDS("../datasets/survival_df.RDS")

library(data.table)

# --- Step 0: Ensure correct types ---
setDT(fertility_df)
setDT(survival_df)

fertility_df[, `:=`(
  year = as.integer(year),
  age = as.integer(age)
)]

survival_df[, `:=`(
  year = as.integer(year),
  age = as.integer(age)
)]

# --- Step 1: Expand across child ages 0–17 ---
C_band_dt <- fertility_df[
  , .(child_age = 0:17),
  by = .(year, age, sex, group_id, fertility_rate, population)
][
  , `:=`(
    birth_year          = year - child_age,
    survival_age        = child_age + 1,
    parent_age_at_birth = age - child_age,
    parent_pop_curr     = population
  )
][
  # keep only biologically valid parents *AND* birth-years
  parent_age_at_birth >= 15 &              # as before
    birth_year >= 1990 & birth_year <= 2023  # where fertility data exist
]


# --- Step 2: Join fertility rate for birth_year & parent's age at birth ---
fertility_lookup <- fertility_df[
  , .(birth_year = year, parent_age_at_birth = age, sex, group_id, fertility_rate)
]

# Use data.table join syntax to update column safely
C_band_dt[
  fertility_lookup,
  on = .(birth_year, parent_age_at_birth, sex, group_id),
  fertility_rate := i.fertility_rate
]

# --- Step 3: Join survival probability ---
# Prepare survival_df keys
survival_df[
  , `:=`(
    birth_year = year,
    survival_age = age
  )
]

# Filter to only valid (birth_year, survival_age) combos in survival_df
valid_survival_keys <- unique(survival_df[, .(group_id, birth_year, survival_age)])
C_band_dt <- C_band_dt[
  valid_survival_keys, 
  on = .(group_id, birth_year, survival_age), 
  nomatch = 0
]

# Join survival probabilities
C_band_dt[
  survival_df, 
  on = .(group_id, birth_year, survival_age), 
  survival_prob := i.survival_prob
]

# --- Step 4: Calculate expected surviving children ---
C_band_dt[
  , expected_children_raw := fertility_rate * survival_prob
]

# Define your banding function
to_band <- function(a) {
  if (is.na(a) || a < 15 || a >= 80) return(NA_character_)
  lo <- floor(a / 5) * 5
  hi <- lo + 4
  sprintf("%02d-%02d", lo, hi)
}

# Vectorize it for efficiency
to_band_vec <- Vectorize(to_band)

# Apply banding to parent age
C_band_dt[, parent_age_band := to_band_vec(parent_age_at_birth)]

# Drop rows where banding failed (e.g. age < 15 or >= 80)
C_band_dt <- C_band_dt[!is.na(parent_age_band)]

# Simple arithmetic mean by band  
band_summary <- C_band_dt[
  , .(
    expected_children = mean(expected_children_raw, na.rm = TRUE)
  ),
  by = .(year, sex, group_id, parent_age_band, child_age)
]


# =============================================================================
#  6.  Parental deaths (≤79)
# =============================================================================

deaths_df <- deaths_raw %>%
  filter(year >= 1990) %>%
  left_join(geo_info %>% select(mun, group_id), by = "mun") %>%
  group_by(group_id, year, sex, age) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  mutate(upper = as.numeric(str_extract(age, "\\d+$"))) %>%
  filter(upper > 15, upper <= 60) %>%
  rename(parent_band = age)

# =============================================================================
#  7.  Unadjusted orphan events (O_death)
# =============================================================================

O_death_df <- band_summary %>%
  inner_join(deaths_df, by = c("year", "parent_age_band" = "parent_band", "sex", "group_id")) %>%
  mutate(O_death = expected_children * deaths)

# =============================================================================
#  8.  Adult 5‑year hazard (≤79) – population‑weighted
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
# 9.  Double‑orphan (O_double) and prior‑orphan (O_prev)
# =============================================================================

O_death_dt <- as.data.table(O_death_df)
setDT(haz_weighted)
haz_weighted <- as.data.table(haz_weighted)

O_death_dt[, opposite_sex := ifelse(sex == "male", "female", "male")]

haz_weighted <- unique(haz_weighted,
                       by = c("group_id", "year", "age", "sex"))

setkey(haz_weighted, group_id, year, age, sex)
setkey(O_death_dt,  group_id, year, parent_age_band, opposite_sex)

O_death_dt <- O_death_dt[              #  ❰ X ❱  (kept)
  haz_weighted,                        #  ❰ i ❱  (adds hazard)
  on = .(group_id,
         year,
         parent_age_band = age,
         opposite_sex   = sex),
  nomatch = 0
]

# rename and tidy
setnames(O_death_dt, "hazard_band", "hazard_opp")



## set missing hazards to zero
O_death_dt[is.na(hazard_opp), hazard_opp := 0]

## double-orphan term
O_death_dt[, O_double := hazard_opp * expected_children * deaths]

# =============================================================================
# =============================================================================
# =============================================================================
# =============================================================================

library(data.table)

# ------------------------------------------------------------------
#  (6)  Prior-orphan term  O_prev  – exact year-by-year accumulation
# ------------------------------------------------------------------
O_death_dt[, row_id := .I]                              # unique key

# ------------------------------------------------------------------
#  Add midpoint of the parent 5-year band  (e.g. "30-34" → 32)
# ------------------------------------------------------------------
O_death_dt[, midpoint_age :=
             as.numeric(sub("^(\\d+)-.*$", "\\1", parent_age_band)) + 2]

setDT(hazard5yr_df)
hazard5yr_df[, hazard_1yr := 1 - (1 - hazard_5yr)^(1/5)]   # ← moved up

prev_expanded <- O_death_dt[child_age > 1, .(
  i = seq_len(child_age - 1)
), by = .(row_id, group_id, year, midpoint_age,
          opp_sex = opposite_sex, expected_children, deaths)]

prev_expanded[, `:=`(
  target_year = year - i,
  target_age  = midpoint_age - i
)]

setkey(hazard5yr_df, group_id, year, age, sex)
prev_expanded <- hazard5yr_df[
  prev_expanded,
  on = .(group_id,
         year = target_year,
         age  = target_age,
         sex  = opp_sex)
]

prev_expanded[is.na(hazard_1yr), hazard_1yr := 0]

prev_expanded[, contrib := hazard_1yr * expected_children * deaths]

## sum across all earlier years and add back
prior_sums   <- prev_expanded[, .(O_prev = sum(contrib)), by = row_id]
O_death_dt   <- prior_sums[O_death_dt, on = .(row_id)]
O_death_dt[is.na(O_prev), O_prev := 0]


# ------------------------------------------------------------------
#  (7)  New orphan events  O_new
# ------------------------------------------------------------------
O_new_dt <- O_death_dt[
  , .(
    O_death  = sum(expected_children * deaths),   # mothers + fathers
    O_double = sum(O_double),                     # both parents same year
    O_prev   = sum(O_prev)                        # other parent earlier
  ),
  by = .(group_id, year, child_age, parent_age_band)
][
  , O_new := O_death - 0.5 * O_double - O_prev][]   # formulas 7a–7c

# You may collapse parent_age_band now if you only need child_age totals
O_new_tot <- O_new_dt[, .(O_new = sum(O_new)), by = .(group_id, year, child_age)]

# ------------------------------------------------------------------
#  (9)  Sex-specific incidence  (needed only if you care about mothers vs fathers)
#       O_new_sex  =  O_death_parent – O_prev_parent
# ------------------------------------------------------------------
O_new_sex <- O_death_dt[
  , .(
    O_new_sex = sum(expected_children * deaths - O_prev)
  ),
  by = .(group_id, year, sex, child_age)
]


# ------------------------------------------------------------------
#  Helper: national child-survival lookup  (1 – 1h) in equation 8
# ------------------------------------------------------------------
library(data.table)

## make sure both tables are data.tables
setDT(survival_df)
setDT(population_children)   # you created this earlier

## bring children’s population onto every (group_id, year, age) row
child_surv <- merge(
  survival_df,                                           # LHS
  population_children[, .(group_id, year, age, population)],  # RHS
  by = c("group_id", "year", "age"),
  all.x = TRUE
)[
  , .(surv = weighted.mean(survival_prob,
                           w = population,
                           na.rm = TRUE)),
  by = .(year, age)
]

setkey(child_surv, year, age)

# ------------------------------------------------------------------
#  (8)  Lifetime prevalence by single age 0–17   O_lifetime_age
# ------------------------------------------------------------------
O_new_nat <- O_new_tot[, .(O_new = sum(O_new)), by = .(year, child_age)]
setkey(O_new_nat, year, child_age)

years <- sort(unique(O_new_nat$year))

O_lifetime <- O_new_nat[
  , {
    y  <- year
    b  <- child_age
    S  <- 1          # survival product
    prev <- 0
    for (i in 0:b) {
      yy <- y - i
      bb <- b - i
      onew <- O_new_nat[.(yy, bb), O_new, nomatch = 0]
      if (!is.na(onew)) {
        prev <- prev + S * onew
      }
      # update survival product for next loop (skip i == b)
      if (i < b) {
        s_j <- child_surv[.(yy, bb), surv, nomatch = 0]
        S   <- S * ifelse(is.na(s_j), 1, s_j)
      }
    }
    .(Olifetime = prev)
  },
  by = .(year, child_age)
]

# ------------------------------------------------------------------
# (10)  Lifetime prevalence by parent sex (optional)
# ------------------------------------------------------------------
O_new_sex_nat <- O_new_sex[, .(O_new = sum(O_new_sex)),
                           by = .(year, sex, child_age)]
setkey(O_new_sex_nat, year, sex, child_age)

O_lifetime_sex <- O_new_sex_nat[
  , {
    y  <- year
    b  <- child_age
    s  <- sex
    S  <- 1
    prev <- 0
    for (i in 0:b) {
      yy <- y - i; bb <- b - i
      onew <- O_new_sex_nat[.(yy, s, bb), O_new, nomatch = 0]
      if (!is.na(onew)) prev <- prev + S * onew
      if (i < b) {
        s_j <- child_surv[.(yy, bb), surv, nomatch = 0]
        S   <- S * ifelse(is.na(s_j), 1, s_j)
      }
    }
    .(Olifetime = prev)
  },
  by = .(year, sex, child_age)
]

# ------------------------------------------------------------------
# (11)  Lifetime prevalence aggregated over child ages 0–17
# ------------------------------------------------------------------
O_lifetime_total <- O_lifetime[, .(Olifetime = sum(Olifetime)),
                               by = year][order(year)]

# The data.tables O_new_tot, O_lifetime, O_lifetime_sex, O_lifetime_total
# correspond to formulas (7), (8), (10) and (11) respectively.

O_new_tot        <- O_new_tot[year >= 2005]
O_lifetime       <- O_lifetime[year >= 2005]
O_lifetime_sex   <- O_lifetime_sex[year >= 2005]
O_lifetime_total <- O_lifetime_total[year >= 2005]


# ──────────────────────────────────────────────────────────────
#  Quick visual checks
# ──────────────────────────────────────────────────────────────
library(ggplot2)
library(scales)
library(patchwork)

# ------------ 1. Annual incidence (O_new_tot) -----------------
p_incidence <- ggplot(O_new_tot, aes(year, O_new)) +
  geom_col(fill = "#0072B2", width = 0.6) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Incidence of Orphanhood (new cases)",
       y = "New orphans", x = NULL) +
  theme_minimal()

print(p_incidence)

inc_plot <- O_new_tot |>
  group_by(year) |>
  summarise(incidence = sum(O_new, na.rm = T), .groups = "drop")

ggplot(inc_plot, aes(year, incidence)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(50000, NA),                                   # ← starts at 0
    labels  = label_number(scale_cut = cut_short_scale())
  ) +
  labs(title = "Incidence of Orphanhood (new cases)",
       y = "New orphans", x = NULL) +
  theme_minimal()


# ------------ 2. Lifetime prevalence (total) ------------------
p_prev_tot <- ggplot(O_lifetime_total, aes(year, Olifetime)) +
  geom_line(linewidth = 1.2, colour = "#D55E00") +
  geom_point(size = 2, colour = "#D55E00") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Lifetime Prevalence of Orphanhood (ages 0–17)",
       y = "Children currently orphaned", x = NULL) +
  theme_minimal()

print(p_prev_tot)
# ------------ 3. Prevalence by child age (latest year) --------
latest_year <- max(O_lifetime$year)
p_prev_age <- O_lifetime[year == latest_year] |>
  ggplot(aes(child_age, Olifetime)) +
  geom_col(fill = "#1F77B4") +
  scale_x_continuous(breaks = 0:17) +
  labs(title = paste("Prevalence by Child Age –", latest_year),
       x = "Child age", y = "Children currently orphaned") +
  theme_minimal()

print(p_prev_age)
# ------------ 4. Lifetime prevalence by parent sex ------------
p_prev_sex <- ggplot(O_lifetime_sex, aes(year, Olifetime, colour = sex)) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_colour_manual(values = c(female = "#e41a1c", male = "#377eb8"),
                      labels = c(female = "Mother", male = "Father"),
                      name = "Deceased parent") +
  labs(title = "Lifetime Prevalence by Parent Sex",
       y = "Children currently orphaned", x = NULL) +
  theme_minimal()

print(p_prev_sex)

## 1️⃣  Collapse to one number per (year, sex)
prev_sex_year <- O_lifetime_sex |>
  group_by(year, sex) |>
  summarise(Olifetime = sum(Olifetime, na.rm = TRUE), .groups = "drop") |>
  mutate(
    sex = recode(sex,
                 female = "Mother",
                 male   = "Father"),
    sex = factor(sex, levels = c("Mother", "Father"))
  )

## 2️⃣  Plot
prev_sex_year <- prev_sex_year |>
  filter(year >=2015)
ggplot(prev_sex_year, aes(year, Olifetime, colour = sex)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Mother" = "#b2182b",
                                 "Father" = "#2166ac"),
                      name = "Deceased parent") +
  scale_y_continuous(
    labels  = label_number(scale_cut = cut_short_scale()),
    expand  = expansion(mult = c(0, 0.05)),
    limits = c(0, NA)
  ) +
  labs(
    title = "Lifetime Prevalence by Parent Sex",
    y     = "Children currently orphaned",
    x     = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.position = "right"
  )
# ------------ Display together --------------------------------
(p_incidence | p_prev_tot) / (p_prev_age | p_prev_sex)


O_death_dt[, .(O_prev = sum(O_prev), O_death = sum(O_death)), by = sex]


rate_df <- O_new_sex[
, .(orphans = sum(O_new_sex)), by = .(year, sex)
][
# total children with each parent alive
population_children[, .(children = sum(population)), by = year],
on = "year"
][
, rate := orphans / children
]


# ──────────────────────────────────────────────────────────────
#  Incidence rate of orphanhood  —  Father vs Mother
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(ggplot2)
library(scales)

## 1️⃣  Collapse new-orphan counts to one value per (year, sex)
inc_orphans <- O_new_sex |>
  filter(year >=2005) |>
  group_by(year, sex) |>
  summarise(orphans = sum(O_new_sex, na.rm = TRUE), .groups = "drop") |>
  mutate(sex = recode(sex, female = "Mother", male = "Father"))

## 2️⃣  Total number of children alive in each calendar year
pop_children_year <- population_children |>
  filter(year >=2005) |>
  group_by(year) |>
  summarise(children = sum(population, na.rm = TRUE), .groups = "drop")

## 3️⃣  Merge and compute rate (per 100 children)
rate_df <- inc_orphans |>
  left_join(pop_children_year, by = "year") |>
  mutate(rate_per_100 = orphans / children * 100)

## 4️⃣  Plot
ggplot(rate_df, aes(year, rate_per_100, colour = sex, group = sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Mother" = "#b2182b", "Father" = "#2166ac"),
                      name = "Deceased parent") +
  scale_y_continuous(
    limits = c(0, NA),                       # start at 0
    labels = label_number(accuracy = 0.01, suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Incidence Rate of Orphanhood by Parent Sex",
    y     = "New orphans per 100 children",
    x     = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.position = "right"
  )


# ──────────────────────────────────────────────────────────────
#  Incidence rate of orphanhood MAP
# ──────────────────────────────────────────────────────────────
aggregated_muni <- readRDS(file = "../datasets/aggregated_muni_50000.RDS")

map_data_incidence <- left_join(
  aggregated_muni,                      
  O_new_tot %>% 
    filter(year == 2023) %>%
    group_by(group_id) %>%
    summarise(incidence = sum(O_new, na.rm = TRUE), .groups = "drop"),
  by = "group_id"
)
library(ggplot2)
library(scales)
map_data_incidence <- map_data_incidence %>%
  mutate(incidence = ifelse(incidence <= 0, NA, incidence))

# Plot
inc_map <- ggplot(map_data_incidence) +
  geom_sf(aes(fill = incidence), colour = "white", linewidth = 0.1) +
  
  # ── new colour scale: low = green, mid = yellow, high = red ─────────
  scale_fill_gradientn(
    colours  = c("#126a38", "#ffff8c", "#b2182b"),
    trans    = "log",
    na.value = "grey90",
    name     = "(Log) Orphans",
    guide    = guide_colorbar(
      barheight     = unit(4, "cm"),
      barwidth      = unit(1, "cm"),
      ticks         = TRUE,
      title.position = "top",
      title.hjust    = 0.5),
    labels   = label_number(scale_cut = cut_short_scale())
  ) +
  # ───────────────────────────────────────────────────────────────────
  
  labs(
    title    = "Orphanhood Incidence by Municipality (2023)",
    subtitle = "Log-scaled number of orphans per region",
    fill     = "Orphans"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold",  size = 14),
    plot.subtitle   = element_text(size = 10)
  )

print(inc_map)

# ──────────────────────────────────────────────────────────────
#  Incidence rate per 1000 children MAP
# ──────────────────────────────────────────────────────────────
population_children_tot <- population_children |>
  filter(year==2023) |>
  group_by(group_id) |>
  summarize(n_children = sum(population), .groups = "drop")

map_data_rate <- O_new_tot |>
  filter(year == 2023) |> 
  group_by(group_id) |> 
  summarise(orphan_count = sum(O_new, na.rm = TRUE), .groups = "drop") |>
  left_join(
    aggregated_muni |> select(group_id, geometry), by = "group_id") |>   
  left_join(population_children_tot , by = "group_id") |>
  mutate(
    orphan_rate = (orphan_count / n_children) * 1000,  # per 1 000 children
    orphan_rate = ifelse(n_children <= 0, NA, orphan_rate)  # avoid ÷0
  )|>
  st_as_sf()         

# ── 2  Plot the rate ─────────────────────────────────────────────────
rate_map <- ggplot(map_data_rate) +
  geom_sf(aes(fill = orphan_rate), colour = "white", linewidth = 0.1) +
  scale_fill_gradientn(
    colours  = c("#126a38", "#ffff8c", "#b2182b"),   # low → high
    na.value = "grey90",
    name     = "Orphans per 1 000 children",
    guide    = guide_colorbar(
      barheight      = unit(4, "cm"),
      barwidth       = unit(1, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    labels   = label_number(accuracy = 0.01)
    # If the distribution is very skewed, uncomment the next line:
    # , trans = "log10"
  ) +
  labs(
    title    = "Orphanhood Rate by Municipality (2023)",
    subtitle = "Number of orphans per 1 000 resident children",
    fill     = "Rate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10)
  )

print(rate_map)


# ──────────────────────────────────────────────────────────────
#  MPI MAP
# ──────────────────────────────────────────────────────────────
mpi <- readRDS("../datasets/index_new_mun.RDS")
mpi <- mpi |> 
  filter(year == 2020) |>
  left_join(
    aggregated_muni |> 
      select(group_id, geometry),
    by = "group_id"
  )

mpi <- sf::st_as_sf(mpi)

map_mpi <- ggplot(mpi) +
  geom_sf(aes(fill = IMN), colour = "white", linewidth = 0.1) +
  
  # ── red → yellow → green scale ───────────────────────────────
  scale_fill_gradient2(
    low       = "#b2182b",         
    mid       = "#ffff8c",          
    high      = "#126a38",          
    midpoint  = 0.85,  # centre the yellow
    na.value  = "grey90",
    name      = "MPI",
    guide     = guide_colorbar(
      barheight     = unit(4, "cm"),
      barwidth      = unit(1, "cm"),
      ticks         = TRUE,
      title.position = "top",
      title.hjust    = 0.5)
  ) +
  # ─────────────────────────────────────────────────────────────
  
  labs(
    title    = "Poverty Index",
    subtitle = "Value of IMN",
    fill     = "MPI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10)
  )


print(map_mpi)

# ──────────────────────────────────────────────────────────────
#  Lineplot standardized fertility rate in 2023
# ──────────────────────────────────────────────────────────────
fert_df_year <- fertility_df |>
  rename(fert_rate = fertility_rate) |>
  filter(year>= 2010)

geo_info_new_mun <- readRDS("../datasets/geo_info_new_mun.RDS")
geo_info_new_mun <- st_drop_geometry(geo_info_new_mun)
mpi <- st_drop_geometry(mpi) |>
  select(group_id, year, IMN) |>
  distinct()

std_raw <- compute_std_fert_rate(fert_df_year);
std_raw_year <- std_raw %>% 
  left_join(y = geo_info_new_mun |> dplyr::select(group_id, capital), by = "group_id") %>% 
  left_join(y = mpi, by = c("group_id", "year")) %>% 
  mutate(
    capital = as.character(capital),
    capital = replace_na(capital, "0"),
    capital = factor(capital)
  )

std_raw_year <- std_raw |>
  group_by(year, sex) |>
  summarise(fertility_rate = mean(std_fert_rate), .groups = "drop")
  


library(ggplot2)
library(scales)

ggplot(std_raw_year, aes(year, fertility_rate, group = sex, color = sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.01),   # 2-dp e.g. 0.07
    limits = c(0, NA),                        # start at zero
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average Standardized Fertility Rate, 2010-2023",
    x     = "Years",
    y     = "Std Fertility Rate"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  )

# ──────────────────────────────────────────────────────────────
#  Standardized fertility rate MAP
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(scales)

# ❶  Join geometry (assuming `new_mun_50000` is your muni shapefile)
std_raw_map <- std_raw |>
  filter(year==2023) |>
  group_by(group_id) |>
  summarise(fertility_rate = mean(std_fert_rate), .groups = "drop")

fert_map <- std_raw_map |>
  left_join(
    aggregated_muni |>
      select(group_id, geometry),
    by = "group_id"
  ) |>
  st_as_sf()                          # ensure it's an sf object

# ❷  Draw the map
fert_map <- ggplot(fert_map) +
  geom_sf(aes(fill = fertility_rate), colour = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low       = "#b2182b",          # deep red   (low MPI)
    mid       = "#ffff8c",          # bright yellow (mid)
    high      = "#126a38",        # rich green (high MPI)
    midpoint  = mean(fert_map$fertility_rate, na.rm = TRUE),  # centre the yellow
    na.value  = "grey90",
    name      = "Standardized fertility rate",
    guide     = guide_colorbar(
      barheight     = unit(4, "cm"),
      barwidth      = unit(1, "cm"),
      ticks         = TRUE,
      title.position = "top",
      title.hjust    = 0.5)
  ) +
  labs(
    title    = "Mean Standardized Fertility Rate by Municipality in 2023",
    fill     = "Fertility"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10)
  )

# ──────────────────────────────────────────────────────────────
#  Lineplot standardized mortality rate in 2023
# ──────────────────────────────────────────────────────────────
deaths <- readRDS("../datasets/deaths_new_mun.RDS")
to_exclude <- c(as.character(1:15), "80-84")

deaths <- deaths[!deaths$age %in% to_exclude, ]

mort_df_year <- deaths |>
  filter(year>= 2010) |>
  select(group_id, year, sex, age, tot_deaths) |>
  left_join(population_df, by = c("group_id", "year", "sex", "age")) |>
  group_by(group_id, year, sex, age) |>
  summarise(deaths = sum(tot_deaths), population = sum(population), .groups = "drop")|>
  mutate(mort_rate = deaths/population)

std_raw_mort <- compute_std_mort_rate(mort_df_year);

std_raw_mort_year <- std_raw_mort %>% 
  left_join(y = geo_info_new_mun |> dplyr::select(group_id, capital), by = "group_id") %>% 
  left_join(y = mpi, by = c("group_id", "year")) %>% 
  mutate(
    capital = as.character(capital),
    capital = replace_na(capital, "0"),
    capital = factor(capital)
  )

std_raw_mort_year <- std_raw_mort_year |>
  group_by(year, sex) |>
  summarise(mortality_rate = mean(std_mort_rate), .groups = "drop")



library(ggplot2)
library(scales)

ggplot(std_raw_mort_year, aes(year, mortality_rate, group = sex, color = sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.001),   # 2-dp e.g. 0.07
    limits = c(0, NA),                        # start at zero
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average Standardized Mortality Rate, 2010-2023",
    x     = "Years",
    y     = "Std Mortality Rate"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  )

# ──────────────────────────────────────────────────────────────
#  Standardized mortality rate map in 2023
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(scales)
library(patchwork)

std_mort_map <- std_raw_mort |>
  filter(year==2023) |>
  group_by(group_id) |>
  summarise(mortality_rate = mean(std_mort_rate), .groups = "drop")

mort_map <- std_mort_map |>
  left_join(
    aggregated_muni |>
      select(group_id, geometry),
    by = "group_id"
  ) |>
  st_as_sf()                          

# ❷  Draw the map
mort_map <- ggplot(mort_map) +
  geom_sf(aes(fill = mortality_rate), colour = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low       = "#126a38",          # deep red   (low MPI)
    mid       = "#ffff8c",          # bright yellow (mid)
    high      = "#b2182b",        
    midpoint  = mean(mort_map$mortality_rate, na.rm = TRUE),  # centre the yellow
    na.value  = "grey90",
    name      = "Standardized mortality rate",
    guide     = guide_colorbar(
      barheight     = unit(4, "cm"),
      barwidth      = unit(1, "cm"),
      ticks         = TRUE,
      title.position = "top",
      title.hjust    = 0.5)
  ) +
  labs(
    title    = "Mean Standardized Mortality Rate by Municipality in 2023",
    fill     = "Mortality"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10)
  )

print(mort_map)
map_mpi | inc_map
(map_mpi | inc_map) / (fert_map | mort_map)
(map_mpi | rate_map) / (fert_map | mort_map)


# ──────────────────────────────────────────────────────────────
#  Orphanhood prevalence in 2023
# ──────────────────────────────────────────────────────────────
preval_plot <- O_lifetime_total |>
  filter(year >=2010)

ggplot(preval_plot, aes(year, Olifetime)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Prevalence, 2010-2023",
    x     = "Years",
    y     = "Prevalence"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  )

# ──────────────────────────────────────────────────────────────
#  Orphanhood prevalence in 2023
# ──────────────────────────────────────────────────────────────
orphans_by_year_sex <- O_lifetime_sex %>%
  group_by(year, sex) %>%
  summarise(total_orphans = sum(Olifetime, na.rm = TRUE), .groups = "drop")

pop_children_year <- pop_children_year |>
  filter(year >= 2007)

orphans_rate <- orphans_by_year_sex %>%
  left_join(pop_children_year, by = "year") %>%
  mutate(rate_per_100 = (total_orphans / children) * 100) |>
  filter(year >= 2007)


ggplot(orphans_rate, aes(x = year, y = rate_per_100, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Orphanhood Prevalence Rate per 100 Children",
    x = "Year",
    y = "Rate per 100 Children",
    color = "Deceased Parent"
  ) +
  scale_y_continuous(
    limits = c(0, 3),       # Force y-axis to start at 0
    expand = c(0, 0)         # Remove padding below axis
  ) +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 0.7), 
        panel.grid = element_blank())

# ──────────────────────────────────────────────────────────────
#  Orphanhood prevalence in 2023
# ──────────────────────────────────────────────────────────────
prevalence_dataset <- O_lifetime_sex %>%
  mutate(age_group = case_when(
    child_age >= 0  & child_age <= 4  ~ "0-4",
    child_age >= 5  & child_age <= 9  ~ "5-9",
    child_age >= 10 & child_age <= 17 ~ "10-17",
    TRUE ~ NA_character_
  ))

pop_children_band <- population_children %>%
  mutate(age_group = case_when(
    age >= 0  & age <= 4  ~ "0-4",
    age >= 5  & age <= 9  ~ "5-9",
    age >= 10 & age <= 17 ~ "10-17",
    TRUE ~ NA_character_
  ))

orphans_by_year_age <- prevalence_dataset %>%
  group_by(year, age_group) %>%
  summarise(total_orphans = sum(Olifetime, na.rm = TRUE), .groups = "drop")

population_by_year_age <- pop_children_band %>%
  group_by(year, age_group) %>%
  summarise(total_children = sum(population, na.rm = TRUE), .groups = "drop")

orphan_rate_by_age <- orphans_by_year_age %>%
  left_join(population_by_year_age, by = c("year", "age_group")) %>%
  mutate(rate_per_100 = (total_orphans / total_children) * 100)

orphan_rate_by_age <- orphan_rate_by_age %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-9", "10-17")))|>
  filter(year >= 2007)

ggplot(orphan_rate_by_age, aes(x = year, y = rate_per_100, color = age_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2)+ 
  scale_color_manual(
    values = c("0-4" = "#a8dbc5", "5-9" = "#5ea77a", "10-17" = "#33673b"),
    name = "Age of child",
    labels = c("0–4 years", "5–9 years", "10–17 years"))+
  labs(
    title = "Orphanhood Prevalence Rate per 100 Children by Age Group",
    x = "Year",
    y = "Rate per 100 Children",
    color = "Age Group"
  )+
  scale_y_continuous(
    limits = c(0, 5),       # Force y-axis to start at 0
    expand = c(0, 0)         # Remove padding below axis
  ) +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 0.7), 
        panel.grid = element_blank())


# ──────────────────────────────────────────────────────────────
#  dot plots, one dot for each location (ie your merged municipalities). y-axis orphanhood incidence rate, x-axis changing eg poverty, mortality, cumulative mortality, etc. Also report correlation coefficients

# ──────────────────────────────────────────────────────────────
library(readr)
library(dplyr)

# ── 1. Read in the component data ─────────────────────────────────
# 'merged_municipalities.csv' already holds MPI and orphan metrics
orphan_incidence <- O_new_sex |>
  filter(year >= 2010) |>
  group_by(group_id, sex, year) |>
  summarise(orphans_incidence = sum(O_new_sex, na.rm = T), .groups = "drop")

df <- mpi |>
  left_join(orphan_incidence, by= c("year", "group_id")) 

# Deaths and population by location–year–age (single or 5-year bands)
grouped_municipality_50000 <- readRDS("../datasets/grouped_municipality_50000.RDS")
grouped_municipality_50000 <- st_drop_geometry(grouped_municipality_50000)
deaths_long <- readRDS("../datasets/deaths_df_long.RDS") |>
  filter(year >=2000) |>
  left_join(grouped_municipality_50000 |> select(group_id, mun), by = "mun")|>
  group_by(group_id, year, age, sex) |>
  summarise(deaths = sum(deaths, na.rm = T), .groups = "drop")

pop    <- population_df_long |>
  filter(year >= 2000) 

# ── 2. Choose the parenting-age range and period ──────────────────
age_lo <- 15
age_hi <- 49
start_year <- 2023 - 18      # 2005
end_year   <- 2023

# ── 3. Annual mortality rates for 15-49, by location & year ───────
parent_mort <- deaths_long %>%
  filter(year >= start_year, year <= end_year,
         age >= age_lo, age <= age_hi) %>%
  group_by(group_id, year) %>%
  summarise(D_parent = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    pop %>%
      filter(year >= start_year, year <= end_year,
             age >= age_lo, age <= age_hi) %>%
      group_by(group_id, year) %>%
      summarise(P_parent = sum(population, na.rm = TRUE), .groups = "drop"),
    by = c("group_id", "year")
  ) %>%
  mutate(rate_parent = D_parent / P_parent)

# ── 4. Collapse across years to get the cumulative rate ───────────
cum_parent_mort <- parent_mort %>%
  group_by(group_id) %>%
  summarise(cum_mort_18yr = sum(rate_parent, na.rm = TRUE), .groups = "drop")

# ── 5. Get the most recent single-year adult mortality rate ───────
latest_year <- end_year      # 2023, or pick another “current” year
adult_mort_latest <- parent_mort %>%
  filter(year == latest_year) %>%
  select(group_id, mort_15_49_latest = rate_parent)

# ── 6. Merge all predictors back into your main frame ─────────────
df <- df %>%
  left_join(cum_parent_mort,      by = "group_id") %>%
  left_join(adult_mort_latest,    by = "group_id")


# ── 7. Quick check ────────────────────────────────────────────────
df %>%
  select(group_id, IMN, mort_15_49_latest, cum_mort_18yr) %>%
  glimpse()


## ── 8. Dot plots & correlations ───────────────────────────────────
library(ggplot2)
library(glue)
library(patchwork)   # install.packages("patchwork") if needed

# 1. Variables to plot
x_vars <- c("IMN", "mort_15_49_latest", "cum_mort_18yr")
y_var  <- "orphans_incidence"

# 2. A softer point colour
nice_blue <- "#2C7BB6"

# 3. Build one plot per predictor
plots <- lapply(x_vars, function(x) {
  test <- cor.test(df[[x]], df[[y_var]], use = "pairwise.complete.obs")
  r    <- test$estimate
  p    <- test$p.value
  
  ggplot(df, aes(.data[[x]], .data[[y_var]])) +
    geom_point(colour = nice_blue, alpha = 0.6, size = 2) +
    labs(
      x = gsub("_", " ", x),
      y = "Orphan incidence",
      title    = glue("{y_var} vs {x}"),
      subtitle = glue("r = {round(r, 2)},  p = {format.pval(p, digits = 3)}")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey85"),
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(colour = "grey40")
    )
})

# 4. Display the plots side-by-side (adjust ncol if you prefer)
wrap_plots(plots, ncol = 2)

# 5. (Optional) print a tidy correlation table
corr_tbl <- do.call(rbind, lapply(x_vars, function(x) {
  test <- cor.test(df[[x]], df[[y_var]], use = "pairwise.complete.obs")
  data.frame(
    x_var     = x,
    pearson_r = round(test$estimate, 3),
    p_value   = signif(test$p.value, 3)
  )
}))
print(corr_tbl)













# ---- 0) Packages --------------------------------------------------------------
library(sf)
library(dplyr)
library(janitor)
library(Hmisc)      # rcorr() for Spearman matrix with p-values
library(spdep)      # neighbors, weights, Moran's I, LISA
library(ggplot2)
library(readr)

# ---- 1) Load municipalities (replace with your file) --------------------------
# Example: a GeoPackage or Shapefile/GeoJSON. Keep everything in one layer.
aggregated_muni <- readRDS(file = "../datasets/aggregated_muni_50000.RDS")

# Read your RDS
x <- readRDS(file = "../datasets/aggregated_muni_50000.RDS")

# ------- CASE A: geometry column is already an sfc (list-column) -------------
# e.g., class(x$geometry) includes "sfc"
if ("geometry" %in% names(x) && inherits(x$geometry, "sfc")) {
  sfx <- st_as_sf(x, sf_column_name = "geometry")
}

muni <- sfx |>  # <-- change path
  st_make_valid() |>
  st_transform(4326) |>                         # projection not critical for contiguity
  clean_names()

# ---- 2) Make sure your key variables exist (rename here if needed) -----------
# orphanhood rate per 1,000 resident children
# standardized fertility rate, standardized mortality rate, poverty index
vars <- c(
  orphan = "orphan_rate",
  fert   = "std_fert_rate",
  mort   = "std_mort_rate",
  pov    = "poverty_index"   # e.g., MPI or similar, direction noted elsewhere
)

# Check presence
stopifnot(all(unlist(vars) %in% names(muni)))

# Keep a clean data frame for correlation work (no geometry)
dat <- muni |>
  st_drop_geometry() |>
  select(any_of(unlist(vars))) |>
  mutate(across(everything(), as.numeric))

# ---- 3) Non-parametric municipal associations (Spearman) ---------------------
# Pairwise Spearman correlations with p-values
cor_res <- Hmisc::rcorr(as.matrix(dat), type = "spearman")

# Tidy correlation matrix and p-values
corr_tbl <- as_tibble(as.table(cor_res$r), .name_repair = "minimal") |>
  rename(var1 = Var1, var2 = Var2, rho = n) |>
  left_join(
    as_tibble(as.table(cor_res$P), .name_repair = "minimal") |>
      rename(var1 = Var1, var2 = Var2, p_value = n),
    by = c("var1", "var2")
  ) |>
  mutate(p_adj_BH = p.adjust(p_value, method = "BH"))

# Save if desired
# write_csv(corr_tbl, "spearman_correlations_municipal.csv")

# ---- 4) Spatial weights: contiguity neighbors (queen) ------------------------
# Build neighbors; allow islands (zero-policy later)
nb <- spdep::poly2nb(muni, queen = TRUE)
lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

# Quick diagnostics
# table(card(nb))  # neighbor counts

# ---- 5) Global Moran's I for each variable -----------------------------------
globals <- lapply(unlist(vars), function(v) {
  x <- muni[[v]]
  x <- as.numeric(x)
  # Parametric test
  mt  <- spdep::moran.test(x, lw, zero.policy = TRUE, alternative = "two.sided")
  # Permutation test (recommended)
  set.seed(123)
  mmc <- spdep::moran.mc(x, lw, nsim = 999, zero.policy = TRUE, alternative = "two.sided")
  tibble(
    variable = v,
    moran_i  = unname(mt$estimate[["Moran I statistic"]]),
    exp_i    = unname(mt$estimate[["Expectation"]]),
    var_i    = unname(mt$estimate[["Variance"]]),
    p_param  = mt$p.value,
    i_perm   = unname(mmc$statistic),
    p_perm   = mmc$p.value
  )
}) |> bind_rows()

# Save if desired
# write_csv(globals, "global_morans_i.csv")

# ---- 6) Local Moran's I (LISA) for orphanhood rate ---------------------------
x <- muni[[vars["orphan"]]] |> as.numeric()
z <- scale(x)[, 1]  # standardize
wz <- spdep::lag.listw(lw, z, zero.policy = TRUE)

# Local Moran stats (Anselin)
loc <- spdep::localmoran(x = z, listw = lw, zero.policy = TRUE)
colnames(loc) <- c("Ii", "Ei", "Vi", "Zi", "p_value")

muni$lisa_I      <- loc[, "Ii"]
muni$lisa_p      <- loc[, "p_value"]
muni$lisa_p_adj  <- p.adjust(muni$lisa_p, method = "BH")
muni$z_std       <- z
muni$wz_std      <- as.numeric(wz)

# LISA cluster classification (at BH-adjusted p < 0.05)
sig <- muni$lisa_p_adj < 0.05
quad <- ifelse( sig & muni$z_std >= 0 & muni$wz_std >= 0, "High–High",
                ifelse( sig & muni$z_std <= 0 & muni$wz_std <= 0, "Low–Low",
                        ifelse( sig & muni$z_std >= 0 & muni$wz_std <= 0, "High–Low",
                                ifelse( sig & muni$z_std <= 0 & muni$wz_std >= 0, "Low–High",
                                        "Not significant"))))

muni$lisa_cluster <- factor(
  quad,
  levels = c("High–High","Low–Low","High–Low","Low–High","Not significant")
)

# Save local results if desired
# st_write(muni, "lisa_orphan_rate.gpkg", delete_dsn = TRUE)

# ---- 7) LISA map (orphanhood rate) -------------------------------------------
ggplot(muni) +
  geom_sf(aes(fill = lisa_cluster), linewidth = 0.1, color = "grey70") +
  scale_fill_manual(values = c(
    "High–High"      = "#b2182b",
    "Low–Low"        = "#2166ac",
    "High–Low"       = "#ef8a62",
    "Low–High"       = "#67a9cf",
    "Not significant"= "grey85"
  )) +
  labs(title = "Local Moran's I (LISA) — Orphanhood rate per 1,000 children, 2023",
       fill  = "Cluster",
       caption = "Contiguity weights (queen). p-values BH-adjusted at 5%.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

# ---- 8) (Optional) Moran scatterplot for orphanhood rate ---------------------
df_ms <- muni |>
  st_drop_geometry() |>
  transmute(z = z_std, wz = wz_std, sig = muni$lisa_p_adj < 0.05)

ggplot(df_ms, aes(x = z, y = wz, color = sig)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_point(alpha = 0.7, size = 1.4) +
  scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey70"),
                     labels = c("Not sig.", "BH p<0.05")) +
  labs(title = "Moran scatterplot — Orphanhood rate (standardized)",
       x = "Standardized orphanhood rate (z)",
       y = "Spatial lag (Wz)",
       color = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

