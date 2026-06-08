# =============================================================================
# ch5_030_calculate_orphanhood.R  ·  Chapter 5 — Orphanhood estimation
# Full orphanhood incidence engine: combine fertility histories, child survival and parental deaths; adjust for double and prior orphanhood.
# Reads input-data-processed/{geo_info,population_grouped_mun,fert,births_long,deaths,mort_df,grouped_municipality_50000,index_grouped_mun,mort}.RDS.
# =============================================================================

# Load required packages
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tibble)
library(stringr)
library(purrr)

# =============================================================================
# DATA INPUTS AND REQUIRED COLUMNS
# =============================================================================
# 1. fertility_df: annual live births by parent age band and sex
#    - columns: year, parent_age_band (e.g. "20-24"), sex ("M"/"F"), births
# 2. population_df: mid-year population by parent age band and sex
#    - columns: year, parent_age_band, sex, population
# 3. survival_df: child survival probability from birth to age+1
#    - columns: year, age (0–17), survival_prob (0–1)
# 4. deaths_df: parental deaths by age band and sex
#    - columns: year, parent_age_band, sex, deaths
# 5. hazard5yr_df: 5-year mortality hazard by single age and sex
#    - columns: year, age, sex, hazard_5yr
# 6. child_hazard_df: 1-year child mortality hazard by age
#    - columns: year, age, hazard_1yr
# 7. age_band_df: mapping of band to exact ages and midpoint
#    - columns: parent_age_band, age_lower, age_upper, midpoint_age

# =============================================================================
# STEP 1: Compute fertility rate (FR) by year, parent_age_band, sex
# FR = births / population
# =============================================================================

#------------------------ POPULATION DATASET ------------------------------
geo_info <- readRDS("input-data-processed/geo_info.RDS")
population_df <- readRDS("input-data-processed/population_grouped_mun.RDS")

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
  arrange(group_id, year, sex, age)

#------------------------- FERTILITY DATASET ------------------------------
# ---------- PER AGE BAND GROUP
fr_df <- readRDS("input-data-processed/fert_by_grouped_mun.RDS") |> 
  filter(year >= 1990) |>
  select(group_id, year, sex, age, tot_births)

fr_df <- fr_df |>
  left_join(population_df, by= c("group_id", "year", "sex", "age"))

fr_df <- fr_df |>
  group_by(year, group_id, age, sex) |>
  summarise(tot_births = sum(tot_births), tot_population = sum(population), .groups = "drop") |>
  mutate(fertility_rate = tot_births / tot_population)|>
  filter(age < 80)
  
#--------- FOR EACH YEAR 
fertility_df <- readRDS("input-data-processed/births_long.RDS")

fertility_df <- fertility_df |>
  left_join(geo_info |> select(mun, group_id), by ="mun")|>
  group_by(group_id, year, sex, age) |>
  summarise(births = sum(births), .groups = "drop") |>
  filter(age < 80)

fertility_df <- fertility_df |>
  left_join(population_df_long, by=c("group_id", "sex", "age", "year")) |>
  mutate(fertility_rate = births/population)

#--------------------------- DEATHS DATASET ------------------------------
deaths_df <- readRDS("input-data-processed/deaths.RDS") |>
  filter(year >= 1990)

deaths_df <- deaths_df |>
  left_join(geo_info |> select(mun, group_id), by ="mun")|>
  group_by(group_id, year, sex, age) |>
  summarise(deaths = sum(deaths), .groups = "drop")

#------------------------- SURVIVAL DATASET ------------------------------
mort_df <- readRDS("input-data-processed/mort_df.RDS")

population_children <- population_df |>
  mutate(
    age = as.character(age),
    age_clean = str_replace_all(age, "[^0-9]+", "-")
  ) |>
  filter(age_clean %in% c("00-04", "05-09", "10-14", "15-19")) |>
  mutate(
    age_start = as.numeric(str_extract(age_clean, "^\\d+")),
    age_end   = as.numeric(str_extract(age_clean, "\\d+$"))
  ) |>
  rowwise() |>
  mutate(age = list(age_start:age_end)) |>
  ungroup() |>
  tidyr::unnest(age) |>
  filter(age <= 17) |>
  mutate(
    population = round(population / (age_end - age_start + 1), 0)
  ) |>
  select(group_id, year, sex, age, population)


# ──────────────────────────────────────────────────────────────
#  Child survival probabilities  (one row per group_id-year-age)
# ──────────────────────────────────────────────────────────────
survival_df <- mort_df |>
  mutate(age = as.integer(age)) |>                      # ← NEW LINE
  left_join(geo_info |> select(mun, group_id), by = "mun") |>
  group_by(group_id, year, sex, age) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  
  # attach sex-specific child population so we can weight later
  left_join(population_children,
            by = c("group_id", "year", "sex", "age")) |>
  
  mutate(
    hazard_1yr    = deaths / population,     # 1-year hazard
    survival_prob = 1 - hazard_1yr
  ) |>
  
  # collapse ♀ + ♂ into one weighted average
  group_by(group_id, year, age) |>
  summarise(
    survival_prob = weighted.mean(survival_prob,
                                  w = population,
                                  na.rm = TRUE),
    .groups = "drop"
  )


# =============================================================================
# STEP 2: Estimate expected surviving children (C) per parent
#   C_{y,a',s,b} = FR_{y-b,a',s} * p_survive_{y-b, b+1}
# Requires: fr_df, survival_df
# =============================================================================
# Define child age range
max_child_age <- 17
child_ages <- 0:max_child_age

# Get valid ranges from data
fertility_years <- range(fertility_df$year, na.rm = TRUE)
fertility_ages  <- range(fertility_df$age,  na.rm = TRUE)
survival_years  <- range(survival_df$year, na.rm = TRUE)
survival_ages   <- range(survival_df$age,  na.rm = TRUE)

# Construct the validated C_df
C_df <- fertility_df |>
  crossing(child_age = child_ages) |>
  mutate(
    birth_year = year - child_age,
    parent_age_at_birth = age - child_age,
    survival_age = child_age + 1
  ) |>
  # Filter out invalid ranges
  filter(
    birth_year >= fertility_years[1],
    birth_year <= fertility_years[2],
    parent_age_at_birth >= fertility_ages[1],
    parent_age_at_birth <= fertility_ages[2],
    survival_age >= survival_ages[1],
    survival_age <= survival_ages[2],
    birth_year >= survival_years[1],
    birth_year <= survival_years[2]
  ) |>
  left_join(
    fertility_df |>
      select(year, age, sex, group_id, fertility_rate) |>
      rename(
        birth_year = year,
        parent_age_at_birth = age,
        fr_birth = fertility_rate
      ),
    by = c("birth_year", "parent_age_at_birth", "sex", "group_id")
  ) |>
  left_join(
    survival_df,
    by = c("birth_year" = "year", "survival_age" = "age", "group_id")
  ) |>
  mutate(
    expected_children = fr_birth * survival_prob
  ) |>
  select(year, age, sex, child_age, expected_children, group_id)


# -------------------------- AGE BAND FOR THE PARENTS ----------------------
child_ages        <- 0:17
fertility_years   <- range(fr_df$year,      na.rm = TRUE)
survival_years    <- range(survival_df$year,            na.rm = TRUE)
survival_ages     <- range(survival_df$age,             na.rm = TRUE)

C_band_df <- fr_df %>%                         # parent strata
  crossing(child_age = child_ages) %>%                    # add child ages
  mutate(
    birth_year  = year - child_age,                      # y-b
    survival_age = child_age + 1                         # b+1
  ) %>%
  # exclude combos outside data ranges
  filter(
    birth_year  >= fertility_years[1],
    birth_year  <= fertility_years[2],
    birth_year  >= survival_years[1],
    birth_year  <= survival_years[2],
    survival_age >= survival_ages[1],
    survival_age <= survival_ages[2]
  ) %>%
  # look up fertility rate for *same parent band* at the child's birth year
  left_join(
    fr_df %>%
      select(birth_year = year,
             age, sex, group_id,
             fr_birth = fertility_rate),
    by = c("birth_year", "age", "sex", "group_id")
  ) %>%
  # look up survival probability p_survive_{y-b, b+1}
  left_join(
    survival_df,
    by = c("birth_year" = "year",
           "survival_age" = "age",
           "group_id")
  ) %>%
  # keep rows where both joins succeeded
  filter(!is.na(fr_birth), !is.na(survival_prob)) %>%
  mutate(
    expected_children = fr_birth * survival_prob          # eq. (2)
  ) %>%
  select(year, age, sex,
         child_age, expected_children, group_id)

# =============================================================================
# STEP 3: Unadjusted orphan events from parental death (O_death)
#   O_death = C * deaths
# Requires: C_df, deaths_df
# =============================================================================
# !!!!!!!!!!!!!!!!!!!!!!!! ONLY FOR SINGLE AGE BAND !!!!!!!!!!!!!!!!!!!!!!!!!
## convert a single age (integer) to a 5-year band label
to_band <- function(a) {
  if (a >= 85) return("85+")
  lo <- floor(a / 5) * 5
  hi <- lo + 4
  sprintf("%02d-%02d", lo, hi)
}


# ── 1. Collapse C_df from single-age to 5-year age-bands ────────────────
C_band_df <- C_df %>%                            # cols: year, age, sex, ...
  mutate(age_band = vapply(age, to_band, "")) %>%# 15 → "15-19", 33 → "30-34", …
  group_by(year, age = age_band, sex, group_id,  # rename age_band → age so the
           child_age) %>%                        # join works out of the box
  summarise(
    expected_children = mean(expected_children), # ⟵ formula (3)
    .groups = "drop"
  )

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ── 2. Join to deaths (which already has 5-year bands) and multiply ────
O_death_df <- C_band_df %>%
  inner_join(deaths_df, by = c("year", "age", "sex", "group_id")) %>%
  mutate(O_death = expected_children * deaths) %>%
  select(year, age, sex, child_age,
         expected_children, deaths, O_death, group_id)

# =============================================================================
# STEP 4: Subtract overlapping events (double orphaning & prior orphaning)
#   O_double = hazard_opp * C * deaths
#   O_prev   = (sum past hazards) * C * deaths
# Requires: O_death_df, hazard5yr_df, age_band_df
# =============================================================================
# Define your band lower bound
female_lowers <- seq(15, 60, by = 5)  # 15,20,…,60 → bands 15–19 … 60–64
male_lowers   <- seq(15, 75, by = 5)  # 15,20,…,75 → bands 15–19 … 75–79

# Helper to turn a vector of lowers into a full band table
make_bands <- function(sex, lowers) {
  tibble(
    sex             = sex,
    age_lower       = lowers,
    age_upper       = lowers + 4,
    parent_age_band = paste0(lowers, "-", lowers + 4),
    midpoint_age    = (lowers + (lowers + 4)) / 2
  )
}

age_band_df <- bind_rows(
  make_bands("female", female_lowers),
  make_bands("male",   male_lowers)
) %>%
  select(sex, parent_age_band, age_lower, age_upper, midpoint_age)

#---------------------- HAZARD 5 YEARS -------------------------
deaths_single <- readRDS("input-data-processed/deaths_df_long.RDS")
deaths_single <- deaths_single |> 
  left_join(geo_info |> select(mun, group_id), by = "mun") |>
  filter(age < 80)

deaths_single <- deaths_single |>
  group_by(group_id, year, sex, age) |>
  summarise(deaths = sum(deaths), .groups = "drop")

mortality_df <- deaths_single |>
  left_join(population_df_long,
            by = c("group_id", "year", "sex", "age"))

hazard5yr_df <- mortality_df |>
  mutate(mx = deaths / population,
         hazard_5yr = 1 - exp(-5 * mx)) |>
  select(year, age, sex, hazard_5yr, group_id)

hazard5yr_df <- hazard5yr_df %>%
  mutate(midpoint_age = floor(age / 5) * 5 + 2)

## ───────────────────────────────────────────────────────────────────────
##  STEP 4 – data.table pipeline to build O_death_haz_df
##         * population-weighted 5-yr hazards
##         * sex-specific opposite-parent join
##         * unique row_id for later expansion
## ───────────────────────────────────────────────────────────────────────
library(data.table)

## 0️⃣ Convert all inputs to data.table -----------------------------------
setDT(O_death_df)            # from STEP-3 (has columns: year age sex …)
setDT(age_band_df)           # cols: sex parent_age_band age_lower … midpoint_age
setDT(hazard5yr_df)          # single-age 5-yr hazards
setDT(population_df_long)    # parent population, single ages

## helper: vectorised 5-year-band label ----------------------------------
to_band <- function(a) {
  ifelse(a >= 85,
         "85+",
         sprintf("%02d-%02d",
                 floor(a/5)*5,
                 floor(a/5)*5 + 4))
}

## 1️⃣  add midpoint_age to every row in O_death_df  ----------------------
O_death_dt <- merge(
  O_death_df,
  age_band_df,
  by.x = c("age", "sex"),                  # parent age-band & sex
  by.y = c("parent_age_band", "sex"),
  all.x = TRUE,
  sort  = FALSE
)
# O_death_dt now has: year age sex child_age expected_children deaths …
#                    … midpoint_age age_lower age_upper

## 2️⃣  population-weighted 5-yr hazard for each parent band & sex --------
# 2a. attach parent pop to every single-age hazard row
haz_weighted <- merge(
  hazard5yr_df,                            # year age sex group_id hazard_5yr
  population_df_long,                      # year age sex group_id population
  by = c("year", "age", "sex", "group_id"),
  all.x = TRUE,
  sort  = FALSE
)

# 2b. compute weighted mean within each 5-yr band
haz_weighted[, age_band := to_band(age)]

haz_band_dt <- haz_weighted[,
                            .(hazard_band =
                                weighted.mean(hazard_5yr,
                                              w = population,
                                              na.rm = TRUE)),
                            by = .(year, age_band, sex, group_id)
]

## 3️⃣  bring in opposite-sex hazard for the same band & year -------------
#   – rename age_band → age so both tables share the same column name
setnames(haz_band_dt, "age_band", "age")
setkeyv(haz_band_dt, c("year", "age", "sex", "group_id"))

# mark the opposite sex in O_death_dt
O_death_dt[, opposite_sex := fifelse(sex == "male", "female", "male")]

# left join: keep all parent rows, add hazard_band
O_death_dt <- merge(
  O_death_dt,
  haz_band_dt,
  by.x = c("year", "age",  "opposite_sex", "group_id"),
  by.y = c("year", "age",  "sex",          "group_id"),
  all.x = TRUE,
  sort  = FALSE
)

setnames(O_death_dt, "hazard_band", "hazard_opp")
O_death_dt[is.na(hazard_opp), hazard_opp := 0]       # default if missing

## 4️⃣  same-year double-orphan counts ------------------------------------
O_death_dt[, O_double := hazard_opp * expected_children * deaths]

## 5️⃣  unique row_id for “prior orphan” expansion ------------------------
O_death_dt[, row_id := .I]

## 6️⃣  nice column order (keeps downstream code unchanged) ---------------
setcolorder(
  O_death_dt,
  c("row_id", "year", "age", "midpoint_age",
    "sex", "opposite_sex", "child_age", "group_id",
    "expected_children", "deaths", "O_death",
    "hazard_opp", "O_double")
)

## 7️⃣  hand back as data.frame for the rest of the pipeline --------------
O_death_haz_df <- as.data.frame(O_death_dt)



## NEW APPROACH
library(data.table)

setDT(O_death_haz_df)
setDT(hazard5yr_df)
# Add row ID to merge back later
O_death_haz_df[, row_id := .I]

# Expand rows for all child-age years prior to current
expanded <- O_death_haz_df[child_age > 1][
  , .(i = seq_len(child_age - 1)), 
  by = .(row_id, year, midpoint_age, opposite_sex, expected_children, deaths, group_id)
][
  , `:=`(
    target_year = year - i,
    target_age  = midpoint_age - i
  )
][
  , .(row_id, target_year, target_age, opposite_sex, expected_children, deaths, group_id)
]


setnames(hazard5yr_df, c("year", "age", "sex", "hazard_5yr", "group_id", "midpoint_age"))

joined <- expanded[
  hazard5yr_df,
  on = .(target_year = year, target_age = age, opposite_sex = sex, group_id),
  nomatch = 0
][
  , partial_hazard := hazard_5yr * expected_children * deaths
]

prior_sums <- joined[
  , .(O_prev = sum(partial_hazard)), by = row_id
]

O_death_haz_df <- merge(
  O_death_haz_df,
  prior_sums,
  by = "row_id",
  all.x = TRUE
)

O_death_haz_df[is.na(O_prev), O_prev := 0]

# =============================================================================
# STEP 5: Calculate incidence of new orphans (O_new)
#   O_new = (sum parent O_death) - 0.5*(sum O_double) - (sum O_prev)
# =============================================================================
O_new_df <- O_death_haz_df %>%
  # combine mother and father contributions
  group_by(year, age, child_age) %>%
  summarise(
    sum_death  = sum(O_death),
    sum_double = sum(O_double),
    sum_prev   = sum(O_prev),
    .groups = 'drop'
  ) %>%
  mutate(
    O_new = sum_death - 0.5 * sum_double - sum_prev
  )

O_new_df <- O_death_haz_df %>%
  group_by(year, child_age, sex, group_id) %>%
  summarise(
    sum_death  = sum(O_death, na.rm = TRUE),
    sum_double = sum(O_double, na.rm = TRUE),
    sum_prev   = sum(O_prev, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    O_new = sum_death - 0.5 * sum_double - sum_prev
  )


# Annual incidence across all bands and ages
incidence_df <- O_new_df %>%
  group_by(year, group_id, sex) %>%
  summarise(incidence = sum(O_new, na.rm = T), .groups = 'drop')



# =============================================================================
# PLOTS
# =============================================================================
# MAP
library(sf)
library(ggplot2)
library(dplyr)

grouped_mun_50000 <- readRDS(file = "input-data-processed/grouped_municipality_50000.RDS")

map_data_incidence <- left_join(
  grouped_mun_50000,                      # sf object with geometry + group_id
  incidence_df %>% 
    filter(year == 2023) %>%
    group_by(group_id) %>%
    summarise(incidence = sum(incidence, na.rm = TRUE), .groups = "drop"),
  by = "group_id"
)
library(ggplot2)
library(scales)
map_data_incidence <- map_data_incidence %>%
  mutate(incidence = ifelse(incidence <= 0, NA, incidence))

# Plot
ggplot(map_data_incidence) +
  geom_sf(aes(fill = incidence), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "C",
    trans = "log",
    na.value = "grey90",
    name = "(Log) Orphans ",
    guide = guide_colorbar(
      barheight = unit(4, "cm"),
      barwidth = unit(1, "cm"),
      ticks = TRUE,
      title.position = "top",
      title.hjust = 0.5
    ),
    labels = label_number(scale_cut = cut_short_scale())  # e.g., 1K, 10K
  ) +
  labs(
    title = "Orphanhood Incidence by Municipality (2023)",
    subtitle = "Log-scaled number of orphans per region",
    fill = "Orphans"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )



# ──────────────────────────────────────────────────────────────
#  Trend in annual orphan incidence – bar version
# ──────────────────────────────────────────────────────────────
library(tidyverse)
library(scales)
library(patchwork)

# If you do NOT have this yet ─────────────────────────────────
# (inc_rate_df joins child population so we can compute a rate)
pop_children_df <- population_children %>%
  group_by(year) %>% summarise(pop = sum(population), .groups = "drop")
#
inc_rate_df <- incidence_df %>%
  left_join(pop_children_df, by = "year") %>%
  mutate(rate_per_100 = incidence / pop * 100) %>%
  filter(year >= 2000)

incidence_df <- incidence_df |>
  filter(year >= 2000)

# ── Panel A  𝗔𝗯𝘀𝗼𝗹𝘂𝘁𝗲 𝗰𝗼𝘂𝗻𝘁 ────────────────────────────────
p_abs_bar <- ggplot(incidence_df, aes(year, incidence)) +
  geom_col(fill = "#0072B2", width = 0.8) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    y = "New orphans",
    x = NULL,
    title = "Annual incidence of orphanhood\n(Mexico, ages 0-17)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# ── Panel B  𝗥𝗮𝘁𝗲 𝗽𝗲𝗿 𝟭𝟬𝟬 𝗰𝗵𝗶𝗹𝗱𝗿𝗲𝗻 ───────────────────────
p_rate_bar <- ggplot(inc_rate_df, aes(year, rate_per_100)) +
  geom_col(fill = "#D55E00", width = 0.8) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1, suffix = "%")   # 0.57 %
  ) +
  labs(
    y = "Incidence (% of children)",
    x = NULL,
    title = "Incidence rate of orphanhood"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),                    # suppress duplicate axis
    axis.ticks.x = element_blank()
  )

# ── Arrange side-by-side like Villaveces et al. Fig 1 ─────────
p_abs_bar | p_rate_bar
p_abs_bar


inc_sex <- O_death_df %>%
  group_by(year, sex) %>%
  filter(year >=2006) %>%
  summarise(orphans = sum(O_death), .groups = "drop") %>%
  mutate(sex = recode(sex, male = "Father", female = "Mother"))

ggplot(inc_sex, aes(x = year, y = orphans, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Father" = "#377eb8", "Mother" = "#e41a1c"),
    name = "Deceased parent"
  ) +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "New orphans",
    title = "Maternal vs. Paternal Orphanhood Over Time"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )



# =============================================================================
# STEP 6: Compute prevalence of living orphans (O_lifetime)
#   Sum incidence over past 17 years, adjusted for child survival to current year
# Requires: O_new_df, child_hazard_df
# =============================================================================
mort_df <- readRDS("input-data-processed/mort_df.RDS")

deaths_child_single <- mort_df |> 
  left_join(geo_info |> select(mun, group_id), by = "mun") |>
  group_by(group_id, year, sex, age) |>
  summarise(deaths = sum(deaths), .groups = "drop")

deaths_child_single <- deaths_child_single|>
  mutate(age = as.numeric(age)) |>
  left_join(population_children, by = c("group_id", "year", "sex", "age"))

child_hazard_df <- deaths_child_single |>
  mutate(mx = deaths / population,
         hazard_1yr = 1 - exp(- mx)) |>
  select(year, age, sex, hazard_1yr, group_id)

prevalence_list <- lapply(unique(O_new_df$year), function(curr_year) {
  tibble(
    year       = curr_year,
    child_age  = child_ages,
    O_lifetime = sapply(child_ages, function(b) {
      sum(sapply(0:b, function(i) {
        # Look up incidence
        inc_val <- O_new_df |>
          filter(
            year      == (curr_year - i),
            child_age == (b - i)
          ) |>
          pull(O_new)
        
        inc_val <- if (length(inc_val) == 0) 0 else sum(inc_val, na.rm = TRUE)
        
        # Compute survival probability from year (curr_year - i) to curr_year
        if (i == 0) {
          inc_val
        } else {
          surv_prob <- sapply(1:i, function(j) {
            age_lookup <- b - i + j - 1
            year_lookup <- curr_year - i + j
            
            df <- child_hazard_df |>
              filter(year == year_lookup, age == age_lookup)
            
            # If no data, assume full survival
            if (nrow(df) == 0 || is.na(df$hazard_1yr[1])) {
              1
            } else {
              1 - df$hazard_1yr[1]
            }
          })
          
          inc_val * prod(surv_prob)
        }
      }))
    })
  )
})

# Combine all years into one data frame
prevalence_df <- bind_rows(prevalence_list)

# Aggregate total prevalence per year
total_prevalent <- prevalence_df |>
  group_by(year) |>
  summarise(prevalence = sum(O_lifetime, na.rm = TRUE), .groups = "drop")

#-------------- PREVALENCE STRATIFIED BY THE SEX OF THE PARENT
# Convert to data.table
O_new_dt <- as.data.table(O_new_df)[year >= 2006]
hazard_dt <- as.data.table(child_hazard_df)

# Ensure key columns for fast joins
setkey(O_new_dt, year, child_age, sex)
setkey(hazard_dt, year, age)

# Create all combinations
years        <- unique(O_new_dt$year)
child_ages   <- unique(O_new_dt$child_age)
parent_sexes <- unique(O_new_dt$sex)

# Initialize output list
prevalence_list_sex <- vector("list", length(years))

for (i in seq_along(years)) {
  curr_year <- years[i]
  
  combo_dt <- CJ(
    year       = curr_year,
    child_age  = child_ages,
    parent_sex = parent_sexes,
    unique = TRUE
  )
  
  # Row-wise compute O_lifetime
  combo_dt[, O_lifetime := {
    b <- child_age
    y <- year
    s <- parent_sex
    
    sum(sapply(0:b, function(i) {
      # Get O_new at (y-i, b-i, s)
      inc_row <- O_new_dt[year == (y - i) & child_age == (b - i) & sex == s]
      inc_val <- if (nrow(inc_row) == 0) 0 else sum(inc_row$O_new, na.rm = TRUE)
      
      if (i == 0) {
        return(inc_val)
      }
      
      # Compute product of survival probabilities over j = 1 to i
      surv_prob <- sapply(1:i, function(j) {
        age_lookup  <- b - i + j - 1
        year_lookup <- y - i + j
        
        haz_val <- hazard_dt[year == year_lookup & age == age_lookup, hazard_1yr]
        
        if (length(haz_val) == 0 || is.na(haz_val[1])) {
          return(1)
        } else {
          return(1 - haz_val[1])
        }
      })
      
      inc_val * prod(surv_prob)
    }))
  }, by = .(year, child_age, parent_sex)]
  
  prevalence_list_sex[[i]] <- combo_dt
}

# Combine into one data.table
prevalence_dt_sex <- rbindlist(prevalence_list_sex)

#-------------------------------------------------------------------------------

#--------------------- DATA TABLE -------------------
library(data.table)

#### 1.  Main pipeline (your code) -----------------------------------
setDT(O_new_df); setDT(child_hazard_df)

child_ages   <- 0:17
all_years    <- sort(unique(O_new_df$year))
all_sex      <- unique(O_new_df$sex)
all_group_id <- unique(O_new_df$group_id)

grid <- CJ(year = all_years,
           child_age = child_ages,
           sex = all_sex,
           group_id = all_group_id,
           i = 0:17)

grid[ , `:=`(year_lookup = year - i,
             age_lookup  = child_age - i)]
grid <- grid[age_lookup >= 0 & year_lookup >= min(O_new_df$year)]
grid[ , row_id := .I]

setkey(O_new_df, year, child_age, sex, group_id)
grid <- O_new_df[grid,
                 on = .(year = year_lookup,
                        child_age = age_lookup,
                        sex, group_id)]
grid[is.na(O_new), O_new := 0]

# ----- survival ------------------------------------------------------
surv_paths <- grid[i > 0, .(year, child_age, sex, group_id, i, row_id,
                            step      = 1:i,
                            year_surv = year - i + 1:i,
                            age_surv  = child_age - i + 0:(i - 1))]

setkey(child_hazard_df, year, age, sex, group_id)
surv_paths <- child_hazard_df[surv_paths,
                              on = .(year = year_surv,
                                     age  = age_surv,
                                     sex, group_id),
                              nomatch = 0]

surv_paths[ , survival_step := 1 - hazard_1yr]
surv_mult_dt <- surv_paths[ , .(surv_mult = prod(survival_step)),
                            by = row_id]

grid <- merge(grid, surv_mult_dt, by = "row_id", all.x = TRUE)
grid[i == 0 | is.na(surv_mult), surv_mult := 1]

grid[ , contrib := O_new * surv_mult]

# ----- aggregate -----------------------------------------------------
prevalence_df <- grid[ ,
                       .(O_lifetime = sum(contrib)),
                       by = .(year, child_age, sex, group_id)
]

#### 2.  Keep only 2006–2023 (full look-back) -------------------------
prevalence_df   <- prevalence_df[
  year >= 2006 & year <= 2023
]

##################### PLOTS
library(ggplot2)
library(dplyr)

total_prevalent <- total_prevalent |>
  filter(year >=2007)
# 1. Plot total orphanhood prevalence over time
ggplot(total_prevalent, aes(x = year, y = prevalence)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_point(color = "#2980B9", size = 2) +
  labs(
    title = "Total Orphanhood Prevalence Over Time",
    x = "Year",
    y = "Number of Children"
  ) +
  theme_minimal()

# 2. Plot orphanhood prevalence rate per 100 children by age and year
prevalence_rate_df <- prevalence_df %>%
  group_by(year, child_age) %>%
  summarise(rate_per_100 = sum(O_lifetime, na.rm = TRUE) / sum(population_children$population) * 100, .groups = "drop")

ggplot(prevalence_rate_df, aes(x = year, y = rate_per_100, group = child_age, color = as.factor(child_age))) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Orphanhood Prevalence Rate per 100 Children by Age",
    x = "Year",
    y = "Prevalence Rate per 100 Children",
    color = "Child Age"
  ) +
  theme_minimal()

# 3. Prevalence by child age in a selected year 
selected_year <- 2023

plot_df <- prevalence_df %>%
  filter(year == selected_year) %>%
  group_by(child_age) %>%
  summarise(prevalence = sum(O_lifetime, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = child_age, y = prevalence)) +
  geom_bar(stat = "identity", fill = "#3498DB") +
  labs(
    title = paste("Orphanhood Prevalence by Child Age in", selected_year),
    x = "Child Age",
    y = "Number of Orphaned Children"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
plot <- prevalence_dt_sex %>%
  group_by(year) %>%
  summarise(prevalence = sum(O_lifetime, na.rm = TRUE), .groups = "drop")

# 1. Plot total orphanhood prevalence over time
ggplot(plot, aes(x = year, y = prevalence)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_point(color = "#2980B9", size = 2) +
  labs(
    title = "Total Orphanhood Prevalence Over Time",
    x = "Year",
    y = "Number of Children"
  ) +
  theme_minimal()

plot_a <- prevalence_dt_sex %>%
  filter(year == 2021) %>%
  group_by(child_age, parent_sex) %>%
  summarise(prevalence = sum(O_lifetime, na.rm = TRUE), .groups = "drop")

ggplot(plot_a, aes(x = child_age, y = prevalence, fill = parent_sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Orphanhood Prevalence by Child Age and Parent Sex (2021)",
    x = "Child Age",
    y = "Number of Orphaned Children",
    fill = "Sex of Parent"
  ) +
  theme_minimal()


plot_b <- prevalence_dt_sex %>%
  filter(year == 2021) %>%
  mutate(age_group = case_when(
    child_age <= 4 ~ "0–4",
    child_age <= 9 ~ "5–9",
    TRUE ~ "10–17"
  )) %>%
  mutate(age_group = factor(age_group, levels = c("0–4", "5–9", "10–17"))) %>%
  group_by(age_group) %>%
  summarise(
    prevalence = sum(O_lifetime, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_b, aes(x = age_group, y = prevalence)) +
  geom_bar(stat = "identity", fill = "#1F77B4") +
  labs(
    title = "Orphanhood Prevalence by Age Group (2021)",
    x = "Age Group",
    y = "Number of Orphaned Children"
  ) +
  theme_minimal()

plot_c <- prevalence_dt_sex %>%
  group_by(year, parent_sex) %>%
  summarise(prevalence = sum(O_lifetime, na.rm = TRUE), .groups = "drop")

ggplot(plot_c, aes(x = year, y = prevalence, color = parent_sex)) +
  geom_line(size = 1.1) +
  geom_point() +
  labs(
    title = "Time Trend of Orphanhood by Parent Sex",
    x = "Year",
    y = "Number of Orphaned Children",
    color = "Sex of Parent"
  ) +
  theme_minimal()


plot_d <- prevalence_dt_sex %>%
  mutate(age_group = case_when(
    child_age <= 4 ~ "0–4",
    child_age <= 9 ~ "5–9",
    TRUE ~ "10–17"
  )) %>%
  group_by(year, age_group) %>%
  summarise(
    prevalence = sum(O_lifetime, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_d, aes(x = year, y = prevalence, color = age_group, group = age_group)) +
  geom_line(alpha = 0.7) +
  geom_point() +
  labs(
    title = "Time Trend of Orphanhood by Child Age",
    x = "Year",
    y = "Number of Orphaned Children",
    color = "Child Age"
  ) +
  theme_minimal()


plot_d <- prevalence_df %>%
  mutate(age_group = case_when(
    child_age <= 4 ~ "0–4",
    child_age <= 9 ~ "5–9",
    TRUE ~ "10–17"
  )) %>%
  group_by(year, age_group) %>%
  summarise(
    orphaned = sum(O_lifetime, na.rm = TRUE),     # total orphans
    total_children = n(),                         # total children
    .groups = "drop"
  ) %>%
  mutate(
    prevalence_rate = (orphaned / total_children) * 100
  )

ggplot(plot_d, aes(x = year, y = prevalence_rate, color = age_group, group = age_group)) +
  geom_line(alpha = 0.7, size = 1.2) +
  labs(
    title = "Orphanhood Prevalence Rate per 100 Children by Age Group",
    x = "Year",
    y = "Prevalence Rate (per 100 children)",
    color = "Child Age"
  ) +
  theme_minimal()


# MAP
library(sf)
library(ggplot2)
library(dplyr)

grouped_mun_50000 <- readRDS(file = "input-data-processed/grouped_municipality_50000.RDS")

map_data <- left_join(
  grouped_mun_50000,                      # sf object with geometry + group_id
  prevalence_df %>% 
    filter(year == 2023) %>%
    group_by(group_id) %>%
    summarise(O_lifetime = sum(O_lifetime, na.rm = TRUE), .groups = "drop"),
  by = "group_id"
)
library(ggplot2)
library(scales)

map_data <- map_data %>%
  mutate(O_lifetime = ifelse(O_lifetime <= 0, NA, O_lifetime))

# Plot
ggplot(map_data) +
  geom_sf(aes(fill = O_lifetime), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "C",
    trans = "log",
    na.value = "grey90",
    name = "Orphans (log scale)",
    guide = guide_colorbar(
      barheight = unit(4, "cm"),
      barwidth = unit(1, "cm"),
      ticks = TRUE,
      title.position = "top",
      title.hjust = 0.5
    ),
    labels = label_number(scale_cut = cut_short_scale())  # e.g., 1K, 10K
  ) +
  labs(
    title = "Orphanhood Prevalence by Municipality (2023)",
    subtitle = "Log-scaled number of orphans (O_lifetime) per region",
    fill = "Orphans"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )


inc_sex <- O_death_df %>%
  group_by(year, sex) %>%
  filter(year >=2006) %>%
  summarise(orphans = sum(O_death), .groups = "drop") %>%
  mutate(sex = recode(sex, male = "Father", female = "Mother"))

ggplot(inc_sex, aes(x = year, y = orphans, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Father" = "#377eb8", "Mother" = "#e41a1c"),
    name = "Deceased parent"
  ) +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "New orphans",
    title = "Maternal vs. Paternal Orphanhood Over Time"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )



################ THREE MAPS
## MPI
mpi <- readRDS("input-data-processed/index_grouped_mun.RDS")
mpi <- mpi |> 
  filter(year == 2020) |>
  left_join(
    grouped_mun_50000 |> 
      select(group_id, geometry),
    by = "group_id"
  )

mpi <- sf::st_as_sf(mpi)

map_mpi <- ggplot(mpi) +
  geom_sf(aes(fill = IMN), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey90",
    name = "MPI",
    guide = guide_colorbar(
      barheight = unit(4, "cm"),
      barwidth  = unit(1, "cm"),
      ticks     = TRUE,
      title.position = "top",
      title.hjust    = 0.5
    ),
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = "Poverty index",
    subtitle = "Value of IMN",
    fill = "MPI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10)
  )




library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(scales)

# Aggregate deaths data
deaths <- readRDS("input-data-processed/mort_by_grouped_mun.RDS") |>
  group_by(year, group_id) |>
  summarise(
    tot_deaths     = sum(tot_deaths, na.rm = TRUE),
    tot_population = sum(tot_population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(year >= 1990 & year <= 2023)

# Filter for 2020 and calculate mortality rate
deaths_map <- deaths |>
  filter(year == 2023) |>
  left_join(
    grouped_mun_50000 |> select(group_id, geometry),
    by = "group_id"
  ) |>
  mutate(
    mort_rate = tot_deaths / tot_population
  ) |>
  sf::st_as_sf()

# Plot corrected mortality rate map
ggplot(deaths_map) +
  geom_sf(aes(fill = mort_rate), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "C",  # more intuitive sequential palette
    na.value = "grey90",
    name = "Mortality rate",
    guide = guide_colorbar(
      barheight     = unit(4, "cm"),
      barwidth      = unit(1, "cm"),
      ticks         = TRUE,
      title.position = "top",
      title.hjust    = 0.5
    ),
    labels = label_percent(accuracy = 0.1)  # Show % like 0.5%, 1.2%
  ) +
  labs(
    title    = "Municipal Mortality Rate",
    subtitle = "Deaths per total population (2023)",
    fill     = "Mortality rate"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "right",
    legend.title     = element_text(face = "bold"),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10)
  )
