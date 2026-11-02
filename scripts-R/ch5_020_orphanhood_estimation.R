# =============================================================================
# ch5_020_orphanhood_estimation.R  ·  Chapter 5 — Orphanhood estimation
# Definitive long-format orphanhood pipeline: incidence + lifetime prevalence by child-age and parent-sex, plus spatial correlation (Moran's I / LISA).
# Reads input-data-processed/{geo_info*,population_grouped_mun,birth_data_all,deaths*,mort_df,births_long,fertility_df,survival_df,aggregated_muni_50000,index_grouped_mun,grouped_municipality_50000}.RDS -> input-data-processed/{fertility_df,survival_df}.RDS, output/ch5/.
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

source("R/rates.R")
source("R/load_year_panels.R")   # collapse_year_reg()
source("R/marginalization.R")

fig_dir <- marg_fig_dir("output/ch5")  # output/ch5_fw when ORPHAN_MARG=fw

# =============================================================================
#  1-11. Orphanhood engine (R/orphanhood.R; thesis §5.3, Eqs 7-19)
# =============================================================================
# Departures from the thesis text, agreed 2026-10-30:
#   * parent ages bound the age AT BIRTH (mothers 15-50, fathers 15-79); a
#     dying parent can be older (mothers to 64, the oldest band with deaths;
#     fathers to 79). Thesis §5: "women 15-49, men 15-79".
#   * Eq (10) uses cumulative cohort survival to the end of age c, not the
#     one-year period survival of the birth year.
#   * O_prev is the exact year-by-year sum, not Eq (16)'s (c-1) * h1.
#   * child survival is the national curve (ORPHAN_SURV=national, default) or
#     the per-unit empirical-Bayes curve (ORPHAN_SURV=unit); see ch5_011.
source("R/orphanhood.R")
g <- "input-data-processed/"
surv_source <- Sys.getenv("ORPHAN_SURV", "national")
stopifnot(surv_source %in% c("national", "unit"))

geo_info <- readRDS(paste0(g, "geo_info_mun.RDS"))
# geo_info has no group_id: attach it from the ch2 grouping (used by the maps).
geo_info <- dplyr::left_join(
  geo_info,
  sf::st_drop_geometry(readRDS(paste0(g, "grouped_municipality_50000.RDS"))) |>
    dplyr::select(mun, group_id),
  by = "mun")
population_df <- readRDS(paste0(g, "population_grouped_mun.RDS"))
pop1 <- single_year_population(population_df)

fert <- fertility_rates(readRDS(paste0(g, "births_grouped_mun.RDS")), pop1)  # Eq (7)
s1   <- readRDS(paste0(g, sprintf("child_survival_%s.RDS", surv_source)))    # Eqs (8)-(9)
S    <- cohort_survival(s1)                                                  # Eq (10)
C    <- expected_children(fert, S, pop1)                                     # Eqs (10)-(11)
D    <- parental_deaths(readRDS(paste0(g, "deaths_grouped_mun.RDS")))        # §5.3.5
hz   <- adult_hazards(D, population_df)                                      # Eqs (13), (15)
ev   <- orphan_events(C, D, hz)                                              # Eqs (12)-(17)
prev_unit <- prevalence(ev, s1, by = "sex")                                  # Eqs (18)-(19)

saveRDS(ev[, .(O_death = sum(O_death), O_double = sum(O_double), O_prev = sum(O_prev),
               O_prev_eq16 = sum(O_prev_eq16), O_new = sum(O_new)),
           by = .(group_id, year, sex, child_age)],
        paste0(g, sprintf("orphan_incidence_%s.RDS", surv_source)))
saveRDS(prev_unit, paste0(g, sprintf("orphan_prevalence_%s.RDS", surv_source)))

# Objects used by the figures below. Years before 2007 lack children born
# before 1990 (no fertility data), so they undercount the oldest ages.
INCOMPLETE_NOTE <- "2005-2006: children aged 16-17 not covered (births before 1990 not in the data)."
fertility_df <- as.data.frame(fert[, .(group_id, year, sex, age, births, population,
                                       fertility_rate = fert_rate)])
saveRDS(fertility_df, "input-data-processed/fertility_df.RDS")   # read by ch5_030
population_df_long <- as.data.frame(pop1[ages_table(ORPHAN_FERT_AGES), on = .(sex, age), nomatch = 0])
population_children <- pop1[age <= 17]
O_death_dt <- ev
O_new_tot  <- ev[, .(O_new = sum(O_new)), by = .(group_id, year, child_age)]
O_new_sex  <- ev[, .(O_new_sex = sum(O_new)), by = .(group_id, year, sex, child_age)]
O_lifetime       <- prev_unit[, .(Olifetime = sum(O_life)), by = .(year, child_age)]
O_lifetime_sex   <- prev_unit[, .(Olifetime = sum(O_life)), by = .(year, sex, child_age)]
O_lifetime_total <- prev_unit[, .(Olifetime = sum(O_life)), by = year][order(year)]

O_new_tot        <- O_new_tot[year >= 2005]
O_new_sex        <- O_new_sex[year >= 2005]
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
  labs(caption = INCOMPLETE_NOTE, title = "Incidence of Orphanhood (new cases)",
       y = "New orphans", x = NULL) +
  theme_minimal()

print(p_incidence)
ggsave(file.path(fig_dir, "ch5_020_incidence.pdf"), p_incidence, width = 8, height = 6)

inc_plot <- O_new_tot |>
  group_by(year) |>
  summarise(incidence = sum(O_new, na.rm = T), .groups = "drop")

p_incidence_line <- ggplot(inc_plot, aes(year, incidence)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(50000, NA),                                   # ← starts at 0
    labels  = label_number(scale_cut = cut_short_scale())
  ) +
  labs(caption = INCOMPLETE_NOTE, title = "Incidence of Orphanhood (new cases)",
       y = "New orphans", x = NULL) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch5_020_incidence_line.pdf"), p_incidence_line, width = 8, height = 6)


# ------------ 2. Lifetime prevalence (total) ------------------
p_prev_tot <- ggplot(O_lifetime_total, aes(year, Olifetime)) +
  geom_line(linewidth = 1.2, colour = "#D55E00") +
  geom_point(size = 2, colour = "#D55E00") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Lifetime Prevalence of Orphanhood (ages 0–17)",
       y = "Children currently orphaned", x = NULL) +
  theme_minimal()

print(p_prev_tot)
ggsave(file.path(fig_dir, "ch5_020_prevalence_total.pdf"), p_prev_tot, width = 8, height = 6)
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
ggsave(file.path(fig_dir, "ch5_020_prevalence_by_child_age.pdf"), p_prev_age, width = 8, height = 6)
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
ggsave(file.path(fig_dir, "ch5_020_prevalence_by_sex.pdf"), p_prev_sex, width = 8, height = 6)

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
p_prev_sex_filtered <- ggplot(prev_sex_year, aes(year, Olifetime, colour = sex)) +
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
ggsave(file.path(fig_dir, "ch5_020_prev_sex_filtered.pdf"), p_prev_sex_filtered, width = 8, height = 6)
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
p_incidence_rate_sex <- ggplot(rate_df, aes(year, rate_per_100, colour = sex, group = sex)) +
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
    caption = INCOMPLETE_NOTE, title = "Incidence Rate of Orphanhood by Parent Sex",
    y     = "New orphans per 100 children",
    x     = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.position = "right"
  )
ggsave(file.path(fig_dir, "ch5_020_incidence_rate_sex.pdf"), p_incidence_rate_sex, width = 8, height = 6)


# ──────────────────────────────────────────────────────────────
#  Incidence rate of orphanhood MAP
# ──────────────────────────────────────────────────────────────
aggregated_muni <- readRDS(file = "input-data-processed/aggregated_muni_50000.RDS")

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
ggsave(file.path(fig_dir, "ch5_020_incidence_map.pdf"), inc_map, width = 8, height = 7)

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
  # Yellow at the median, as on the other maps: a few very high units would
  # otherwise put yellow far above the typical unit.
  scale_fill_gradient2(
    low      = "#126a38",
    mid      = "#ffff8c",
    high     = "#b2182b",
    midpoint = median(map_data_rate$orphan_rate, na.rm = TRUE),
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
ggsave(file.path(fig_dir, "ch5_020_orphan_rate_map.pdf"), rate_map, width = 8, height = 7)


# ──────────────────────────────────────────────────────────────
#  MPI MAP
# ──────────────────────────────────────────────────────────────
mpi <- readRDS(marg_file("index_grouped_mun"))
mpi <- mpi |> 
  filter(year == 2020) |>
  left_join(
    aggregated_muni |> 
      select(group_id, geometry),
    by = "group_id"
  )

mpi <- sf::st_as_sf(mpi)

map_mpi <- ggplot(mpi) +
  # IMN runs backwards (higher = LESS marginalized). Plot the deprivation
  # score 100 * (1 - IMN) instead, so high = more marginalized = red (Oliver's
  # note on Fig 18); yellow at the median rather than a fixed midpoint.
  geom_sf(aes(fill = 100 * (1 - IMN)), colour = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low       = "#126a38",
    mid       = "#ffff8c",
    high      = "#b2182b",
    midpoint  = median(100 * (1 - mpi$IMN), na.rm = TRUE),
    na.value  = "grey90",
    name      = "Marginalization\n100 x (1 - IMN)",
    guide     = guide_colorbar(
      barheight     = unit(4, "cm"),
      barwidth      = unit(1, "cm"),
      ticks         = TRUE,
      title.position = "top",
      title.hjust    = 0.5)
  ) +
  # ─────────────────────────────────────────────────────────────
  
  labs(
    title    = "Marginalization (2020)",
    subtitle = "Higher = more marginalized",
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
ggsave(file.path(fig_dir, "ch5_020_mpi_map.pdf"), map_mpi, width = 8, height = 7)

# ──────────────────────────────────────────────────────────────
#  Lineplot standardized fertility rate in 2023
# ──────────────────────────────────────────────────────────────
fert_df_year <- fertility_df |>
  rename(fert_rate = fertility_rate) |>
  filter(year>= 2007)   # 2007 onwards for the period-mean Fig 18

geo_info_grouped_mun <- readRDS("input-data-processed/geo_info_grouped_mun.RDS")
geo_info_grouped_mun <- st_drop_geometry(geo_info_grouped_mun)
mpi <- st_drop_geometry(mpi) |>
  select(group_id, year, IMN) |>
  distinct()

std_raw <- compute_std_fert_rate(fert_df_year, std_pop = std_ref_population(population_df_long));
std_raw_year <- std_raw %>% 
  left_join(y = geo_info_grouped_mun |> dplyr::select(group_id, capital), by = "group_id") %>% 
  left_join(y = mpi, by = c("group_id", "year")) %>% 
  mutate(
    capital = as.character(capital),
    capital = replace_na(capital, "0"),
    capital = factor(capital)
  )

std_raw_year <- std_raw |>
  filter(year >= 2010) |>
  group_by(year, sex) |>
  summarise(fertility_rate = mean(std_fert_rate), .groups = "drop")
  


library(ggplot2)
library(scales)

p_std_fert_rate <- ggplot(std_raw_year, aes(year, fertility_rate, group = sex, color = sex)) +
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
ggsave(file.path(fig_dir, "ch5_020_std_fert_rate.pdf"), p_std_fert_rate, width = 8, height = 6)

# ──────────────────────────────────────────────────────────────
#  Standardized fertility rate MAP
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(scales)

# ❶  Join geometry (assuming `grouped_mun_50000` is your muni shapefile)
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
    low       = "#126a38",          # high = red (Oliver's note on Fig 18)
    mid       = "#ffff8c",
    high      = "#b2182b",
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
ggsave(file.path(fig_dir, "ch5_020_std_fert_map.pdf"), fert_map, width = 8, height = 7)

# ──────────────────────────────────────────────────────────────
#  Lineplot standardized mortality rate in 2023
# ──────────────────────────────────────────────────────────────
deaths <- readRDS("input-data-processed/deaths_grouped_mun.RDS")
to_exclude <- c(as.character(1:15), "80-84")

deaths <- deaths[!deaths$age %in% to_exclude, ]

# deaths_grouped_mun has one row per registration year; collapse_year_reg()
# sums deaths over them and keeps the cell's population once.
mort_df_year <- deaths |>
  filter(year>= 2007, !is.na(age)) |>   # 2007 onwards for the period-mean Fig 18
  select(group_id, year, sex, age, tot_deaths) |>
  left_join(population_df, by = c("group_id", "year", "sex", "age")) |>
  collapse_year_reg("tot_deaths") |>
  rename(deaths = tot_deaths) |>
  mutate(mort_rate = deaths/population)

std_raw_mort <- compute_std_mort_rate(mort_df_year, std_pop = std_ref_population(population_df));

std_raw_mort_year <- std_raw_mort %>% 
  left_join(y = geo_info_grouped_mun |> dplyr::select(group_id, capital), by = "group_id") %>% 
  left_join(y = mpi, by = c("group_id", "year")) %>% 
  mutate(
    capital = as.character(capital),
    capital = replace_na(capital, "0"),
    capital = factor(capital)
  )

std_raw_mort_year <- std_raw_mort_year |>
  filter(year >= 2010) |>
  group_by(year, sex) |>
  summarise(mortality_rate = mean(std_mort_rate), .groups = "drop")



library(ggplot2)
library(scales)

p_std_mort_rate <- ggplot(std_raw_mort_year, aes(year, mortality_rate, group = sex, color = sex)) +
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
ggsave(file.path(fig_dir, "ch5_020_std_mort_rate.pdf"), p_std_mort_rate, width = 8, height = 6)

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
ggsave(file.path(fig_dir, "ch5_020_std_mort_map.pdf"), mort_map, width = 8, height = 7)
# Thesis Figure 18: poverty index | orphanhood rate / std fertility | std mortality
# Three versions: 2023, 2010, and the mean over 2007-2023 (the years covering all
# child ages). Marginalization is only available for 2010-2020. Each panel keeps
# one colour scale across the versions, so they can be compared: yellow at the
# median, limits at the 1st/99th percentiles of the values shown.
red_high <- c("#126a38", "#ffff8c", "#b2182b")
fig18_data <- list(
  mpi = readRDS(marg_file("index_grouped_mun")) |>
    transmute(group_id, year, value = 100 * (1 - IMN)),
  orphan = O_new_tot |>
    group_by(group_id, year) |>
    summarise(O = sum(O_new), .groups = "drop") |>
    left_join(population_children |>
                group_by(group_id, year) |>
                summarise(n_children = sum(population), .groups = "drop"),
              by = c("group_id", "year")) |>
    mutate(value = 1000 * O / n_children),
  fert = std_raw |>
    group_by(group_id, year) |>
    summarise(value = mean(std_fert_rate), .groups = "drop"),
  mort = std_raw_mort |>
    group_by(group_id, year) |>
    summarise(value = mean(std_mort_rate), .groups = "drop"),
  # Mortality at parenting ages: one rate for men and women, ages 25-54 (where
  # about 89% of children's fathers and 87% of their mothers are), standardised
  # to the national 2020 age and sex mix. Unit-level fertility plays no part.
  parent_mort = compute_std_mort_rate(
    filter(mort_df_year, age %in% c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54")),
    std_pop = std_ref_population(population_df), by_sex = FALSE) |>
    transmute(group_id, year, value = 1000 * std_mort_rate),
  # Full-span mortality directly standardised to the parents of children: each
  # sex x band weighted by its national share of children (2007-2023), the same
  # weights in every unit. Summed over mothers and fathers, so it reads as parent
  # deaths per 1 000 children if every unit had the national parental age mix.
  child_mort = {
    b <- C[, .(C_band = sum(C), N = parents[1]), by = .(group_id, year, sex, band)]
    wt <- b[year %in% 2007:2023, .(W = sum(C_band * N)), by = .(sex, band)]
    wt[, W := W / sum(W), by = sex]
    b <- D[, .(group_id, year, sex, band, deaths)][b, on = .(group_id, year, sex, band)]
    b[is.na(deaths), deaths := 0]
    wt[b, on = .(sex, band)][, .(value = 1000 * sum(W * deaths / N)), by = .(group_id, year)]
  }
)
fig18_data <- lapply(fig18_data, \(d) mutate(d, year = as.integer(as.character(year))))

fig18_versions <- list(
  list(file = "ch5_020_fig18_comparison.pdf",      years = 2023L,    mpi_years = 2020L,
       label = "2023",           mpi_label = "2020"),
  list(file = "ch5_020_fig18_comparison_2010.pdf", years = 2010L,    mpi_years = 2010L,
       label = "2010",           mpi_label = "2010"),
  list(file = "ch5_020_fig18_comparison_mean.pdf", years = 2007:2023, mpi_years = 2010:2020,
       label = "mean 2007-2023", mpi_label = "mean 2010-2020")
)
for (v in seq_along(fig18_versions)) {
  fig18_versions[[v]]$values <- Map(function(d, yrs) {
    d |> filter(year %in% yrs) |>
      group_by(group_id) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  }, fig18_data, list(fig18_versions[[v]]$mpi_years, fig18_versions[[v]]$years,
                      fig18_versions[[v]]$years, fig18_versions[[v]]$years,
                      fig18_versions[[v]]$years, fig18_versions[[v]]$years))
}
fig18_scale <- lapply(names(fig18_data), function(p) {
  x <- unlist(lapply(fig18_versions, \(v) v$values[[p]]$value))
  list(limits = unname(quantile(x, c(0.01, 0.99), na.rm = TRUE)), midpoint = median(x, na.rm = TRUE))
})
names(fig18_scale) <- names(fig18_data)

fig18_map <- function(df, scale, title, name, colours = red_high) {
  ggplot(left_join(select(aggregated_muni, group_id, geometry), df, by = "group_id")) +
    geom_sf(aes(fill = value), colour = "white", linewidth = 0.1) +
    scale_fill_gradient2(
      low = colours[1], mid = colours[2], high = colours[3],
      midpoint = scale$midpoint, limits = scale$limits, oob = scales::squish,
      na.value = "grey90", name = name,
      guide = guide_colorbar(barheight = unit(4, "cm"), barwidth = unit(1, "cm"),
                             title.position = "top", title.hjust = 0.5)
    ) +
    labs(title = title) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 14))
}

for (v in fig18_versions) {
  panels <- list(
    fig18_map(v$values$mpi, fig18_scale$mpi,
              sprintf("Marginalization (%s)", v$mpi_label),
              "Marginalization\n100 x (1 - IMN)"),
    fig18_map(v$values$orphan, fig18_scale$orphan,
              sprintf("Orphanhood Rate by Municipality (%s)", v$label),
              "Orphans per 1 000 children"),
    fig18_map(v$values$fert, fig18_scale$fert,
              sprintf("Standardized Fertility Rate (%s)", v$label),
              "Standardized fertility rate"))
  note <- plot_annotation(caption = paste(
    "Each panel uses the same colours in the 2023, 2010 and period-mean versions:",
    "yellow = median; values beyond the 1st/99th percentiles shown at the limit."))
  mort <- fig18_map(v$values$mort, fig18_scale$mort,
                    sprintf("Standardized Mortality Rate (%s)", v$label),
                    "Standardized mortality rate")
  parent_mort <- fig18_map(v$values$parent_mort, fig18_scale$parent_mort,
                           sprintf("Standardized Mortality Rate, Ages 25-54 (%s)", v$label),
                           "Deaths per 1 000\nadults aged 25-54")
  fig18 <- (panels[[1]] | panels[[2]]) / (panels[[3]] | mort) + note
  ggsave(file.path(fig_dir, v$file), fig18, width = 16, height = 14)
  fig18 <- (panels[[1]] | panels[[2]]) / (panels[[3]] | parent_mort) + note
  ggsave(file.path(fig_dir, sub("comparison", "parent_mort", v$file)), fig18, width = 16, height = 14)
  child_mort <- fig18_map(v$values$child_mort, fig18_scale$child_mort,
                          sprintf("Child-Weighted Parental Mortality (%s)", v$label),
                          "Parent deaths per\n1 000 children")
  fig18 <- (panels[[1]] | panels[[2]]) / (panels[[3]] | child_mort) + note
  ggsave(file.path(fig_dir, sub("comparison", "child_mort", v$file)), fig18, width = 16, height = 14)
  # The three candidates for the bottom-right panel, next to the orphanhood rate.
  if (v$label == "mean 2007-2023") {
    options_fig <- (panels[[2]] | mort) / (parent_mort | child_mort) + note
    ggsave(file.path(fig_dir, "ch5_020_fig18_mortality_options.pdf"), options_fig, width = 16, height = 14)
  }
}


# ──────────────────────────────────────────────────────────────
#  Orphanhood prevalence in 2023
# ──────────────────────────────────────────────────────────────
preval_plot <- O_lifetime_total |>
  filter(year >=2010)

p_prevalence_total <- ggplot(preval_plot, aes(year, Olifetime)) +
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
ggsave(file.path(fig_dir, "ch5_020_prevalence_2010_2023.pdf"), p_prevalence_total, width = 8, height = 6)

# ──────────────────────────────────────────────────────────────
#  Orphanhood prevalence in 2023
# ──────────────────────────────────────────────────────────────
orphans_by_year_sex <- O_lifetime_sex %>%
  group_by(year, sex) %>%
  summarise(total_orphans = sum(Olifetime, na.rm = TRUE), .groups = "drop")

pop_children_year <- pop_children_year |>
  filter(year >= 2005)

orphans_rate <- orphans_by_year_sex %>%
  left_join(pop_children_year, by = "year") %>%
  mutate(rate_per_100 = (total_orphans / children) * 100) |>
  filter(year >= 2005)


p_prevalence_rate_sex <- ggplot(orphans_rate, aes(x = year, y = rate_per_100, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    caption = INCOMPLETE_NOTE, title = "Orphanhood Prevalence Rate per 100 Children",
    x = "Year",
    y = "Rate per 100 Children",
    color = "Deceased Parent"
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 0.7),
        panel.grid = element_blank())
ggsave(file.path(fig_dir, "ch5_020_prevalence_rate_sex.pdf"), p_prevalence_rate_sex, width = 8, height = 6)

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
  filter(year >= 2005)

p_prevalence_rate_age <- ggplot(orphan_rate_by_age, aes(x = year, y = rate_per_100, color = age_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2)+
  scale_color_manual(
    values = c("0-4" = "#a8dbc5", "5-9" = "#5ea77a", "10-17" = "#33673b"),
    name = "Age of child",
    labels = c("0–4 years", "5–9 years", "10–17 years"))+
  labs(
    caption = INCOMPLETE_NOTE, title = "Orphanhood Prevalence Rate per 100 Children by Age Group",
    x = "Year",
    y = "Rate per 100 Children",
    color = "Age Group"
  )+
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 0.7),
        panel.grid = element_blank())
ggsave(file.path(fig_dir, "ch5_020_prevalence_rate_age.pdf"), p_prevalence_rate_age, width = 8, height = 6)


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
grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
grouped_municipality_50000 <- st_drop_geometry(grouped_municipality_50000)
deaths_long <- readRDS("input-data-processed/deaths_df_long.RDS") |>
  filter(year >=2000) |>   # already group_id-level (ch5_010)
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
p_dotplots_corr <- wrap_plots(plots, ncol = 2)
ggsave(file.path(fig_dir, "ch5_020_dotplots_corr.pdf"), p_dotplots_corr, width = 8, height = 6)

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
aggregated_muni <- readRDS(file = "input-data-processed/aggregated_muni_50000.RDS")

# Read your RDS
x <- readRDS(file = "input-data-processed/aggregated_muni_50000.RDS")

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

# Check presence. The 4 analysis variables (orphan_rate, std_fert_rate,
# std_mort_rate, poverty_index) are computed earlier in this script as separate
# per-group objects (map_data_rate, the std-rate summaries, marg_index) but are
# not yet merged onto the grouped geometry here. Until that merge is wired, skip
# the spatial-correlation / Fig-18 four-panel + Moran/LISA block gracefully — all
# the incidence/prevalence results and the orphanhood choropleth above are
# already produced.
if (!all(unlist(vars) %in% names(muni))) {
  message("ch5_020: spatial-correlation / Fig 18 (poverty–orphanhood panels + Moran/LISA) ",
          "skipped — orphan_rate/std_fert_rate/std_mort_rate/poverty_index are not yet ",
          "merged onto the grouped geometry (TODO: join map_data_rate + std-rate ",
          "summaries + marg_index by group_id). Main results above were produced.")
  quit(save = "no", status = 0)
}

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
# write_csv(corr_tbl, file.path(fig_dir, "spearman_correlations_municipal.csv"))

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
# write_csv(globals, file.path(fig_dir, "global_morans_i.csv"))

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
# st_write(muni, file.path(fig_dir, "lisa_orphan_rate.gpkg"), delete_dsn = TRUE)

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

