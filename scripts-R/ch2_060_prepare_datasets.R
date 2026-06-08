# =============================================================================
# ch2_060_prepare_datasets.R  ·  Chapter 1 — Analytical panel builder
# Master assembly script: re-aggregate births/deaths/population/marginalization
# to the new (merged) municipalities, build the long birth/death panels, the
# monthly-birth nowcasting input, and the area-type lookups.
#
# Loads supplied per-year births/deaths + IMM 2010/2015/2020 + ch2 grouping;
# builds the new-municipality panels and marginalization. The raw-only variants
# (mort_df/deaths_df_long via orphans/long; monthly/long fertility; mun-level
# index_marg/im.tmp) are guarded/skipped pending raw INEGI extraction.
# Reads : input-data-processed/{fertility,mortality} datasets/, grouped_municipality_50000.RDS,
#         aggregated_muni_50000.RDS, geo_info.RDS, input-data-raw/marginalization/IMM_{2020.xls,DP2_2015.xlsx,DP2_2010.xlsx}
# Writes: input-data-processed/{deaths, births, births_grouped_mun, geo_info, geo_info_grouped_mun,
#         population, population_grouped_mun, mpi_mun, mpi_classified, mpi_grouped_mun, mpi_imputed,
#         marg_index, index_grouped_mun, index_marg}.RDS
# Run after: ch2_050, ch2_010
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
library(sf)
library(Polychrome)
source("R/preprocess_fertility.R"); source("R/preprocess_mortality.R"); source("R/preprocess_orphans.R")
library(arrow)
source("R/load_population.R")
source("R/marginalization.R")
source("R/load_year_panels.R")
#############################################

# MORTALITY

#############################################
# ---- DEATHS: load the supplied per-year mortality (preprocess_mortality output)
deaths <- load_year_panels("mortality datasets") |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop")
saveRDS(deaths, file = "input-data-processed/deaths.RDS")

# ---- mort_df (single-age, orphans) and deaths_df_long require the raw INEGI
#      files via preprocess_orphans() / preprocess_mortality_long(); the supplied
#      per-year RDS do not carry these. Skipped here (feed Chapter 5).
message("ch2_060: skipping mort_df / deaths_df_long (need raw orphans/long preprocessing).")


grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
aggregated_muni_50000 <- readRDS("input-data-processed/aggregated_muni_50000.RDS")
################################################################################

#---------------------------------- GEO INFO ----------------------------------

################################################################################


index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)

index <- index[-1,]

geo_info <- build_geo_info(index)

geo_info <- geo_info %>%
  left_join(grouped_municipality_50000 |> sf::st_drop_geometry() |> select(mun, group_id), by = "mun") |>
  filter(!is.na(group_id))

# Geometry per new municipality = the aggregated polygons (already unioned by
# group_id in ch2); attributes (state, capital) summarised from geo_info.
geo_attrs <- geo_info |> group_by(group_id) |>
  summarise(state_name = first(state_name), state = first(state), capital = first(capital), .groups = "drop")
geo_info_grouped_mun <- aggregated_muni_50000 |> select(group_id) |> left_join(geo_attrs, by = "group_id")
geo_info_grouped_mun$capital[geo_info_grouped_mun$group_id %in% c("06002", "30087")] <- 1


saveRDS(geo_info, file = "input-data-processed/geo_info.RDS")
saveRDS(geo_info_grouped_mun, file = "input-data-processed/geo_info_grouped_mun.RDS")

################################################################################

#--------------------------------- POPULATION ----------------------------------

################################################################################
population <- load_population(keep_child = TRUE)

population <- population |>
  left_join(geo_info |>
              select(mun, group_id) |>
              sf::st_drop_geometry(), by = "mun")

population_grouped_mun <- population |> 
  group_by(group_id, year, sex, age) |>
  summarise(population = sum(population), .groups = "drop")

saveRDS(population %>% dplyr::select(-dplyr::any_of("group_id")), file = "input-data-processed/population.RDS")
saveRDS(population_grouped_mun, file = "input-data-processed/population_grouped_mun.RDS")


################################################################################

#------------------------------------- MPI -------------------------------------

################################################################################


index_2020 <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)

index_2015 <- load_imm("input-data-raw/marginalization/IMM_DP2_2015.xlsx", 2015, sheet = "IMM_2015")

index_2010 <- load_imm("input-data-raw/marginalization/IMM_DP2_2010.xlsx", 2010, sheet = "IMM_2010")

index_2020 <- index_2020[-1,]

index_2020 <- index_2020 %>%
  mutate(IMN = as.numeric(IMN), year=2020)

index_2015 <- index_2015 %>%
  mutate(IMN = as.numeric(IMN), year=2015)

index_2010 <- index_2010 %>%
  mutate(IMN = as.numeric(IMN), year=2010)

combined_index <- bind_rows(
  index_2020 %>% dplyr:: select(GM, IMN, mun, year, population),
  index_2015 %>% dplyr:: select(GM, IMN, mun, year, population),
  index_2010 %>% dplyr:: select(GM, IMN, mun, year, population)
)

saveRDS(combined_index, "input-data-processed/mpi_mun.RDS")

grouped_municipality_50000 <- grouped_municipality_50000 |>
  st_drop_geometry() |>
  select(mun, group_id)

# ---- Urban/rural classification (from IMM small_towns_pct = PL.5000, the share of
# population living in localities < 5,000 inhabitants). A grouped unit is "Rural"
# when its population-weighted small-locality share is >= 50%, else "Urban".
# Saved as rural_urban_area.RDS, consumed by ch3_030 / ch4 nowcast EDA. ----
rural_urban_area <- index_2020 |>
  dplyr::transmute(mun, population, small_towns_pct = as.numeric(small_towns_pct)) |>
  dplyr::left_join(grouped_municipality_50000, by = "mun") |>
  dplyr::filter(!is.na(group_id)) |>
  dplyr::group_by(group_id) |>
  dplyr::summarise(small_towns_pct = weighted.mean(small_towns_pct, w = population, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::mutate(area_type = dplyr::if_else(small_towns_pct >= 50, "Rural", "Urban"))
saveRDS(rural_urban_area, "input-data-processed/rural_urban_area.RDS")

# Perform the join
combined_index_grouped_mun <- left_join(
  combined_index,
  grouped_municipality_50000,
  by = "mun"
)

mpi_grouped_mun <- combined_index_grouped_mun |>
  group_by(group_id, year) |>
  summarise(mpi = weighted.mean(IMN, w = population), .groups = "drop")

thresholds <- combined_index_grouped_mun |>
  group_by(GM) |>
  summarise(
    min_value = min(IMN, na.rm = TRUE),
    max_value = max(IMN, na.rm = TRUE)
  ) |>
  arrange(min_value)

classified_data <- combined_index_grouped_mun %>%
  group_by(group_id, year) %>%
  summarise(IMN = mean(IMN, na.rm = TRUE), .groups = "drop") %>%
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

saveRDS(classified_data, file = "input-data-processed/mpi_classified.RDS")

#------------------------------------------------------------------------------
index <- mpi_grouped_mun %>%
  complete(group_id, year = 2010:2020) %>%
  group_by(group_id) %>%
  arrange(year, .by_group = TRUE) %>%
  group_modify(~ {
    non_na_vals <- .x %>% filter(!is.na(mpi))
    
    if (nrow(non_na_vals) >= 2) {
      .x$mpi <- approx(
        x = non_na_vals$year,
        y = non_na_vals$mpi,
        xout = .x$year
      )$y
    } else if (nrow(non_na_vals) == 1) {
      .x$mpi <- rep(non_na_vals$mpi, nrow(.x))
    } else {
      # all values are NA — leave them as NA
      .x$mpi <- NA_real_
    }
    
    return(.x)
  }) %>%
  ungroup()

saveRDS(index, "input-data-processed/mpi_grouped_mun.RDS")

index <- index %>%
  mutate(
  GM = case_when(
    mpi >= thresholds$min_value[thresholds$GM == "Muy alto"] &
      mpi <= thresholds$max_value[thresholds$GM == "Muy alto"] ~ "Very High",
    mpi >= thresholds$min_value[thresholds$GM == "Alto"] &
      mpi <= thresholds$max_value[thresholds$GM == "Alto"] ~ "High",
    mpi >= thresholds$min_value[thresholds$GM == "Medio"] &
      mpi <= thresholds$max_value[thresholds$GM == "Medio"] ~ "Medium",
    mpi >= thresholds$min_value[thresholds$GM == "Bajo"] &
      mpi <= thresholds$max_value[thresholds$GM == "Bajo"] ~ "Low",
    mpi >= thresholds$min_value[thresholds$GM == "Muy bajo"] &
      mpi <= thresholds$max_value[thresholds$GM == "Muy bajo"] ~ "Very Low",
    TRUE ~ NA_character_
  )
)
index <- index |> dplyr::rename(IMN = mpi)

saveRDS(index, file = "input-data-processed/marg_index.RDS")

# index_grouped_mun = group-level classified marginalization (group_id, year, IMN, GM);
# mpi_imputed = the interpolated MPI (group_id, year, mpi). Both consumed by ch3.
saveRDS(index, file = "input-data-processed/index_grouped_mun.RDS")
# index_marg: one population-weighted marginalization value per new municipality
index_marg <- combined_index |>
  left_join(geo_info |> dplyr::select(mun, group_id), by = "mun") |>
  filter(!is.na(group_id)) |>
  group_by(group_id) |>
  summarise(marg_index_weighted = weighted.mean(as.numeric(IMN), population, na.rm = TRUE),
            .groups = "drop")
saveRDS(index_marg, file = "input-data-processed/index_marg.RDS")
file.copy("input-data-processed/mpi_grouped_mun.RDS",
          "input-data-processed/mpi_imputed.RDS", overwrite = TRUE)

################################################################################
# BIRTHS (municipality + new-municipality)
################################################################################
births <- load_year_panels("fertility datasets") |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(births = sum(births), .groups = "drop") |>
  filter(year >= 1985, year <= 2024)
saveRDS(births, file = "input-data-processed/births.RDS")

grouped_mun_lookup <- readRDS("input-data-processed/grouped_municipality_50000.RDS") |>
  sf::st_drop_geometry() |> dplyr::select(mun, group_id)
births_grouped_mun <- births |>
  dplyr::left_join(grouped_mun_lookup, by = "mun") |>
  group_by(group_id, year, sex, age, year_reg) |>
  summarise(births = sum(births), .groups = "drop")
saveRDS(births_grouped_mun, file = "input-data-processed/births_grouped_mun.RDS")

# index_marg / im.tmp (mun-level detail) feed Chapter 5; need fuller mun-level MPI.
message("ch2_060: skipping index_marg / im.tmp (need fuller mun-level MPI).")



