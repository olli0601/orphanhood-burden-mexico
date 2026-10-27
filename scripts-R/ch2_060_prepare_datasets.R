# =============================================================================
# ch2_060_prepare_datasets.R  ·  Chapter 2 — Grouped-municipality panels
# Re-expresses geography, population, births and CONAPO marginalization (IMM
# 2010/2015/2020, interpolated over 2010-2020) on the grouped municipalities,
# and classifies units as rural/urban.
#
# Reads : input-data-processed/{births, grouped_municipality_50000, aggregated_muni_50000}.RDS,
#         input-data-raw/population/.../1_Grupo_Quinq_00_RM.xlsx,
#         input-data-raw/marginalization/IMM_{2020.xls, DP2_2015.xlsx, DP2_2010.xlsx}
# Writes: input-data-processed/{geo_info_mun_grouped, geo_info_grouped_mun, population_mun,
#         population_grouped_mun, mpi_mun, mpi_classified, mpi_imputed, index_grouped_mun,
#         index_marg, rural_urban_area, births_grouped_mun}.RDS
# Run after: ch1_015, ch2_010
# =============================================================================

source("R/setup.R")
library(sf)
proc <- "input-data-processed"
aggregated_muni_50000 <- readRDS(file.path(proc, "aggregated_muni_50000.RDS"))

# --- Geography ------------------------------------------------------------------
index_2020 <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)

# One row per municipality, with group_id (geo_info_mun_grouped), and one per
# unit with the unioned polygon, state and capital flag (geo_info_grouped_mun).
geo_info <- build_geo_info(index_2020) |> attach_group_id(what = "ch2_060 geo_info")
geo_attrs <- geo_info |>
  group_by(group_id) |>
  summarise(state_name = first(state_name), state = first(state), capital = first(capital), .groups = "drop")
geo_info_grouped_mun <- aggregated_muni_50000 |> select(group_id) |> left_join(geo_attrs, by = "group_id")
geo_info_grouped_mun$capital[geo_info_grouped_mun$group_id %in% c("06002", "30087")] <- 1
saveRDS(geo_info, file = file.path(proc, "geo_info_mun_grouped.RDS"))
saveRDS(geo_info_grouped_mun, file = file.path(proc, "geo_info_grouped_mun.RDS"))

# --- Population, all ages 0-79 --------------------------------------------------------
# population_mun keeps municipalities that have no grouped unit.
population <- load_population(keep_child = TRUE) |>
  attach_group_id(grouping = select(geo_info, mun, group_id), drop_unmatched = FALSE,
                  what = "ch2_060 population")
population_grouped_mun <- population |>
  filter(!is.na(group_id)) |>
  group_by(group_id, year, sex, age) |>
  summarise(population = sum(population), .groups = "drop")
saveRDS(select(population, -group_id), file = file.path(proc, "population_mun.RDS"))
saveRDS(population_grouped_mun, file = file.path(proc, "population_grouped_mun.RDS"))

# --- Marginalization ---------------------------------------------------------------
combined_index <- load_imm_rounds(c("GM", "IMN", "mun", "year", "population"))
saveRDS(combined_index, file.path(proc, "mpi_mun.RDS"))

# Rural when the population-weighted share living in localities < 5,000 is >= 50%.
rural_urban_area <- index_2020 |>
  transmute(mun, population, small_towns_pct = as.numeric(small_towns_pct)) |>
  attach_group_id(what = "ch2_060 rural_urban") |>
  group_by(group_id) |>
  summarise(small_towns_pct = weighted.mean(small_towns_pct, w = population, na.rm = TRUE), .groups = "drop") |>
  mutate(area_type = if_else(small_towns_pct >= 50, "Rural", "Urban"))
saveRDS(rural_urban_area, file.path(proc, "rural_urban_area.RDS"))

marg <- group_marg_index(combined_index, years = 2010:2020, what = "ch2_060 marginalization")
saveRDS(marg$classified, file = file.path(proc, "mpi_classified.RDS"))
saveRDS(marg$imputed, file.path(proc, "mpi_imputed.RDS"))
saveRDS(marg$index, file = file.path(proc, "index_grouped_mun.RDS"))

# One population-weighted value per unit, pooled over the three rounds.
index_marg <- combined_index |>
  attach_group_id(grouping = select(geo_info, mun, group_id), what = "ch2_060 index_marg") |>
  group_by(group_id) |>
  summarise(marg_index_weighted = weighted.mean(as.numeric(IMN), population, na.rm = TRUE), .groups = "drop")
saveRDS(index_marg, file = file.path(proc, "index_marg.RDS"))

# --- Births -------------------------------------------------------------------------
births_grouped_mun <- aggregate_to_grouped_mun(readRDS(file.path(proc, "births.RDS")), "births")
saveRDS(births_grouped_mun, file = file.path(proc, "births_grouped_mun.RDS"))

# Every grouped artefact carries exactly the grouping's units.
n_units <- n_distinct(read_group_lookup()$group_id)
stopifnot(!anyNA(births_grouped_mun$group_id),
          !anyNA(population_grouped_mun$group_id),
          n_distinct(births_grouped_mun$group_id) == n_units,
          n_distinct(population_grouped_mun$group_id) == n_units)
message(sprintf("ch2_060: %d grouped units.", n_units))
