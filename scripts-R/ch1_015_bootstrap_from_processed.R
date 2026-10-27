# =============================================================================
# ch1_015_bootstrap_from_processed.R  ·  Chapter 1 — Bootstrap from processed data
# Municipal-level intermediates from the per-year panels, the CONAPO population
# workbook and IMM 2020: the inputs Chapter 2 starts from.
#
# Reads : input-data-processed/{fertility,mortality} datasets/*.RDS,
#         input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx,
#         input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/{births, deaths, population_mun_adult, marg_index_mun,
#         geo_info_mun}.RDS
# Run after: ch1_012 (or ch1_020); before ch2_010.
# =============================================================================
suppressMessages({
  library(dplyr)
  source("R/load_population.R")
  source("R/load_year_panels.R")
  source("R/marginalization.R")
})

proc <- "input-data-processed"

# --- 1. Births and deaths, summed over duplicate cells ----------------------
# Births 1985-2024 (unknown parental age redistributed); deaths keep every
# occurrence year (back to 1900).
load_years <- function(panels, value_col, year_min = 1985, year_max = 2024) {
  panels |>
    mutate(year = as.integer(year), year_reg = as.integer(year_reg)) |>
    filter(year >= year_min & year <= year_max) |>
    group_by(year, sex, age, mun, year_reg) |>
    summarise("{value_col}" := sum(.data[[value_col]]), .groups = "drop")
}
births <- load_years(load_birth_panels(dir = proc), "births")
deaths <- load_years(load_year_panels("mortality datasets", dir = proc), "deaths", year_min = -Inf)
saveRDS(births, file.path(proc, "births.RDS"))
saveRDS(deaths, file.path(proc, "deaths.RDS"))
cat(sprintf("births: %d rows (%d-%d) | deaths: %d rows (%d-%d)\n",
            nrow(births), min(births$year), max(births$year),
            nrow(deaths), min(deaths$year), max(deaths$year)))

# --- 2. Population, adult bands 15-79 (ch2_060 writes the 0-79 population_mun) ---
population <- load_population(keep_child = FALSE)
saveRDS(population, file.path(proc, "population_mun_adult.RDS"))
cat(sprintf("population_mun_adult: %d rows\n", nrow(population)))

# --- 3. IMM 2020 and the municipality lookup -----------------------------------
index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)
saveRDS(index, file.path(proc, "marg_index_mun.RDS"))

geo_info <- build_geo_info(index)
saveRDS(geo_info, file.path(proc, "geo_info_mun.RDS"))
cat(sprintf("geo_info_mun: %d municipalities | marg_index_mun: %d\n", nrow(geo_info), nrow(index)))

cat("\nBootstrap complete. Intermediates written to input-data-processed/.\n")


