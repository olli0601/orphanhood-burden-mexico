# =============================================================================
# ch1_005_bootstrap_from_processed.R  ·  Chapter 1 — Bootstrap from processed data
# Build the deterministic intermediates needed to start the pipeline from the
# SUPPLIED per-year datasets (input-data-processed/{fertility,mortality}
# datasets/) instead of the raw INEGI files. Produces the inputs that
# Chapter 2 (municipality aggregation) and later chapters consume.
#
# Reads : input-data-processed/{fertility,mortality} datasets/*.RDS,
#         input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx,
#         input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/{births, deaths, population, geo_info, marg_index}.RDS
# Run    : after ch1_010 (raw download); before ch2_010.
# =============================================================================
suppressMessages({
  library(dplyr); library(tidyr); library(stringr); library(readxl); library(purrr)
source("R/load_population.R")
source("R/marginalization.R")
})

proc <- "input-data-processed"

# --- 1. Combine supplied per-year files (original-municipality level) --------
load_years <- function(subdir, value_col) {
  files <- list.files(file.path(proc, subdir), pattern = "[.]RDS$", full.names = TRUE)
  stopifnot(length(files) > 0)
  map_dfr(files, readRDS) |>
    # early files (1985-1997) store occurrence year as 2 digits (e.g. 90); fix to 19xx
    mutate(year = ifelse(year < 100, 1900L + as.integer(year), as.integer(year))) |>
    filter(year >= 1985 & year <= 2024) |>
    group_by(year, sex, age, mun, year_reg) |>
    summarise("{value_col}" := sum(.data[[value_col]]), .groups = "drop")
}
births <- load_years("fertility datasets", "births")
deaths <- load_years("mortality datasets", "deaths")
saveRDS(births, file.path(proc, "births.RDS"))
saveRDS(deaths, file.path(proc, "deaths.RDS"))
cat(sprintf("births: %d rows | deaths: %d rows\n", nrow(births), nrow(deaths)))

# --- 2. Population (from CONAPO quinquennial workbook) -----------------------
population <- load_population(keep_child = FALSE)
saveRDS(population, file.path(proc, "population.RDS"))
cat(sprintf("population: %d rows\n", nrow(population)))

# --- 3. Marginalization index + geo_info (from CONAPO IMM 2020) --------------
index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)
index <- index[-1, ]
saveRDS(index, file.path(proc, "marg_index.RDS"))

geo_info <- build_geo_info(index)
saveRDS(geo_info, file.path(proc, "geo_info.RDS"))
cat(sprintf("geo_info: %d municipalities | marg_index: %d\n", nrow(geo_info), nrow(index)))

cat("\nBootstrap complete. Intermediates written to input-data-processed/.\n")


