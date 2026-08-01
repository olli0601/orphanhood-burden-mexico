# =============================================================================
# ch1_015_bootstrap_from_processed.R  ·  Chapter 1 — Bootstrap from processed data
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
source("R/load_year_panels.R")
source("R/marginalization.R")
})

proc <- "input-data-processed"

# --- 1. Combine supplied per-year files (original-municipality level) --------
# Loading goes through load_year_panels(), the same helper ch2_040/ch2_050/ch2_060
# use. This script used to inline its own version, which differed in one respect
# that matters: it mapped the 2-digit occurrence year with 1900 + year for every
# value, turning the `99` UNKNOWN-YEAR sentinel into a real occurrence year 1999.
# That silently moved 28,346 deaths (registered 1990-1997) into 1999, where they
# also implied a negative registration delay. load_year_panels() drops the
# sentinel instead. Fertility is unaffected — no fert_* file carries a 99.
# `year_min`: ch2_060 used to rewrite these two files with NO lower year bound,
# and its version is the one that survived, so the deaths panel on disk spans
# 1900-2024. ch2_060 no longer writes them, so the bound is set here — and set
# to preserve exactly what was there before, rather than silently dropping rows
# from analyses that have been reading them.

load_years <- function(subdir, value_col, year_min = 1985, year_max = 2024) {
  load_year_panels(subdir, dir = proc) |>
    mutate(year = as.integer(year), year_reg = as.integer(year_reg)) |>
    filter(year >= year_min & year <= year_max) |>
    group_by(year, sex, age, mun, year_reg) |>
    summarise("{value_col}" := sum(.data[[value_col]]), .groups = "drop")
}
births <- load_years("fertility datasets", "births")
deaths <- load_years("mortality datasets", "deaths", year_min = -Inf)
saveRDS(births, file.path(proc, "births.RDS"))
saveRDS(deaths, file.path(proc, "deaths.RDS"))
cat(sprintf("births: %d rows (%d-%d) | deaths: %d rows (%d-%d)\n",
            nrow(births), min(births$year), max(births$year),
            nrow(deaths), min(deaths$year), max(deaths$year)))

# --- 2. Population (from CONAPO quinquennial workbook) -----------------------
# ADULT bands only (15-19 .. 75-79). ch2_060 writes the 0-79 version under
# population_mun.RDS; these are different objects and no longer share a name.
population <- load_population(keep_child = FALSE)
saveRDS(population, file.path(proc, "population_mun_adult.RDS"))
cat(sprintf("population_mun_adult: %d rows\n", nrow(population)))

# --- 3. Marginalization index + geo_info (from CONAPO IMM 2020) --------------
# Both are MUNICIPALITY-level. ch2_060's group-level counterparts are
# index_grouped_mun.RDS and geo_info_mun_grouped.RDS — previously all four
# collided on two names, so a file's meaning depended on how far the pipeline
# had run.
index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)
index <- index[-1, ]
saveRDS(index, file.path(proc, "marg_index_mun.RDS"))

geo_info <- build_geo_info(index)
saveRDS(geo_info, file.path(proc, "geo_info_mun.RDS"))
cat(sprintf("geo_info_mun: %d municipalities | marg_index_mun: %d\n", nrow(geo_info), nrow(index)))

cat("\nBootstrap complete. Intermediates written to input-data-processed/.\n")


