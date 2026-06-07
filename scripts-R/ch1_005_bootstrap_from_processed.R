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
X1_Grupo_Quinq_00_RM <- read_excel(
  "input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx")
population <- X1_Grupo_Quinq_00_RM |>
  rename(mun = CLAVE, state = CLAVE_ENT, mun_name = NOM_MUN,
         state_name = NOM_ENT, sex = SEXO, year = AÑO) |>
  pivot_longer(cols = starts_with("POB_"), values_to = "population", names_to = "age") |>
  mutate(sex = case_when(sex == "HOMBRES" ~ "male", sex == "MUJERES" ~ "female")) |>
  mutate(mun = str_pad(mun, 5, pad = "0")) |>
  filter(year >= 1990 & year < 2024) |>
  mutate(age = str_replace(str_replace(age, "POB_", ""), "_", "-")) |>
  filter(!age %in% c("TOTAL", "00-04", "05-09", "85-mm", "80-84")) |>
  mutate(sex = as.factor(sex), age = as.factor(age), mun = as.factor(mun))
saveRDS(population, file.path(proc, "population.RDS"))
cat(sprintf("population: %d rows\n", nrow(population)))

# --- 3. Marginalization index + geo_info (from CONAPO IMM 2020) --------------
IMM_2020 <- read_excel("input-data-raw/marginalization/IMM_2020.xls", skip = 5)
index <- IMM_2020 |>
  rename(mun = CVE_MUN, state = CVE_ENT, mun_name = NOM_MUN, state_name = NOM_ENT,
         population = POB_TOT, illiterate_pct = ANALF, no_basic_edu_pct = SBASC,
         no_drainage_pct = OVSDE, no_electricity_pct = OVSEE, no_piped_water_pct = OVSAE,
         dirt_floors_pct = OVPT, overcrowding_pct = VHAC, small_towns_pct = `PL.5000`,
         low_income_pct = PO2SM, IM = IM_2020, IMN = IMN_2020, GM = GM_2020) |>
  drop_na(IM)
index <- index[-1, ]
saveRDS(index, file.path(proc, "marg_index.RDS"))

geo_info <- index |> dplyr::select(state, state_name, mun, mun_name)
capitals <- c("Aguascalientes", "Mexicali", "La Paz", "Campeche", "Tuxtla Gutiérrez",
              "Chihuahua", "Saltillo", "Colima", "Durango", "Guanajuato",
              "Chilpancingo de los Bravo", "Pachuca de Soto", "Guadalajara",
              "Toluca", "Morelia", "Cuernavaca", "Tepic", "Monterrey", "Oaxaca de Juárez",
              "Puebla", "Querétaro", "Othón P. Blanco", "San Luis Potosí",
              "Culiacán", "Hermosillo", "Centro", "Victoria",
              "Tlaxcala", "Xalapa", "Mérida", "Zacatecas")
geo_info <- geo_info |> mutate(capital = if_else(mun_name %in% capitals, 1, 0))
geo_info$capital[geo_info$state_name == "Guanajuato" & geo_info$mun_name == "Victoria"] <- 0
saveRDS(geo_info, file.path(proc, "geo_info.RDS"))
cat(sprintf("geo_info: %d municipalities | marg_index: %d\n", nrow(geo_info), nrow(index)))

cat("\nBootstrap complete. Intermediates written to input-data-processed/.\n")
