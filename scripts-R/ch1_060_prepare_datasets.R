# =============================================================================
# ch1_060_prepare_datasets.R  ·  Chapter 1 — Analytical panel builder
# Master assembly script: re-aggregate births/deaths/population/marginalization
# to the new (merged) municipalities, build the long birth/death panels, the
# monthly-birth nowcasting input, and the area-type lookups.
#
# Reads : input-data-raw/ (per-year births/deaths, IMM 2010/2015/2020, type_of_mun),
#         input-data-processed/{grouped_municipality_50000, aggregated_muni_50000,
#         population_new_mun, index_new_mun}.RDS (ch2)
# Writes: input-data-processed/{deaths, births, *_new_mun, geo_info*, population*,
#         mpi_*, marg_index, month_*, monthly_births.parquet, rural_urban_area.parquet}
# Run after: ch1_050, ch2_010
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
#############################################

# MORTALITY

#############################################
# ---- DEATHS: load the supplied per-year mortality (preprocess_mortality output)
mortality_rds_files <- list.files("input-data-processed/mortality datasets", pattern = "[.]RDS$", full.names = TRUE)
for (file in mortality_rds_files) {
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  d <- readRDS(file); if (!"year_reg" %in% names(d)) d$year_reg <- year_reg
  assign(paste0("mort_", year_reg), d)
}
for (nm in intersect(paste0("mort_", 1985:1997), ls(pattern = "^mort_[0-9]{4}$"))) assign(nm, correct_year(get(nm)))
deaths <- bind_rows(mget(ls(pattern = "^mort_[0-9]{4}$"))) |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop")
saveRDS(deaths, file = "input-data-processed/deaths.RDS")
rm(list = ls(pattern = "^mort_[0-9]{4}$"))

# ---- mort_df (single-age, orphans) and deaths_df_long require the raw INEGI
#      files via preprocess_orphans() / preprocess_mortality_long(); the supplied
#      per-year RDS do not carry these. Skipped here (feed Chapter 5).
message("ch1_060: skipping mort_df / deaths_df_long (need raw orphans/long preprocessing).")


grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
aggregated_muni_50000 <- readRDS("input-data-processed/aggregated_muni_50000.RDS")
################################################################################

#---------------------------------- GEO INFO ----------------------------------

################################################################################

IMM_2020 <- read_excel("input-data-raw/marginalization/IMM_2020.xls", skip = 5)

index <- IMM_2020 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2020,
    IMN = IMN_2020,
    GM = GM_2020
  ) |> drop_na(IM)

index <- index[-1,]

geo_info <- index |> dplyr::select(state, state_name, mun, mun_name)
colSums(is.na(geo_info))
capitals <- c("Aguascalientes", "Mexicali", "La Paz", "Campeche", "Tuxtla Gutiérrez", 
              "Chihuahua", "Saltillo", "Colima", "Durango", "Guanajuato", 
              "Chilpancingo de los Bravo", "Pachuca de Soto", "Guadalajara",
              "Toluca", "Morelia", "Cuernavaca", "Tepic", "Monterrey", "Oaxaca de Juárez", 
              "Puebla", "Querétaro", "Othón P. Blanco", "San Luis Potosí", 
              "Culiacán", "Hermosillo", "Centro", "Victoria", 
              "Tlaxcala", "Xalapa", "Mérida", "Zacatecas")

geo_info <- geo_info |> mutate(
  capital = if_else(mun_name %in% capitals, 1, 0)
)

geo_info$capital[geo_info$state_name == "Guanajuato" & geo_info$mun_name == "Victoria"] <- 0

geo_info <- geo_info %>%
  left_join(grouped_municipality_50000 |> sf::st_drop_geometry() |> select(mun, group_id), by = "mun") |>
  filter(!is.na(group_id))

# Geometry per new municipality = the aggregated polygons (already unioned by
# group_id in ch2); attributes (state, capital) summarised from geo_info.
geo_attrs <- geo_info |> group_by(group_id) |>
  summarise(state_name = first(state_name), state = first(state), capital = first(capital), .groups = "drop")
geo_info_new_mun <- aggregated_muni_50000 |> select(group_id) |> left_join(geo_attrs, by = "group_id")
geo_info_new_mun$capital[geo_info_new_mun$group_id %in% c("06002", "30087")] <- 1


saveRDS(geo_info, file = "input-data-processed/geo_info.RDS")
saveRDS(geo_info_new_mun, file = "input-data-processed/geo_info_new_mun.RDS")

################################################################################

#--------------------------------- POPULATION ----------------------------------

################################################################################
X1_Grupo_Quinq_00_RM <- read_excel("input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx")

population <- X1_Grupo_Quinq_00_RM |> rename(
  mun = CLAVE,
  state = CLAVE_ENT,
  mun_name = NOM_MUN,
  state_name = NOM_ENT,
  sex = SEXO,
  year = AÑO
) |> pivot_longer(
  cols = starts_with("POB_"), values_to = "population", names_to = "age"
) |> mutate(
  sex = case_when(
    sex == "HOMBRES" ~ "male",
    sex == "MUJERES" ~ "female"
  )
)

population$mun <- str_pad(population$mun, 5, pad = "0")

population <- population |> filter(year >= 1990 & year < 2024)

population <- population |>
  mutate(
    age = str_replace(age, "POB_", "")
  ) |>
  mutate(
    age = str_replace(age, "_", "-")
  ) |> filter(age != "TOTAL" & age!= "85-mm" & age != "80-84")

population$sex <- as.factor(population$sex)
population$age <- as.factor(population$age)
population$mun <- as.factor(population$mun)

population <- population |>
  left_join(geo_info |>
              select(mun, group_id) |>
              sf::st_drop_geometry(), by = "mun")

population_new_mun <- population |> 
  group_by(group_id, year, sex, age) |>
  summarise(population = sum(population), .groups = "drop")

saveRDS(population %>% dplyr::select(-dplyr::any_of("group_id")), file = "input-data-processed/population.RDS")
saveRDS(population_new_mun, file = "input-data-processed/population_new_mun.RDS")


################################################################################

#------------------------------------- MPI -------------------------------------

################################################################################

IMM_2020 <- read_excel("input-data-raw/marginalization/IMM_2020.xls", skip = 5)
IMM_2015 <- read_excel("input-data-raw/marginalization/IMM_DP2_2015.xlsx", sheet = "IMM_2015")
IMM_2010 <- read_excel("input-data-raw/marginalization/IMM_DP2_2010.xlsx", sheet = "IMM_2010")

index_2020 <- IMM_2020 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2020,
    IMN = IMN_2020,
    GM = GM_2020
  ) |> drop_na(IM)

index_2015 <- IMM_2015 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2015,
    IMN = IMN_2015,
    GM = GM_2015
  ) |> drop_na(IM)

index_2010 <- IMM_2010 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2010,
    IMN = IMN_2010,
    GM = GM_2010
  ) |> drop_na(IM)

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
grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
grouped_municipality_50000 <- grouped_municipality_50000 |>
  st_drop_geometry() |>
  select(mun, group_id)

# Perform the join
combined_index_new_mun <- left_join(
  combined_index,
  grouped_municipality_50000,
  by = "mun"
)

mpi_new_mun <- combined_index_new_mun |>
  group_by(group_id, year) |>
  summarise(mpi = weighted.mean(IMN, w = population), .groups = "drop")

thresholds <- combined_index_new_mun |>
  group_by(GM) |>
  summarise(
    min_value = min(IMN, na.rm = TRUE),
    max_value = max(IMN, na.rm = TRUE)
  ) |>
  arrange(min_value)

classified_data <- combined_index_new_mun %>%
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
index <- mpi_new_mun %>%
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

saveRDS(index, "input-data-processed/mpi_new_mun.RDS")

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

# index_new_mun = group-level classified marginalization (group_id, year, IMN, GM);
# mpi_imputed = the interpolated MPI (group_id, year, mpi). Both consumed by ch3.
saveRDS(index, file = "input-data-processed/index_new_mun.RDS")
# index_marg: one population-weighted marginalization value per new municipality
index_marg <- combined_index |>
  left_join(geo_info |> dplyr::select(mun, group_id), by = "mun") |>
  filter(!is.na(group_id)) |>
  group_by(group_id) |>
  summarise(marg_index_weighted = weighted.mean(as.numeric(IMN), population, na.rm = TRUE),
            .groups = "drop")
saveRDS(index_marg, file = "input-data-processed/index_marg.RDS")
file.copy("input-data-processed/mpi_new_mun.RDS",
          "input-data-processed/mpi_imputed.RDS", overwrite = TRUE)
# index_marg / im.tmp (mun-level detail) feed Chapter 5; need fuller mun-level MPI.
message("ch1_060: skipping index_marg / im.tmp (need fuller mun-level MPI).")
