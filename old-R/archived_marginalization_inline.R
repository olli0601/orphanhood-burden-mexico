# old-R/archived_marginalization_inline.R
# IMM read+rename and geo_info-build blocks removed from ch1_005/040/050/060,
# replaced by R/load_imm() and R/build_geo_info(). Kept for reference.
# ============================================================================

# --- ch1_005_bootstrap_from_processed.R geo_info ---
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

# --- ch1_040_clean_mort.R ---
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

# --- ch1_040_clean_mort.R geo_info ---
geo_info <- index |> dplyr::select(state, state_name, mun, mun_name)

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

# --- ch1_050_clean_fert.R ---
index <- IMM_2020 |> dplyr::select("CVE_ENT", "NOM_ENT", "CVE_MUN", "NOM_MUN","POB_TOT",
                                   "IM_2020", "IMN_2020", "GM_2020") |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    IM = IM_2020,
    IMN = IMN_2020,
    GM = GM_2020
  ) |> drop_na(IM)

# --- ch1_060_prepare_datasets.R ---
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

# --- ch1_060_prepare_datasets.R ---
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

# --- ch1_060_prepare_datasets.R ---
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

# --- ch1_060_prepare_datasets.R ---
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

# --- ch1_060_prepare_datasets.R geo_info ---
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
