# load_population.R
# Read + reshape the CONAPO municipal population workbook to long format
# (mun x sex x 5-year age group x year, 1990-2023). Single source for the block
# that was previously duplicated across ch1_005 / ch1_040 / ch1_060.
#   keep_child = FALSE  drop the 0-4 and 5-9 bands (adult panels: fertility/mortality)
#   keep_child = TRUE   retain them (needed for child survival in Chapter 5)
# =============================================================================
load_population <- function(keep_child = FALSE,
                            path = "input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx") {
  drop_age <- c("TOTAL", "85-mm", "80-84")
  if (!keep_child) drop_age <- c(drop_age, "00-04", "05-09")
  readxl::read_excel(path) |>
    dplyr::rename(mun = CLAVE, state = CLAVE_ENT, mun_name = NOM_MUN,
                  state_name = NOM_ENT, sex = SEXO, year = `AÑO`) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("POB_"),
                        values_to = "population", names_to = "age") |>
    dplyr::mutate(sex = dplyr::case_when(sex == "HOMBRES" ~ "male",
                                         sex == "MUJERES" ~ "female"),
                  mun = stringr::str_pad(mun, 5, pad = "0")) |>
    dplyr::filter(year >= 1990 & year < 2024) |>
    dplyr::mutate(age = stringr::str_replace(stringr::str_replace(age, "POB_", ""), "_", "-")) |>
    dplyr::filter(!age %in% drop_age) |>
    dplyr::mutate(sex = as.factor(sex), age = as.factor(age), mun = as.factor(mun))
}
