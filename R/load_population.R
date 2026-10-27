# CONAPO municipal population workbook in long format: mun x sex x 5-year age
# band x year, 1990-2023.
#   keep_child: keep 00-04 and 05-09 (child survival, Chapter 5)
#   keep_old  : keep 80-84 and 85-mm
# Both TRUE gives total population (the workbook's POB_TOTAL), which is what the
# ch2_010 aggregation threshold counts.
# =============================================================================
load_population <- function(keep_child = FALSE, keep_old = FALSE,
                            path = "input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx") {
  drop_age <- "TOTAL"
  if (!keep_child) drop_age <- c(drop_age, "00-04", "05-09")
  if (!keep_old)   drop_age <- c(drop_age, "80-84", "85-mm")
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
