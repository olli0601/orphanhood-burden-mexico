# marginalization.R
# Shared helpers for the CONAPO marginalization workbooks and the geographic
# lookup, isolated from the duplicated inline blocks in ch1_005/ch1_040/ch1_060.
# =============================================================================

# Read a CONAPO IMM workbook and standardise its column names. `year` selects the
# IM_/IMN_/GM_ suffix (2010, 2015, 2020). 2020 is .xls with a 5-row preamble
# (skip = 5); the DP2 2010/2015 files are .xlsx with a named sheet.
load_imm <- function(path, year, sheet = NULL, skip = 0) {
  raw <- if (is.null(sheet)) readxl::read_excel(path, skip = skip) else
                              readxl::read_excel(path, sheet = sheet)
  yr <- stats::setNames(paste0(c("IM_", "IMN_", "GM_"), year), c("IM", "IMN", "GM"))
  raw |>
    dplyr::rename(
      mun = CVE_MUN, state = CVE_ENT, mun_name = NOM_MUN, state_name = NOM_ENT,
      population = POB_TOT, illiterate_pct = ANALF, no_basic_edu_pct = SBASC,
      no_drainage_pct = OVSDE, no_electricity_pct = OVSEE, no_piped_water_pct = OVSAE,
      dirt_floors_pct = OVPT, overcrowding_pct = VHAC, small_towns_pct = `PL.5000`,
      low_income_pct = PO2SM) |>
    dplyr::rename(!!!yr) |>
    tidyr::drop_na(IM)
}

# Municipality lookup (state/mun names) with a binary `capital` flag, from a
# renamed IMM index (output of load_imm()).
build_geo_info <- function(index) {
  capitals <- c("Aguascalientes", "Mexicali", "La Paz", "Campeche", "Tuxtla Gutiérrez",
                "Chihuahua", "Saltillo", "Colima", "Durango", "Guanajuato",
                "Chilpancingo de los Bravo", "Pachuca de Soto", "Guadalajara",
                "Toluca", "Morelia", "Cuernavaca", "Tepic", "Monterrey", "Oaxaca de Juárez",
                "Puebla", "Querétaro", "Othón P. Blanco", "San Luis Potosí",
                "Culiacán", "Hermosillo", "Centro", "Victoria",
                "Tlaxcala", "Xalapa", "Mérida", "Zacatecas")
  geo <- index |>
    dplyr::select(state, state_name, mun, mun_name) |>
    dplyr::mutate(capital = dplyr::if_else(mun_name %in% capitals, 1, 0))
  geo$capital[geo$state_name == "Guanajuato" & geo$mun_name == "Victoria"] <- 0
  geo
}
