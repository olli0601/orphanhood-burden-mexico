# marginalization.R
# Shared helpers for the CONAPO marginalization workbooks and the geographic
# lookup, isolated from the duplicated inline blocks in ch1_005/ch2_040/ch2_060.
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

# --- Marginalization variant switch -----------------------------------------
# ORPHAN_MARG selects which group-level marginalization files the chapter 3/5
# consumers read: "conapo" (default; ch2_060, CONAPO IMN 2010-2020) or "fw"
# (ch2_061, fixed-weight index 1990-2020 from ch2_055). Figures for "fw" go to
# a sibling "<dir>_fw" folder so both variants can be compared side by side.
#   ORPHAN_MARG=fw pixi run Rscript scripts-R/ch3_010_...R
marg_variant <- function() {
  v <- Sys.getenv("ORPHAN_MARG", "conapo")
  if (!v %in% c("conapo", "fw")) stop("ORPHAN_MARG must be 'conapo' or 'fw', not '", v, "'")
  v
}
marg_file <- function(name) {
  suffix <- if (marg_variant() == "fw") "_fw" else ""
  f <- file.path("input-data-processed", paste0(name, suffix, ".RDS"))
  if (!file.exists(f)) stop(f, " not found (variant '", marg_variant(), "'; run ",
                            if (suffix == "") "ch2_060" else "ch2_061", ")")
  f
}
marg_fig_dir <- function(dir) {
  out <- if (marg_variant() == "fw") paste0(dir, "_fw") else dir
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out
}

# Extend a group_id x year marginalization table to `years`, carrying the first
# observed year backwards and the last forwards, ONLY into years the table does
# not already have. Replaces an inline bind_rows(2010 -> 1990:2009, 2020 ->
# 2021:2023) that assumed coverage started in 2010 and would duplicate rows (and
# so double-count events in the join) for an index that already covers 1990.
extend_marg_years <- function(index, years) {
  index <- dplyr::mutate(index, year = as.integer(year))
  all_years <- sort(union(unique(index$year), as.integer(years)))
  index |>
    tidyr::complete(group_id, year = all_years) |>
    dplyr::group_by(group_id) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(-c(group_id, year), .direction = "downup") |>
    dplyr::ungroup()
}
