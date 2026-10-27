# CONAPO marginalization (IMM) workbooks, the municipal geography lookup, and
# the grouped-unit marginalization index.
#
# Orientation: CONAPO's IMN is HIGHER for LESS marginalized municipalities
# (cor(IMN, illiterate) ~ -0.85). GM, the categorical grade, is correctly oriented.
# =============================================================================

# Read one IMM workbook with standardised column names and without its national
# total row. `year` selects the IM_/IMN_/GM_ suffix. 2020 is .xls with a 5-row
# preamble (skip = 5); the DP2 2010/2015 files are .xlsx with a named sheet.
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
    tidyr::drop_na(IM) |>
    dplyr::filter(!is.na(mun))
}

# The three CONAPO IMM rounds (2020 .xls, 2015/2010 DP2 .xlsx), stacked with a
# numeric `year`, keeping `cols`. Rows in order 2020, 2015, 2010.
load_imm_rounds <- function(cols, dir = "input-data-raw/marginalization") {
  read_round <- function(file, year, ...) {
    load_imm(file.path(dir, file), year, ...) |>
      dplyr::mutate(IMN = as.numeric(IMN), year = year) |>
      dplyr::select(dplyr::all_of(cols))
  }
  dplyr::bind_rows(read_round("IMM_2020.xls", 2020, skip = 5),
                   read_round("IMM_DP2_2015.xlsx", 2015, sheet = "IMM_2015"),
                   read_round("IMM_DP2_2010.xlsx", 2010, sheet = "IMM_2010"))
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
# ORPHAN_MARG selects the group-level marginalization files chapter 3/5 read:
# "conapo" (default; ch2_060, CONAPO IMN 2010-2020) or "fw" (ch2_061,
# fixed-weight index 1990-2020). Figures for "fw" go to "<dir>_fw".
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

# Extend a group_id x year table to `years`: the first observed year is carried
# backwards and the last forwards, only into years the table does not have.
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
# Grade cut-points: the [min, max] IMN of each CONAPO grade (GM) over the
# municipalities in `df`.
gm_thresholds <- function(df) {
  df |>
    dplyr::filter(!is.na(GM)) |>
    dplyr::group_by(GM) |>
    dplyr::summarise(min_value = min(IMN, na.rm = TRUE), max_value = max(IMN, na.rm = TRUE)) |>
    dplyr::arrange(min_value)
}

# Assign an English grade to IMN values `x` from gm_thresholds(). Overlapping
# ranges resolve to the first match, most marginalized first. `open_ended`
# also grades values beyond the extremes of the reference municipalities.
classify_gm <- function(x, thresholds, open_ended = FALSE) {
  lo <- function(g) thresholds$min_value[thresholds$GM == g]
  hi <- function(g) thresholds$max_value[thresholds$GM == g]
  out <- dplyr::case_when(
    x >= lo("Muy alto") & x <= hi("Muy alto") ~ "Very High",
    x >= lo("Alto")     & x <= hi("Alto")     ~ "High",
    x >= lo("Medio")    & x <= hi("Medio")    ~ "Medium",
    x >= lo("Bajo")     & x <= hi("Bajo")     ~ "Low",
    x >= lo("Muy bajo") & x <= hi("Muy bajo") ~ "Very Low",
    TRUE ~ NA_character_)
  if (!open_ended) return(out)
  dplyr::case_when(x < lo("Muy alto") ~ "Very High",
                   x > hi("Muy bajo") ~ "Very Low",
                   TRUE ~ out)
}

# Municipal marginalization (mun, year, IMN, GM, population) -> grouped units.
# Returns the intermediate tables ch2_060 / ch2_061 write:
#   grouped    : municipal rows with group_id (unmatched municipalities dropped)
#   thresholds : grade cut-points, from `threshold_years` (all years if NULL)
#   classified : unweighted mean IMN per unit-year, graded
#   imputed    : population-weighted IMN per unit, interpolated linearly over
#                `years` (`rule` as in approx(); a unit with one round is constant)
#   index      : `imputed` graded, with mpi renamed to IMN
group_marg_index <- function(mun_index, years, rule = 1, threshold_years = NULL,
                             open_ended = FALSE, what = "group_marg_index") {
  grouped <- attach_group_id(mun_index, what = what)
  thresholds <- gm_thresholds(if (is.null(threshold_years)) grouped
                              else dplyr::filter(grouped, year %in% threshold_years))
  classified <- grouped |>
    dplyr::group_by(group_id, year) |>
    dplyr::summarise(IMN = mean(IMN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(GM = classify_gm(IMN, thresholds, open_ended))
  imputed <- grouped |>
    dplyr::group_by(group_id, year) |>
    dplyr::summarise(mpi = weighted.mean(IMN, w = population), .groups = "drop") |>
    tidyr::complete(group_id, year = years) |>
    dplyr::group_by(group_id) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::group_modify(~ {
      obs <- dplyr::filter(.x, !is.na(mpi))
      .x$mpi <- if (nrow(obs) >= 2) stats::approx(obs$year, obs$mpi, xout = .x$year, rule = rule)$y
                else if (nrow(obs) == 1) rep(obs$mpi, nrow(.x)) else NA_real_
      .x
    }) |>
    dplyr::ungroup()
  index <- imputed |>
    dplyr::mutate(GM = classify_gm(mpi, thresholds, open_ended)) |>
    dplyr::rename(IMN = mpi)
  list(grouped = grouped, thresholds = thresholds, classified = classified,
       imputed = imputed, index = index)
}

# Marginalization quintile of IMN (1 = lowest IMN = MOST marginalized, since
# higher IMN means less marginalized) with English grade labels.
add_marg_quintile <- function(df) {
  df |>
    dplyr::mutate(
      marginalization_quintile = dplyr::ntile(as.numeric(IMN), 5),
      mpi_label = factor(c("Very High", "High", "Medium", "Low", "Very Low")[marginalization_quintile],
                         levels = c("Very Low", "Low", "Medium", "High", "Very High")))
}
