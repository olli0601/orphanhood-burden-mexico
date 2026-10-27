# Rates and direct age-sex standardisation.
# =============================================================================

# Rows that cannot be age-standardised: unknown age (mortality keeps INEGI EDAD
# 4998/4999 as age = NA), missing or zero population, non-finite rate. Removed
# before the reference total is built, because one NA in it turns every
# standardised rate into NA.
.drop_unstandardisable <- function(data, rte = NULL, what = "standardisation") {
  keep <- !is.na(data$age) & !is.na(data$population) & data$population > 0
  if (!is.null(rte) && rte %in% names(data)) keep <- keep & is.finite(data[[rte]])
  keep[is.na(keep)] <- FALSE
  n_drop <- sum(!keep)
  if (n_drop > 0) {
    message(sprintf(
      "%s: dropped %d of %d rows with unknown age / missing population (cannot be age-standardised).",
      what, n_drop, nrow(data)))
  }
  data[keep, , drop = FALSE]
}

# Standard population (sex, age, p_nat) for `ref_year` (2020, as in thesis
# §4.1.2; the nearest year if absent). Takes a POPULATION table, one row per
# unit x year x sex x age: an events frame repeats population once per
# registration year and omits cells with no events.
std_ref_population <- function(population, ref_year = 2020) {
  if ("year_reg" %in% names(population))
    stop("std_ref_population: got an events frame (has year_reg); pass the population table.")
  keys <- intersect(c("group_id", "mun", "year", "sex", "age"), names(population))
  if (anyDuplicated(as.data.frame(population)[keys]))
    stop("std_ref_population: duplicate ", paste(keys, collapse = " x "), " rows; ",
         "population would be counted more than once.")
  yrs <- sort(unique(as.integer(as.character(population$year))))
  use <- as.integer(ref_year)
  if (!use %in% yrs) {
    use <- yrs[which.min(abs(yrs - use))]
    message(sprintf("std_ref_population: reference year %s not present; using %s.", ref_year, use))
  }
  population |>
    dplyr::filter(as.integer(as.character(year)) == use, !is.na(age)) |>
    dplyr::mutate(sex = as.character(sex), age = as.character(age)) |>
    dplyr::group_by(sex, age) |>
    dplyr::summarise(p_nat = sum(population, na.rm = TRUE), .groups = "drop")
}

# Within-sex weights over the (sex, age) cells present in `data`, so the age
# range of the reference matches that of the rates (e.g. female 15-54 only).
.std_weights <- function(std_pop, data, what) {
  cells <- dplyr::distinct(data, sex = as.character(sex), age = as.character(age))
  w <- std_pop |>
    dplyr::semi_join(cells, by = c("sex", "age")) |>
    dplyr::group_by(sex) |>
    dplyr::mutate(w = p_nat / sum(p_nat)) |>
    dplyr::ungroup() |>
    dplyr::select(sex, age, w)
  miss <- dplyr::anti_join(cells, w, by = c("sex", "age"))
  if (nrow(miss)) stop(what, ": no reference population for ",
                       paste(miss$sex, miss$age, collapse = ", "))
  w
}

# Directly standardised rate per unit x year x sex, from the age-specific rate
# column `rte`.
#   unit_col : "group_id" (grouped units) or "mun" (ungrouped baseline)
#   std_pop  : standard population (sex, age, p_nat). If NULL: mean population
#              per unit-sex-age over the years present, summed over units. Pass
#              one explicitly to compare aggregation levels, since the rows
#              dropped as unstandardisable differ between levels.
#   by_sex   : TRUE normalises weights within sex (a rate per woman / per man);
#              FALSE gives one both-sex rate (sex = "both"). Mortality only: for
#              fertility every birth is on a mother's and a father's row.
.std_rate_by_unit <- function(data, rte, out_col, unit_col, std_pop, what,
                              by_sex = TRUE) {
  data <- .drop_unstandardisable(data, rte, what)

  if (is.null(std_pop)) {
    std_pop <- data |>
      dplyr::select(dplyr::all_of(c(unit_col, "sex", "age")), population) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(unit_col, "sex", "age")))) |>
      dplyr::summarise(population = mean(population, na.rm = TRUE), .groups = "drop") |>
      dplyr::group_by(sex, age) |>
      dplyr::summarise(p_nat = sum(population, na.rm = TRUE), .groups = "drop")
  }
  w <- std_pop |>
    dplyr::group_by(dplyr::across(dplyr::any_of(if (by_sex) "sex"))) |>
    dplyr::mutate(w = p_nat / sum(p_nat, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(sex, age, w)

  out <- data |>
    dplyr::left_join(w, by = c("sex", "age")) |>
    dplyr::mutate(std_rate = w * .data[[rte]])
  if (!by_sex) out <- dplyr::mutate(out, sex = "both")
  out |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(unit_col, "year", "sex")))) |>
    dplyr::summarise("{out_col}" := sum(std_rate, na.rm = TRUE), .groups = "drop")
}

compute_std_fert_rate <- function(data, unit_col = "group_id", std_pop = NULL) {
  .std_rate_by_unit(data, "fert_rate", "std_fert_rate", unit_col, std_pop,
                    "compute_std_fert_rate")
}

compute_std_mort_rate <- function(data, unit_col = "group_id", std_pop = NULL,
                                  by_sex = TRUE) {
  .std_rate_by_unit(data, "mort_rate", "std_mort_rate", unit_col, std_pop,
                    "compute_std_mort_rate", by_sex)
}

# Each age band's contribution to the standardised rate (age-specific rate x
# within-sex weight), per unit x age x sex x year; with `unit_col = NULL`,
# national. `std_pop` from std_ref_population(). Open question (backlog):
# per band this is a weighted rate, not a standardised one.
.std_rate_by_age <- function(data, std_pop, unit_col, what) {
  ctg <- if ("deaths" %in% colnames(data)) "deaths" else "births"
  rte <- if ("deaths" %in% colnames(data)) "mort_rate" else "fert_rate"
  data <- .drop_unstandardisable(data, rte, what)
  w    <- .std_weights(std_pop, data, what)
  data |>
    dplyr::select(dplyr::all_of(c(unit_col, "year", "age", "sex", ctg, "population", rte))) |>
    dplyr::mutate(sex = as.character(sex), age = as.character(age)) |>
    dplyr::left_join(w, by = c("age", "sex")) |>
    dplyr::mutate(std_rate = w * .data[[rte]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(unit_col, "age", "sex", "year")))) |>
    dplyr::summarise(std_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
}

compute_std_rate_age_gender <- function(data, std_pop, unit_col = "group_id", ...) {
  .std_rate_by_age(data, std_pop, unit_col, "compute_std_rate_age_gender")
}

compute_national_std_rate_age_gender <- function(data, std_pop, ...) {
  .std_rate_by_age(data, std_pop, NULL, "compute_national_std_rate_age_gender")
}

# National counts, population and rate per year x age x sex. `df` must hold
# one row per cell (see collapse_year_reg()), else population is counted once
# per registration year. Rows with any NA are dropped.
national_rates <- function(df, count_col, out_col, rate_col, na_rm = TRUE) {
  df |>
    dplyr::group_by(year, age, sex) |>
    dplyr::summarise("{out_col}" := sum(.data[[count_col]], na.rm = na_rm),
                     population = sum(population, na.rm = na_rm), .groups = "drop") |>
    dplyr::mutate("{rate_col}" := .data[[out_col]] / population) |>
    tidyr::drop_na()
}

# Drop women 65-79 and men 80-84. preprocess_mortality() already keeps only
# women 15-64 and men 15-84, so on the death panels this removes men 80-84 and
# leaves parental-age mortality for women 15-64, men 15-79; on the birth panels
# (mothers to 50, fathers to 75-79) it removes nothing.
drop_nonparental_ages <- function(df) {
  dplyr::filter(df, !(sex == "female" & age %in% c("65-69", "70-74", "75-79")),
                !(sex == "male" & age == "80-84"))
}
