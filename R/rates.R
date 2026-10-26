# Standardised rate computation helpers
# Helper functions split from the former R/utils.R
# =============================================================================

# --- internal: guard rails for age-sex standardisation -----------------------
# Rows that cannot contribute to an age-standardised rate are removed BEFORE the
# standard population is built. This matters because the reference total is a
# single scalar: with no na.rm, one missing population value makes p_nat_total
# NA, and `p_nat / p_nat_total` then turns EVERY standardised rate into NA -
# silently, producing empty figures rather than an error.
#
# In practice this bites mortality: preprocess_mortality() deliberately keeps
# unknown-age deaths (INEGI EDAD 4998/4999) as age = NA, which have no
# population to join to. Fertility escaped only because ch3_030 happens to call
# drop_na() on its fertility frame.
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

# --- reference (standard) population -----------------------------------------
# Direct standardisation needs one fixed age-sex population. Build it from a
# POPULATION table (one row per unit x year x sex x age), never from an events
# frame: births/deaths frames carry one row per registration year, so summing
# their population column counts each cell once per year_reg row (about 5x for
# young mothers in 2020, 2.3x at 45-49), and cells with no events drop out
# entirely (female 50-54: 225 of 777 units). The old .std_ref_pop() did exactly
# that. `ref_year` defaults to 2020, the only reference the thesis names (§4.1.2);
# if absent, the nearest year is used and reported.
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

# --- internal: directly standardised rate per unit-year-sex -------------------
# Shared body of compute_std_mort_rate() / compute_std_fert_rate(), which were
# identical apart from the rate column and the output name.
#
# `unit_col` is the spatial unit to report by. It defaults to "group_id" (the
# ch2 aggregated units) but accepts "mun", which is what makes an ungrouped
# baseline comparable with a grouped run: both then come from this one code path
# rather than from two implementations whose difference would contaminate every
# before/after-aggregation comparison.
#
# `std_pop` is the standard (reference) population: a data frame of sex, age,
# p_nat. When NULL it is built here as before - the mean population per
# unit-sex-age over the years present, summed over units. Pass one explicitly
# when comparing several aggregation levels: the sums are algebraically equal,
# but .drop_unstandardisable() removes a different row set at each level, so
# leaving it implicit makes the levels quietly non-comparable.
#
# Weights are normalised WITHIN SEX (decided Oct 2026): each per-sex value is a
# rate per woman / per man of parental age, standardised to that sex's age
# structure. Until then the total spanned both sexes, so a per-sex value carried
# only its sex's share of the weight mass (female fertility came out at 0.463 x
# the per-woman rate on the grouped panels).
#
# `by_sex = FALSE` gives the both-sex rate instead: one weight total over every
# (sex, age), summed over sex, returned with sex = "both". Mortality only - for
# fertility `sex` is the parent's sex and every birth is on both a mother's and a
# father's row, so a both-sex fertility rate double-counts. Pass mother rows for
# per-woman fertility.
.std_rate_by_unit <- function(data, rte, out_col, unit_col, std_pop, what,
                              by_sex = TRUE) {
  data <- .drop_unstandardisable(data, rte, what)

  if (is.null(std_pop)) {
    # Reference population by unit/sex/age, averaged over the years present (the
    # old comment claiming "from 2023" was never what the code did), then summed
    # to a national standard population.
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

#-------------------------------------------------------------------------------
compute_std_mort_rate <- function(data, unit_col = "group_id", std_pop = NULL,
                                  by_sex = TRUE) {
  .std_rate_by_unit(data, "mort_rate", "std_mort_rate", unit_col, std_pop,
                    "compute_std_mort_rate", by_sex)
}

#-------------------------------------------------------------------------------


# Age-sex standardised rate per grouped municipality, by age band.
# `std_pop` is required: build it with std_ref_population() from the population
# table (see above). Weights are normalised within sex. Note the open question
# (backlog): grouped by age, each band's value is just its age-specific rate x
# that band's weight, not a standardised rate.
# `unit_col` mirrors compute_std_mort_rate(): "group_id" by default, "mun" for an
# ungrouped baseline.
compute_std_rate_age_gender <- function (data, std_pop, unit_col = "group_id", ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")
  rte <- ifelse("deaths" %in% cns, "mort_rate", "fert_rate")

  data <- .drop_unstandardisable(data, rte, "compute_std_rate_age_gender")
  w    <- .std_weights(std_pop, data, "compute_std_rate_age_gender")

  data |>
    dplyr::select(dplyr::all_of(c(unit_col, "year", "age", "sex", ctg, "population", rte))) |>
    dplyr::mutate(sex = as.character(sex), age = as.character(age)) |>
    dplyr::left_join(w, by = c("age", "sex")) |>
    dplyr::mutate(std_rate = w * .data[[rte]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(unit_col, "age", "sex", "year")))) |>
    dplyr::summarise(std_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
}

#-------------------------------------------------------------------------------
# National age-sex standardised rate by age band. Same contract as above.
compute_national_std_rate_age_gender <- function (data, std_pop, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")
  rte <- ifelse("deaths" %in% cns, "mort_rate", "fert_rate")

  data <- .drop_unstandardisable(data, rte, "compute_national_std_rate_age_gender")
  w    <- .std_weights(std_pop, data, "compute_national_std_rate_age_gender")

  data |>
    dplyr::select(dplyr::all_of(c("year", "age", "sex", ctg, "population", rte))) |>
    dplyr::mutate(sex = as.character(sex), age = as.character(age)) |>
    dplyr::left_join(w, by = c("age", "sex")) |>
    dplyr::mutate(std_rate = w * .data[[rte]]) |>
    dplyr::group_by(age, sex, year) |>
    dplyr::summarise(std_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
}

#-------------------------------------------------------------------------------

compute_rate <- function (count, pop, ...) { 
  r <- (count / pop) 
  r[is.nan(r)] <- 0 # 0/0 
  # r[is.infinite(r)] <- 0 # x/0, x > 0
  r
}

#-------------------------------------------------------------------------------
range_0_1 <- function (x, ...) { (x - min(x)) / (max(x) - min(x)) }


#-------------------------------------------------------------------------------
