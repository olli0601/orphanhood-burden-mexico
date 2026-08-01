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

# --- internal: reference (standard) population -------------------------------
# Direct standardisation needs one fixed age-sex population. `ref_year` defaults
# to 2020, the project's documented reference population (cf. compute_std_rate()
# below and docs/nowcasting_pipeline.md). If that year is absent from `data` -
# e.g. ch3_030 filters to 2010-2020 while the old code hardcoded 2023 - fall
# back to the nearest available year and say so, instead of building an empty
# reference and returning all-NA.
.std_ref_pop <- function(data, ref_year = 2020, what = "standardisation") {
  yrs <- sort(unique(as.integer(data$year)))
  use <- as.integer(ref_year)
  if (!use %in% yrs) {
    if (!length(yrs)) stop(what, ": no usable rows to build a reference population from.")
    use <- yrs[which.min(abs(yrs - use))]
    message(sprintf("%s: reference year %s not present (data covers %s-%s); using %s.",
                    what, ref_year, min(yrs), max(yrs), use))
  }
  p_nat <- data |>
    dplyr::filter(as.integer(year) == use) |>
    dplyr::group_by(sex, age) |>
    dplyr::summarise(p_nat = sum(population, na.rm = TRUE), .groups = "drop")
  list(p_nat = p_nat, total = sum(p_nat$p_nat, na.rm = TRUE), year = use)
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
# ⚠️ WHAT THIS RETURNS, AND AN OPEN QUESTION FOR THE RESEARCHERS.
# The weights are p_nat / p_nat_total where the TOTAL spans every (sex, age)
# present in `data`, but the sum below runs over ages WITHIN one sex. So when
# both sexes are passed, a per-sex value carries only its own share of the
# weight mass (~46% female / ~54% male on these panels) and is NOT a rate per
# woman or per man - it is that sex's contribution to a rate per head of the
# combined parental-age population. Consequences:
#
#   * MORTALITY: summing the two sexes gives the whole rate. Correct, and what
#     ch2_020 and ch2_040 do.
#   * FERTILITY: pass MOTHER ROWS ONLY. `sex` is the parent's sex and every
#     birth appears on both the mother's and the father's row, so summing sexes
#     double-counts. With female-only input the weights sum to 1 over the female
#     bands and the result is births per woman, which is what you want.
#
# Whether the per-sex figures should instead be normalised within sex - making
# each a true per-woman / per-man rate, and changing every published LEVEL - is
# a definitional choice for the researchers, not a bug. Nothing here changes it;
# the thesis never states which is intended (it defines no standardisation
# method at all). Flagged in docs/audit/thesis_vs_code_backlog.md.
.std_rate_by_unit <- function(data, rte, out_col, unit_col, std_pop, what) {
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
  p_nat_total <- sum(std_pop$p_nat, na.rm = TRUE)

  data |>
    dplyr::left_join(std_pop, by = c("sex", "age")) |>
    dplyr::mutate(std_rate = (p_nat / p_nat_total) * .data[[rte]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(unit_col, "year", "sex")))) |>
    dplyr::summarise("{out_col}" := sum(std_rate, na.rm = TRUE), .groups = "drop")
}

compute_std_fert_rate <- function(data, unit_col = "group_id", std_pop = NULL) {
  .std_rate_by_unit(data, "fert_rate", "std_fert_rate", unit_col, std_pop,
                    "compute_std_fert_rate")
}

#-------------------------------------------------------------------------------
compute_std_mort_rate <- function(data, unit_col = "group_id", std_pop = NULL) {
  .std_rate_by_unit(data, "mort_rate", "std_mort_rate", unit_col, std_pop,
                    "compute_std_mort_rate")
}

#-------------------------------------------------------------------------------


# Age-sex standardised rate per grouped municipality, by age band.
# `ref_year` was hardcoded to 2018 here and to 2023 in the national counterpart,
# so the municipal and national rates plotted side by side in ch3_030 were
# standardised to DIFFERENT populations. Both now default to 2020.
# `unit_col` mirrors compute_std_mort_rate(): "group_id" by default, "mun" for an
# ungrouped baseline.
compute_std_rate_age_gender <- function (data, ref_year = 2020, unit_col = "group_id", ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")
  rte <- ifelse("deaths" %in% cns, "mort_rate", "fert_rate")

  data <- .drop_unstandardisable(data, rte, "compute_std_rate_age_gender")
  ref  <- .std_ref_pop(data, ref_year, "compute_std_rate_age_gender")

  my_clmns <- c(unit_col, "year", "age", "sex", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = ref$p_nat, by = c("age", "sex"))
  std_rate <- std_rate %>% mutate(p_nat_total = ref$total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(across(all_of(c(unit_col, "age", "sex", "year")))) %>%
    summarise(std_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")

  std_rate
}

#-------------------------------------------------------------------------------
# National age-sex standardised rate by age band. See the note above on ref_year:
# this previously hardcoded 2023, which ch3_030's 2010-2020 filter excludes
# entirely, so the reference population was empty and every rate came back NA.
compute_national_std_rate_age_gender <- function (data, ref_year = 2020, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")
  rte <- ifelse("deaths" %in% cns, "mort_rate", "fert_rate")

  data <- .drop_unstandardisable(data, rte, "compute_national_std_rate_age_gender")
  ref  <- .std_ref_pop(data, ref_year, "compute_national_std_rate_age_gender")

  my_clmns <- c("year", "age", "sex", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = ref$p_nat, by = c("age", "sex"))
  std_rate <- std_rate %>% mutate(p_nat_total = ref$total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(age, sex, year) %>%
    summarise(std_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")

  std_rate
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
