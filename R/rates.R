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

compute_std_fert_rate <- function(data) {
  # Set fertility-specific column names
  ctg <- "tot_births"
  rte <- "fert_rate"

  data <- .drop_unstandardisable(data, rte, "compute_std_fert_rate")

  # Step 1: Build a reference population by group/sex/age
  pop_ref <- data |>
    dplyr::select(group_id, sex, age, population) |>
    group_by(group_id, sex, age) |>
    summarise(population = mean(population, na.rm = TRUE), .groups = "drop")

  # Step 2: Compute national standard population (by sex and age)
  p_nat <- pop_ref |>
    group_by(sex, age) |>
    summarise(p_nat = sum(population, na.rm = TRUE), .groups = "drop")

  p_nat_total <- sum(p_nat$p_nat, na.rm = TRUE)
  
  # Step 3: Calculate standardized fertility rate
  std_rate <- data |>
    left_join(p_nat, by = c("sex", "age")) |>
    mutate(std_rate = (p_nat / p_nat_total) * !!sym(rte)) |>
    group_by(group_id, year, sex) |>
    summarise(std_fert_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
  
  return(std_rate)
}

#-------------------------------------------------------------------------------
compute_std_mort_rate <- function(data) {
  ctg <- "tot_deaths"
  rte <- "mort_rate"

  data <- .drop_unstandardisable(data, rte, "compute_std_mort_rate")

  # Step 1: Build a reference population by group/sex/age (averaged over the
  # years present; the old comment claiming "from 2023" was never what the code
  # did).
  pop_ref <- data |>
    dplyr::select(group_id, sex, age, population) |>
    group_by(group_id, sex, age) |>
    summarise(population = mean(population, na.rm = TRUE), .groups = "drop")

  # Step 2: Build national standard population
  p_nat <- pop_ref |>
    group_by(sex, age) |>
    summarise(p_nat = sum(population, na.rm = TRUE), .groups = "drop")

  p_nat_total <- sum(p_nat$p_nat, na.rm = TRUE)
  
  # Step 3: Compute standardised mortality rate
  std_rate <- data |>
    left_join(p_nat, by = c("sex", "age")) |>
    mutate(std_rate = (p_nat / p_nat_total) * !!sym(rte)) |>
    group_by(group_id, year, sex) |>
    summarise(std_mort_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
  
  return(std_rate)
}

#-------------------------------------------------------------------------------


# Age-sex standardised rate per grouped municipality, by age band.
# `ref_year` was hardcoded to 2018 here and to 2023 in the national counterpart,
# so the municipal and national rates plotted side by side in ch3_030 were
# standardised to DIFFERENT populations. Both now default to 2020.
compute_std_rate_age_gender <- function (data, ref_year = 2020, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")
  rte <- ifelse("deaths" %in% cns, "mort_rate", "fert_rate")

  data <- .drop_unstandardisable(data, rte, "compute_std_rate_age_gender")
  ref  <- .std_ref_pop(data, ref_year, "compute_std_rate_age_gender")

  my_clmns <- c("group_id", "year", "age", "sex", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = ref$p_nat, by = c("age", "sex"))
  std_rate <- std_rate %>% mutate(p_nat_total = ref$total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(group_id, age, sex, year) %>%
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

#-------------------------------------------------------------------------------
min_max_norm <- function(x) {
  # TODO: check that reconstruction okay — original helper was not ported.
  # Assumed to be standard min-max scaling to [0, 1].
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#-------------------------------------------------------------------------------
compute_std_rate <- function(data, ref_year = 2020) {
  # TODO: check that reconstruction okay — original helper was not ported (the
  # compute_std_rate in ch1_030 uses a different schema). Reconstructed as a
  # direct age-sex standardisation of municipality death/birth rates to the
  # national `ref_year` population. Returns one row per (mun, year) with column
  # `std_mort_rate` (deaths) or `std_fert_rate` (births), matching plot_std_rate().
  is_mort <- "deaths" %in% names(data)
  cnt <- if (is_mort) "deaths" else "births"
  out <- if (is_mort) "std_mort_rate" else "std_fert_rate"
  ref <- data |>
    dplyr::filter(year == ref_year) |>
    dplyr::group_by(sex, age) |>
    dplyr::summarise(ref_pop = sum(population, na.rm = TRUE), .groups = "drop")
  tot_ref <- sum(ref$ref_pop, na.rm = TRUE)
  data |>
    dplyr::mutate(.rate = .data[[cnt]] / population) |>
    dplyr::left_join(ref, by = c("sex", "age")) |>
    dplyr::group_by(mun, year) |>
    dplyr::summarise("{out}" := sum(.rate * ref_pop, na.rm = TRUE) / tot_ref,
                     .groups = "drop")
}
