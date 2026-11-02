# Orphanhood engine (thesis §5.3): fertility -> expected surviving children per
# parent -> orphan events from parental deaths -> new-orphan incidence ->
# lifetime prevalence. Shared by ch5_020 (point estimates) and, later, ch5_030
# (bootstrap), so both run the same model.
#
# Conventions: `group_id` is the unit, `year` the calendar year, `sex` the
# parent's sex, `age` a single-year age (integer) unless named `band`
# ("15-19"...), `child_age` 0-17. Data tables throughout.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# Fertility ages are bounds on the parent's age AT THE CHILD'S BIRTH. A parent
# who dies can be older: up to the fertility bound + 17, capped by the oldest
# band with deaths (women 60-64) and by §5.3.5's exclusion of ages >= 80.
ORPHAN_FERT_AGES   <- list(female = 15:50, male = 15:79)
ORPHAN_PARENT_AGES <- list(female = 15:64, male = 15:79)
ORPHAN_FIRST_YEAR  <- 1990L   # first year with fertility data
ORPHAN_CHILD_AGES  <- 0:17

band_lo <- function(band) as.integer(sub("^(\\d+)-.*$", "\\1", band))
band_hi <- function(band) as.integer(sub("^.*-(\\d+)$", "\\1", band))
age_band <- function(a) {
  lo <- (a %/% 5L) * 5L
  sprintf("%02d-%02d", lo, lo + 4L)
}

# Uniformly split 5-year bands into single years, dividing `value_col` by the
# band width. Rows with no parseable band (age = NA) are dropped.
split_to_single <- function(df, value_col) {
  d <- as.data.table(df)[!is.na(age)]
  d[, `:=`(a0 = band_lo(as.character(age)), a1 = band_hi(as.character(age)), .r = .I)]
  d <- d[rep(.r, a1 - a0 + 1L)]
  d[, age := NULL][, age := a0 + rowid(.r) - 1L]
  d[, (value_col) := get(value_col) / (a1 - a0 + 1L)]
  d[, c("a0", "a1", ".r") := NULL][]
}

ages_table <- function(ages) {
  rbindlist(lapply(names(ages), function(s) data.table(sex = s, age = ages[[s]])))
}

# Single-year population, from the band population table (uniform within band).
single_year_population <- function(population) {
  split_to_single(as.data.table(population)[, .(group_id, year = as.integer(year), sex,
                                                age, population)], "population")
}

# --- Eq (7): fertility rates on a complete grid -------------------------------
# births: group_id, year, sex, age (band), births; summed over any other column
# (e.g. year_reg). INEGI top-codes mothers at 50, so the female "50-54" band is
# "50+" and goes to age 50 rather than being spread over 50-54. Every
# (unit, year, sex, age) with population > 0 gets a rate, zero if no births.
fertility_rates <- function(births, pop1, fert_ages = ORPHAN_FERT_AGES) {
  b <- as.data.table(births)[!is.na(age), .(births = sum(births)),
                             by = .(group_id, year = as.integer(year), sex, age)]
  top <- b[sex == "female" & age == "50-54"]
  b1  <- split_to_single(b[!(sex == "female" & age == "50-54")], "births")
  b1  <- rbind(b1, top[, .(group_id, year, sex, age = 50L, births)])
  b1  <- b1[, .(births = sum(births)), by = .(group_id, year, sex, age)]

  grid <- pop1[ages_table(fert_ages), on = .(sex, age), nomatch = 0][population > 0]
  out <- b1[grid, on = .(group_id, year, sex, age)]
  out[is.na(births), births := 0]
  # Births outside the population years (occurrence before 1990, or 2024) are
  # out of scope by design; report only losses inside them.
  in_years <- b1[year %in% unique(pop1$year), sum(births)]
  lost <- in_years - sum(out$births)
  if (lost > 1e-6 * in_years)
    message(sprintf("fertility_rates: %.0f births (%.3f%%) in %d-%d outside the fertility ages or with no population.",
                    lost, 100 * lost / in_years, min(pop1$year), max(pop1$year)))
  out[, fert_rate := births / population][]
}

# --- Eqs (8)-(9): one-year child survival -------------------------------------
# child_deaths: [group_id,] year, sex, age (0-17), deaths. child_pop: same keys,
# population. Sexes are pooled as a ratio of sums (Eq 8), not by averaging two
# survival curves. `by_unit = FALSE` gives the national curve.
child_survival_1yr <- function(child_deaths, child_pop, by_unit = FALSE) {
  k <- c(if (by_unit) "group_id", "year", "age")
  d <- as.data.table(child_deaths)[, .(deaths = sum(deaths)), by = k]
  p <- as.data.table(child_pop)[, .(population = sum(population)), by = k]
  s <- d[p, on = k]
  s[is.na(deaths), deaths := 0]
  s[, s1 := 1 - pmin(deaths / population, 1)][]
}

# Per-unit survival by indirect standardisation: unit relative risk by age group
# (0, 1-4, 5-17) over a centred `window`-year span, shrunk towards the national
# level by a gamma-Poisson empirical-Bayes prior (Marshall 1991 moments),
# applied to the national single-age, year-specific hazard.
child_survival_unit_eb <- function(child_deaths, child_pop, window = 5L) {
  nat <- child_survival_1yr(child_deaths, child_pop, by_unit = FALSE)
  nat[, h_nat := 1 - s1]
  d <- as.data.table(child_deaths)[, .(deaths = sum(deaths)), by = .(group_id, year, age)]
  p <- as.data.table(child_pop)[, .(population = sum(population)), by = .(group_id, year, age)]
  u <- d[p, on = .(group_id, year, age)]
  u[is.na(deaths), deaths := 0]
  u <- nat[, .(year, age, h_nat)][u, on = .(year, age)]
  u[, `:=`(expected = population * h_nat,
           grp = fcase(age == 0L, "0", age <= 4L, "1-4", default = "5-17"))]
  ug <- u[, .(O = sum(deaths), E = sum(expected)), by = .(group_id, year, grp)]
  h <- (window - 1L) %/% 2L
  setorder(ug, group_id, grp, year)
  win <- function(x) Reduce(`+`, lapply(-h:h, function(k) shift(x, k, fill = 0)))
  ug[, `:=`(Ow = win(O), Ew = win(E)), by = .(group_id, grp)]   # years contiguous per unit
  ug[, c("alpha", "beta") := {
    m  <- sum(Ow) / sum(Ew)
    r  <- Ow / Ew
    s2 <- sum(Ew * (r - m)^2) / sum(Ew) - m / mean(Ew)
    if (!is.finite(s2) || s2 <= 0) list(Inf, Inf) else list(m^2 / s2, m / s2)
  }, by = .(year, grp)]
  ug[, rr := fifelse(is.finite(alpha), (Ow + alpha) / (Ew + beta), 1)]
  out <- ug[, .(group_id, year, grp, rr)][u, on = .(group_id, year, grp)]
  out[, s1 := 1 - pmin(h_nat * rr, 1)]
  out[, .(group_id, year, age, deaths, population, rr, s1)][]
}

# --- Eq (10) survival term: cumulative survival to the end of age c -----------
# S(t, c) = prod_{j=0}^{c} s1(t + j, j) along the birth cohort t = Y - c, as in
# the Nature Medicine definition: born in Y - c and alive at the start of age
# c + 1. Missing one-year values (outside the data years) are taken as 1.
cohort_survival <- function(s1) {
  k <- intersect("group_id", names(s1))
  d <- as.data.table(s1)[, c(k, "year", "age", "s1"), with = FALSE]
  grid <- CJ(birth_year = ORPHAN_FIRST_YEAR:max(d$year), child_age = ORPHAN_CHILD_AGES)
  if (length(k)) grid <- grid[, .(group_id = unique(d$group_id)), by = .(birth_year, child_age)]
  grid[, `:=`(year = birth_year + child_age, age = child_age)]
  grid <- d[grid, on = c(k, "year", "age")]
  grid[is.na(s1), s1 := 1]
  setorderv(grid, c(k, "birth_year", "child_age"))
  grid[, S := cumprod(s1), by = c(k, "birth_year")]
  grid[, c(k, "birth_year", "child_age", "S"), with = FALSE][]
}

# --- Eqs (10)-(11): expected surviving children per parent, by current band ---
# C(a, c) = F_{Y-c}(a-c) * S_{Y-c}(c); zero when a-c is outside the fertility
# ages or Y-c is before the first fertility year. Banded by the parent's
# CURRENT age with population weights N_Y(a).
expected_children <- function(fert, S, pop1, parent_ages = ORPHAN_PARENT_AGES) {
  parents <- pop1[ages_table(parent_ages), on = .(sex, age), nomatch = 0]
  F <- fert[, .(group_id, birth_year = year, sex, p = age, fert_rate)]
  by_unit_S <- "group_id" %in% names(S)
  out <- vector("list", length(ORPHAN_CHILD_AGES))
  for (c in ORPHAN_CHILD_AGES) {
    x <- parents[, .(group_id, year, sex, age, population,
                     birth_year = year - c, p = age - c, child_age = c)]
    x <- F[x, on = .(group_id, birth_year, sex, p)]
    x <- if (by_unit_S) S[x, on = .(group_id, birth_year, child_age)]
         else S[x, on = .(birth_year, child_age)]
    x[, C := fifelse(is.na(fert_rate) | is.na(S), 0, fert_rate * S)]
    out[[c + 1L]] <- x[, .(C = if (sum(population) > 0) sum(population * C) / sum(population) else 0,
                           parents = sum(population)),
                       by = .(group_id, year, sex, band = age_band(age), child_age)]
  }
  rbindlist(out)
}

# --- §5.3.5: parental deaths by band ------------------------------------------
# deaths: grouped panel (group_id, year, sex, age band, [year_reg], count col).
# Unknown ages dropped; bands kept where the whole band is a parent age.
parental_deaths <- function(deaths, count_col = "tot_deaths", parent_ages = ORPHAN_PARENT_AGES) {
  d <- as.data.table(deaths)[!is.na(age) & !is.na(group_id),
                             .(deaths = sum(get(count_col))),
                             by = .(group_id, year = as.integer(year), sex, band = age)]
  keep <- ages_table(parent_ages)[, .(band = unique(age_band(age))), by = sex]
  d[keep, on = .(sex, band), nomatch = 0]
}

# --- Eqs (13), (15): adult hazards by band ------------------------------------
adult_hazards <- function(deaths_band, population) {
  p <- as.data.table(population)[, .(group_id, year = as.integer(year), sex, band = age, population)]
  h <- deaths_band[p, on = .(group_id, year, sex, band), nomatch = 0]
  h[, `:=`(h5 = 1 - exp(-5 * deaths / population))]
  h[, h1 := 1 - (1 - h5)^(1 / 5)]
  h[, .(group_id, year, sex, band, h5, h1)][]
}

# --- Eqs (12), (14), (16), (17): orphan events and new-orphan incidence -------
# O_prev is the exact sum over the c-1 earlier years of the opposite-sex 1-year
# hazard at that year and at the band the other parent was in (taking the
# other parent to be the dying parent's band midpoint, i - years younger).
# Eq (16)'s (c-1) * h1(current) approximation is returned as O_prev_eq16 for
# comparison. Missing hazards are 0 (§5.3.6). O_new is formed per parent sex,
# so the sexes add up to the total.
orphan_events <- function(C, D, hz) {
  e <- C[D, on = .(group_id, year, sex, band), nomatch = 0]
  e[, O_death := C * deaths]
  e[, opp := fifelse(sex == "male", "female", "male")]
  e <- hz[, .(group_id, year, opp = sex, band, h5_opp = h5, h1_opp = h1)][
    e, on = .(group_id, year, opp, band)]
  e[is.na(h5_opp), `:=`(h5_opp = 0, h1_opp = 0)]
  e[, O_double := h5_opp * O_death]
  e[, O_prev_eq16 := pmax(child_age - 1L, 0L) * h1_opp * O_death]

  mid <- band_lo(e$band) + 2L
  e[, H_prev := 0]
  h1 <- hz[, .(group_id, year, opp = sex, band, h1)]
  for (i in 1:(max(ORPHAN_CHILD_AGES) - 1L)) {
    sel <- e$child_age > i
    if (!any(sel)) next
    look <- data.table(row = which(sel), group_id = e$group_id[sel], year = e$year[sel] - i,
                       opp = e$opp[sel], band = age_band(pmax(mid[sel] - i, 0L)))
    look <- h1[look, on = .(group_id, year, opp, band)]
    look[is.na(h1), h1 := 0]
    e[look$row, H_prev := H_prev + look$h1]
  }
  e[, O_prev := H_prev * O_death]
  e[, O_new := O_death - 0.5 * O_double - O_prev]
  e[, .(group_id, year, sex, band, child_age, C, deaths, O_death, O_double, O_prev,
        O_prev_eq16, O_new)][]
}

# --- Eqs (18)-(19): lifetime prevalence, per unit -----------------------------
# Along each birth cohort: L(Y, c) = L(Y-1, c-1) * s1(Y-1, c-1) + O_new(Y, c),
# i.e. events of earlier years carried forward by one-year survival from the
# age and year they occurred. `by` adds keys (e.g. "sex").
prevalence <- function(O_new, s1, by = character(0)) {
  k  <- c("group_id", by)
  n  <- O_new[, .(O_new = sum(O_new)), by = c(k, "year", "child_age")]
  sk <- intersect("group_id", names(s1))
  s  <- as.data.table(s1)[, c(sk, "year", "age", "s1"), with = FALSE]
  last <- max(n$year)
  out  <- vector("list", length(ORPHAN_CHILD_AGES))
  prev <- NULL
  for (c in ORPHAN_CHILD_AGES) {
    cur <- n[child_age == c, c(k, "year", "O_new"), with = FALSE]
    if (c > 0L) {
      carry <- s[age == c - 1L][prev, on = c(sk, "year")]
      carry[is.na(s1), s1 := 1]
      carry <- carry[year < last, .(carry = L * s1), by = c(k, "year")]
      carry[, year := year + 1L]
      cur <- merge(cur, carry, by = c(k, "year"), all = TRUE)
      cur[is.na(O_new), O_new := 0][is.na(carry), carry := 0]
      cur[, L := carry + O_new][, carry := NULL]
    } else {
      cur[, L := O_new]
    }
    prev <- cur[, c(k, "year", "L"), with = FALSE]
    out[[c + 1L]] <- cur[, child_age := c]
  }
  setnames(rbindlist(out, use.names = TRUE), "L", "O_life")[]
}
