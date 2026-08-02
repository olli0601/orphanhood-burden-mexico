# =============================================================================
# ch2_020_grouping_diagnostics.R  ·  Chapter 2 — Choosing the aggregation threshold
#
# Thesis Section 2.4 states that the 50,000-inhabitant threshold was chosen to
# minimise the coefficient of variation of age-standardised death rates over a
# 10,000-100,000 sweep. No such sweep existed in this repository, and nothing
# measured dispersion before vs after aggregation at all — so the stated
# rationale for aggregating (avoid zero counts, reduce year-on-year standard
# deviations) was undemonstrated. This script implements it, and widens it,
# because CV alone cannot arbitrate the trade-off: aggregating always reduces
# noise, so a criterion that only rewards low noise is minimised by aggregating
# the whole country into one unit.
#
# Three families of metrics are computed for each candidate threshold, plus an
# ungrouped (municipality-level) baseline:
#
#   (a) stability   - what aggregation buys: CV and year-on-year first-difference
#                     SD of the age-standardised rates.
#   (b) sparsity    - "too many sparse event counts", quantified: share of
#                     unit x year x sex x age cells with zero or small counts.
#   (c) heterogeneity - what aggregation costs: how much between-unit variation
#                     in rates, deprivation and rural/urban composition is
#                     absorbed into within-unit averaging.
#
# THIS SCRIPT DOES NOT PICK A THRESHOLD. Which point on the trade-off to take is
# a statistical decision for the researchers; the output is the evidence.
#
# Reads : input-data-processed/{mex_muni, deaths, births}.RDS (ch2_010, ch1_015),
#         input-data-raw/population/.../1_Grupo_Quinq_00_RM.xlsx (via load_population),
#         input-data-raw/marginalization/IMM_2020.xls
# Writes: output/ch2/ch2_020_grouping_diagnostics.{RDS,csv},
#         output/ch2/ch2_020_grouping_diagnostics_wide.csv,
#         output/ch2/ch2_020_*.pdf
# Run after: ch2_010. Independent of ch2_040/ch2_050/ch2_060 — it reproduces
#         ch2_060's group_by(group_id) roll-ups in memory rather than re-running
#         that script seven times, which would overwrite population.RDS and
#         geo_info.RDS with a different vintage each time.
# =============================================================================

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
source("R/municipality_grouping.R")
source("R/load_population.R")
source("R/marginalization.R")
source("R/rates.R")

# --- knobs -------------------------------------------------------------------
THRESHOLDS     <- c(10, 20, 30, 40, 50, 75, 100) * 1000
ANALYSIS_YEARS <- 2010:2020   # IMM 2020 anchors the deprivation covariates.
POP_YEAR       <- 2020        # vintage the threshold is evaluated against (ch2_010)

# 2020 carries the COVID mortality shock, and occurrence years near the end of
# the series are still filling in through registration delay (Chapters 3-4). Set
# SENSITIVITY_YEARS to NULL to skip the second pass.
SENSITIVITY_YEARS <- 2010:2019

fig_dir <- "output/ch2"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. Inputs
# =============================================================================
mex_muni <- readRDS("input-data-processed/mex_muni.RDS")
stopifnot(nrow(mex_muni) == 2457,
          !anyNA(mex_muni$mun),
          sum(mex_muni$total_pop, na.rm = TRUE) > 1.2e8)
mex_muni$total_pop <- ifelse(is.na(mex_muni$total_pop), 0, mex_muni$total_pop)
mun_pop <- st_drop_geometry(mex_muni) |> select(mun, total_pop)

# Deaths: sum over registration year (the delay structure is Chapter 3's
# subject, not this one). `age = NA` is a real category in mortality (INEGI
# EDAD 4998/4999) with no population to join to — dropped here, as it must be
# before any age standardisation.
deaths <- readRDS("input-data-processed/deaths.RDS") |>
  mutate(mun = as.character(mun), year = as.integer(year)) |>
  filter(year %in% ANALYSIS_YEARS, !is.na(age), !is.na(sex)) |>
  group_by(mun, year, sex, age) |>
  summarise(deaths = sum(deaths), .groups = "drop")

# Births: `sex` is the PARENT's sex and every birth appears twice, once on the
# mother's row and once on the father's. Mother rows are the meaningful figure.
births <- readRDS("input-data-processed/births.RDS") |>
  mutate(mun = as.character(mun), year = as.integer(year)) |>
  filter(year %in% ANALYSIS_YEARS, !is.na(age), sex == "female") |>
  group_by(mun, year, sex, age) |>
  summarise(births = sum(births), .groups = "drop")

population <- load_population(keep_child = FALSE, keep_old = TRUE) |>
  mutate(mun = as.character(mun), year = as.integer(year),
         sex = as.character(sex), age = as.character(age)) |>
  filter(year %in% ANALYSIS_YEARS)

# The analytical universe is the 2,457 GADM polygons ch2_010 groups. The CONAPO
# workbook and the event panels carry a handful of codes outside it; left
# unfiltered they join to group_id = NA and surface as one enormous spurious
# unit (CLAUDE.md flags this class of bug). Drop them explicitly and say how many.
universe <- unique(as.character(mex_muni$mun))
drop_outside <- function(df, what) {
  keep <- df$mun %in% universe
  if (any(!keep)) {
    message(sprintf("ch2_020: %s - dropped %d of %d rows for %d municipality codes outside the %d grouped polygons.",
                    what, sum(!keep), nrow(df), n_distinct(df$mun[!keep]), length(universe)))
  }
  df[keep, , drop = FALSE]
}
population <- drop_outside(population, "population")
deaths     <- drop_outside(deaths, "deaths")
births     <- drop_outside(births, "births")

# The population grid, not the event tables, defines what cells exist. Absent
# event rows are structural zeros: computing a zero-share over only the rows
# that happen to be present would make sparsity *improve* as units merge, which
# is the exact opposite of the truth.
mort_ages <- sort(unique(as.character(deaths$age)))
fert_ages <- sort(unique(as.character(births$age)))
grid_mort <- population |> filter(age %in% mort_ages)
grid_fert <- population |> filter(age %in% fert_ages, sex == "female")

message(sprintf("ch2_020: %d municipalities, %s-%s, %d mortality age bands, %d fertility age bands.",
                n_distinct(population$mun), min(ANALYSIS_YEARS), max(ANALYSIS_YEARS),
                length(mort_ages), length(fert_ages)))

# Marginalization + rural/urban, municipality level. Weighted exactly as
# ch2_060 does it (IMM POB_TOT as the weight, small_towns_pct >= 50 => Rural).
imm <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)
imm <- imm[-1, ] |>
  transmute(mun = as.character(mun),
            imm_pop = as.numeric(population),
            IMN = as.numeric(IMN),
            GM  = as.character(GM),
            small_towns_pct = as.numeric(small_towns_pct)) |>
  mutate(area_type = if_else(small_towns_pct >= 50, "Rural", "Urban"))
# CAUTION (see CLAUDE.md): in CONAPO's workbooks higher IMN means LESS
# marginalized. GM is correctly oriented. Every IMN-signed quantity below says
# so at the point of use.
GM_ORDER <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")   # least -> most deprived
imm$GM <- factor(imm$GM, levels = GM_ORDER)
stopifnot(sum(!is.na(imm$IMN)) > 2400)

# =============================================================================
# 2. Per-threshold panel construction
# =============================================================================
adj <- build_contiguity(mex_muni)

# Municipality -> unit lookup. threshold 0 = the ungrouped baseline.
make_grouping <- function(th) {
  if (th == 0) {
    list(lookup = tibble(mun = as.character(mex_muni$mun),
                         group_id = as.character(mex_muni$mun)),
         units  = mex_muni |> transmute(group_id = as.character(mun), total_pop),
         diag   = list(threshold = 0, n_units = nrow(mex_muni), n_units_merged = 0,
                       n_units_singleton = nrow(mex_muni), max_muns_per_unit = 1,
                       n_island = 0, n_crossing_state = 0))
  } else {
    g <- group_until_threshold(mex_muni, threshold = th, nb = adj, verbose = FALSE)
    list(lookup = st_drop_geometry(g$muni_ref_with_groups) |>
                    transmute(mun = as.character(mun), group_id = as.character(group_id)),
         units  = g$aggregated,
         diag   = g$diagnostics)
  }
}

# Roll a municipality-level count table and the population grid to unit level,
# completing the grid first so structural zeros are present.
build_panel <- function(grid, events, lookup, count_col) {
  pan <- grid |>
    left_join(lookup, by = "mun") |>
    group_by(group_id, year, sex, age) |>
    summarise(population = sum(population), .groups = "drop") |>
    left_join(events |>
                left_join(lookup, by = "mun") |>
                group_by(group_id, year, sex, age) |>
                summarise("{count_col}" := sum(.data[[count_col]]), .groups = "drop"),
              by = c("group_id", "year", "sex", "age")) |>
    mutate("{count_col}" := tidyr::replace_na(.data[[count_col]], 0))
  pan
}

wtd_median <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

# --- (a) stability -----------------------------------------------------------
metrics_stability <- function(rates, rate_col, unit_w, family_label) {
  per_unit <- rates |>
    group_by(group_id, sex) |>
    arrange(year, .by_group = TRUE) |>
    summarise(mean_rate = mean(.data[[rate_col]]),
              sd_rate   = sd(.data[[rate_col]]),
              fd_sd     = sd(diff(.data[[rate_col]])),
              .groups   = "drop") |>
    mutate(cv = sd_rate / mean_rate, fd_cv = fd_sd / mean_rate) |>
    left_join(unit_w, by = "group_id")

  per_unit |>
    filter(is.finite(cv)) |>
    group_by(sex) |>
    summarise(
      cv_median      = median(cv),
      cv_q25         = quantile(cv, .25),
      cv_q75         = quantile(cv, .75),
      cv_p90         = quantile(cv, .90),
      cv_mean        = mean(cv),
      cv_wtd_median  = wtd_median(cv, W),
      fd_cv_median   = median(fd_cv[is.finite(fd_cv)]),
      fd_sd_median   = median(fd_sd[is.finite(fd_sd)]),
      sd_median      = median(sd_rate),
      .groups = "drop") |>
    pivot_longer(-sex, names_to = "metric", values_to = "value") |>
    mutate(family = paste0("stability_", family_label), .before = 1)
}

# --- (b) sparsity ------------------------------------------------------------
metrics_sparsity <- function(panel, count_col, family_label) {
  x <- panel[[count_col]]
  by_unit_year <- panel |> group_by(group_id, year) |>
    summarise(tot = sum(.data[[count_col]]), .groups = "drop")
  tibble(
    metric = c("zero_cell_share", "lt5_cell_share", "lt10_cell_share",
               "zero_unit_year_share", "median_per_cell", "p10_per_cell", "n_cells"),
    value  = c(mean(x == 0), mean(x < 5), mean(x < 10),
               mean(by_unit_year$tot == 0), median(x), quantile(x, .10), nrow(panel))) |>
    mutate(family = paste0("sparsity_", family_label), sex = "all", .before = 1)
}

# --- (c) heterogeneity -------------------------------------------------------
# Poisson sampling variance of a directly standardised rate (delta method,
# counts independent across age-sex cells):
#     Var(R_u(y)) ~= sum_sa w_sa^2 * d_u,sa,y / N_u,sa,y^2
# and, for the year-averaged rate used below, Var(Rbar_u) = (1/Y^2) sum_y Var(R_u(y)).
# `var_samp` is for the year-averaged rate (what V_obs is a variance of);
# `var_samp_1y` is the mean single-year sampling variance, i.e. the noise a
# reader of one year's map actually faces. The two answer different questions
# and, over an 11-year window, differ by two orders of magnitude — report both.
sampling_variance <- function(panel, count_col, std_pop) {
  tot <- sum(std_pop$p_nat)
  n_y <- n_distinct(panel$year)
  panel |>
    left_join(std_pop, by = c("sex", "age")) |>
    filter(population > 0) |>
    mutate(v = (p_nat / tot)^2 * .data[[count_col]] / population^2) |>
    group_by(group_id) |>
    summarise(var_samp = sum(v) / n_y^2, var_samp_1y = sum(v) / n_y, .groups = "drop")
}

metrics_heterogeneity <- function(rate_bar, var_samp, lookup, unit_w, family_label) {
  d <- rate_bar |> left_join(unit_w, by = "group_id") |> left_join(var_samp, by = "group_id")
  stopifnot(!anyNA(d$W), !anyNA(d$mean_rate), !anyNA(d$var_samp))
  Rbar <- weighted.mean(d$mean_rate, d$W)
  V_obs  <- sum(d$W * (d$mean_rate - Rbar)^2) / sum(d$W)
  V_samp <- weighted.mean(d$var_samp, d$W)
  V_samp_1y <- weighted.mean(d$var_samp_1y, d$W)
  V_sig  <- max(0, V_obs - V_samp)

  # Deprivation gradient. Population-weighted OLS of the unit rate on the unit's
  # population-weighted IMN. Higher IMN = LESS marginalized, so a NEGATIVE slope
  # means more deprived -> higher rate. Descriptive only: aggregation can inflate
  # |beta| through ecological correlation even as real signal is lost.
  unit_imm <- lookup |>
    left_join(imm, by = "mun") |>
    filter(!is.na(IMN)) |>
    group_by(group_id) |>
    # sd_IMN BEFORE IMN: summarise() evaluates in order, so redefining IMN first
    # would make the dispersion collapse to zero against its own mean.
    summarise(sd_IMN = sqrt(sum(imm_pop * (IMN - weighted.mean(IMN, imm_pop))^2) / sum(imm_pop)),
              IMN = weighted.mean(IMN, imm_pop),
              n_GM = n_distinct(GM),
              n_area = n_distinct(area_type),
              imm_pop = sum(imm_pop),
              .groups = "drop")
  dg <- d |> inner_join(unit_imm, by = "group_id")
  fit <- lm(mean_rate ~ IMN, data = dg, weights = dg$W)
  beta <- unname(coef(fit)[2]); beta_se <- unname(summary(fit)$coefficients[2, 2])
  rho  <- suppressWarnings(cor(dg$mean_rate, dg$IMN, method = "spearman"))

  tibble(
    metric = c("V_obs", "V_samp", "V_sig", "NSR", "V_samp_1y", "NSR_1y",
               "beta_IMN", "beta_IMN_se", "r2_IMN", "spearman_IMN"),
    value = c(V_obs, V_samp, V_sig, if (V_sig > 0) V_samp / V_sig else NA_real_,
              V_samp_1y, if (V_sig > 0) V_samp_1y / V_sig else NA_real_,
              beta, beta_se, summary(fit)$r.squared, rho)) |>
    mutate(family = paste0("heterogeneity_", family_label), sex = "all", .before = 1)
}

# Composition metrics that do not involve rates, so they are computed once per
# threshold rather than per outcome.
metrics_composition <- function(lookup, units, diag) {
  ui <- lookup |> left_join(imm, by = "mun") |> filter(!is.na(IMN))

  # Within-unit deprivation dispersion. A standard deviation is orientation-free,
  # so the CONAPO IMN sign convention does not affect it — only the gradient
  # slope in metrics_heterogeneity() is signed.
  unit_imn <- ui |> group_by(group_id) |>
    summarise(sd_IMN = sqrt(sum(imm_pop * (IMN - weighted.mean(IMN, imm_pop))^2) / sum(imm_pop)),
              n_GM = n_distinct(GM), n_area = n_distinct(area_type),
              imm_pop = sum(imm_pop), .groups = "drop")

  unit_area <- ui |> group_by(group_id) |>
    summarise(area_type = if_else(weighted.mean(small_towns_pct, imm_pop) >= 50, "Rural", "Urban"),
              .groups = "drop")
  reclass_area <- ui |> left_join(unit_area, by = "group_id", suffix = c("_mun", "_unit")) |>
    summarise(share = sum(imm_pop[area_type_mun != area_type_unit]) / sum(imm_pop)) |> pull(share)
  mixed_pop <- ui |> group_by(group_id) |>
    summarise(mixed = n_distinct(area_type) > 1, pop = sum(imm_pop), .groups = "drop") |>
    summarise(share = sum(pop[mixed]) / sum(pop)) |> pull(share)

  # Unit GM assigned from the population-weighted IMN against the municipality
  # GM cut points, mirroring ch2_060's `thresholds` block.
  cuts <- ui |> group_by(GM) |> summarise(lo = min(IMN), hi = max(IMN), .groups = "drop") |>
    arrange(lo)
  unit_gm <- ui |> group_by(group_id) |>
    summarise(IMN = weighted.mean(IMN, imm_pop), .groups = "drop") |>
    mutate(GM_unit = cut(IMN, breaks = c(-Inf, cuts$hi[-nrow(cuts)], Inf),
                         labels = cuts$GM, right = TRUE))
  reclass_gm <- ui |> left_join(unit_gm |> select(group_id, GM_unit), by = "group_id") |>
    summarise(share = sum(imm_pop[as.character(GM) != as.character(GM_unit)]) / sum(imm_pop)) |>
    pull(share)

  pops <- units$total_pop
  n_per <- table(lookup$group_id)
  geom <- if (inherits(units, "sf")) {
    gp <- st_transform(st_geometry(units), 6372)
    a  <- as.numeric(st_area(gp)); p <- as.numeric(st_length(st_boundary(gp)))
    c(median_compactness = median(4 * pi * a / p^2), median_area_km2 = median(a) / 1e6)
  } else c(median_compactness = NA_real_, median_area_km2 = NA_real_)

  merged <- names(n_per)[n_per > 1]
  tibble(
    metric = c("n_units", "n_units_merged", "n_units_singleton",
               "median_unit_pop", "q25_unit_pop", "q75_unit_pop",
               "median_unit_pop_merged", "median_unit_pop_singleton",
               "share_pop_over_2x_threshold", "max_muns_per_unit", "median_muns_per_unit",
               "n_crossing_state", "n_island_fallbacks",
               "pop_reclassified_GM_share", "pop_reclassified_area_share",
               "pop_in_mixed_area_unit_share",
               "share_units_multi_GM", "share_units_mixed_area",
               "within_unit_sd_IMN_wtd_mean", "within_unit_var_IMN",
               "median_compactness", "median_area_km2"),
    value = c(diag$n_units, diag$n_units_merged, diag$n_units_singleton,
              median(pops), quantile(pops, .25), quantile(pops, .75),
              if (length(merged)) median(units$total_pop[units$group_id %in% merged]) else NA_real_,
              if (length(merged)) median(units$total_pop[!units$group_id %in% merged]) else median(pops),
              if (diag$threshold > 0) sum(pops[pops > 2 * diag$threshold]) / sum(pops) else NA_real_,
              diag$max_muns_per_unit, median(as.numeric(n_per)),
              diag$n_crossing_state, diag$n_island,
              reclass_gm, reclass_area, mixed_pop,
              mean(unit_imn$n_GM > 1), mean(unit_imn$n_area > 1),
              weighted.mean(unit_imn$sd_IMN, unit_imn$imm_pop),
              sum(unit_imn$imm_pop * unit_imn$sd_IMN^2) / sum(unit_imn$imm_pop),
              geom[["median_compactness"]], geom[["median_area_km2"]])) |>
    mutate(family = "composition", sex = "all", .before = 1)
}

# =============================================================================
# 3. The sweep
# =============================================================================
run_sweep <- function(years, label) {
  gm <- grid_mort |> filter(year %in% years)
  gf <- grid_fert |> filter(year %in% years)
  dm <- deaths |> filter(year %in% years)
  df <- births |> filter(year %in% years)

  # One standard population, built once at municipality level, shared by every
  # threshold. Without this each level would standardise to a slightly different
  # reference (see .drop_unstandardisable) and the columns would not be comparable.
  base_lookup <- tibble(mun = as.character(mex_muni$mun), group_id = as.character(mex_muni$mun))
  base_mort <- build_panel(gm, dm, base_lookup, "deaths") |> mutate(mort_rate = deaths / population)
  base_fert <- build_panel(gf, df, base_lookup, "births") |> mutate(fert_rate = births / population)
  std_mort <- suppressMessages(.drop_unstandardisable(base_mort, "mort_rate", "std_pop")) |>
    group_by(group_id, sex, age) |> summarise(population = mean(population), .groups = "drop") |>
    group_by(sex, age) |> summarise(p_nat = sum(population), .groups = "drop")
  std_fert <- suppressMessages(.drop_unstandardisable(base_fert, "fert_rate", "std_pop")) |>
    group_by(group_id, sex, age) |> summarise(population = mean(population), .groups = "drop") |>
    group_by(sex, age) |> summarise(p_nat = sum(population), .groups = "drop")

  out <- list()
  for (th in c(0, THRESHOLDS)) {
    g <- make_grouping(th)
    W <- g$lookup |> left_join(mun_pop, by = "mun") |>
      group_by(group_id) |> summarise(W = sum(total_pop), .groups = "drop")

    pm <- build_panel(gm, dm, g$lookup, "deaths") |> mutate(mort_rate = deaths / population)
    pf <- build_panel(gf, df, g$lookup, "births") |> mutate(fert_rate = births / population)

    rm_ <- suppressMessages(compute_std_mort_rate(pm, unit_col = "group_id", std_pop = std_mort))
    rf_ <- suppressMessages(compute_std_fert_rate(pf, unit_col = "group_id", std_pop = std_fert))
    # Both-sex rate: p_nat_total spans both sexes, so the two sex rows sum to it.
    rm_both <- rm_ |> group_by(group_id, year) |>
      summarise(std_mort_rate = sum(std_mort_rate), sex = "both", .groups = "drop")
    rm_all <- bind_rows(rm_, rm_both)

    rbar_m <- rm_both |> group_by(group_id) |> summarise(mean_rate = mean(std_mort_rate), .groups = "drop")
    rbar_f <- rf_     |> group_by(group_id) |> summarise(mean_rate = mean(std_fert_rate), .groups = "drop")

    out[[length(out) + 1]] <- bind_rows(
      metrics_composition(g$lookup, g$units, g$diag),
      metrics_stability(rm_all, "std_mort_rate", W, "mortality"),
      metrics_stability(rf_,    "std_fert_rate", W, "fertility"),
      metrics_sparsity(pm, "deaths", "mortality"),
      metrics_sparsity(pf, "births", "fertility"),
      metrics_heterogeneity(rbar_m, sampling_variance(pm, "deaths", std_mort), g$lookup, W, "mortality"),
      metrics_heterogeneity(rbar_f, sampling_variance(pf, "births", std_fert), g$lookup, W, "fertility")
    ) |> mutate(threshold = th, window = label, .before = 1)

    message(sprintf("ch2_020 [%s] threshold %6s: %4d units, median pop %s",
                    label, format(th, big.mark = ","), g$diag$n_units,
                    format(round(median(g$units$total_pop)), big.mark = ",")))
  }
  bind_rows(out)
}

res <- run_sweep(ANALYSIS_YEARS, sprintf("%d-%d", min(ANALYSIS_YEARS), max(ANALYSIS_YEARS)))
if (!is.null(SENSITIVITY_YEARS)) {
  res <- bind_rows(res, run_sweep(SENSITIVITY_YEARS,
                                  sprintf("%d-%d", min(SENSITIVITY_YEARS), max(SENSITIVITY_YEARS))))
}

# --- internal consistency checks ---------------------------------------------
main <- res |> filter(window == sprintf("%d-%d", min(ANALYSIS_YEARS), max(ANALYSIS_YEARS)))
val <- function(fam, met, th) {
  main$value[main$family == fam & main$metric == met & main$threshold == th][1]
}
series <- function(fam, met) vapply(c(0, THRESHOLDS), function(t) val(fam, met, t), numeric(1))

# The baseline must be a real, finite, positive between-municipality variance:
# everything in family (c) is expressed relative to it.
stopifnot(is.finite(val("heterogeneity_mortality", "V_obs", 0)),
          val("heterogeneity_mortality", "V_obs", 0) > 0,
          val("composition", "n_units", 0) == nrow(mex_muni))

# Direction checks. A non-monotone series is either a bug or a genuinely
# interesting finding — it must be looked at either way, not published blind.
chk <- function(lbl, x, decreasing = TRUE) {
  bad <- if (decreasing) is.unsorted(rev(x)) else is.unsorted(x)
  if (bad) message(sprintf("ch2_020: WARNING - %s is not monotone in the threshold: %s",
                           lbl, paste(signif(x, 4), collapse = " ")))
}
chk("unit count",            series("composition", "n_units"))
chk("zero-cell share",       series("sparsity_mortality", "zero_cell_share"))
chk("between-unit variance", series("heterogeneity_mortality", "V_obs"))
chk("median unit population", series("composition", "median_unit_pop"), decreasing = FALSE)

res <- res |>
  group_by(window, family, metric, sex) |>
  mutate(rel_to_baseline = value / value[threshold == 0]) |>
  ungroup()

saveRDS(res, file.path(fig_dir, "ch2_020_grouping_diagnostics.RDS"))
readr::write_csv(res, file.path(fig_dir, "ch2_020_grouping_diagnostics.csv"))
readr::write_csv(
  main |> mutate(key = paste(family, metric, sex, sep = ".")) |>
    select(threshold, key, value) |> pivot_wider(names_from = key, values_from = value),
  file.path(fig_dir, "ch2_020_grouping_diagnostics_wide.csv"))

# =============================================================================
# 4. Figures
# =============================================================================
th_scale <- function(p) p + scale_x_continuous(breaks = c(0, THRESHOLDS),
                                               labels = scales::comma) +
  labs(x = "Population threshold (0 = ungrouped municipalities)") + theme_minimal()

pick <- function(fam, mets, sexes = NULL) {
  d <- main |> filter(family == fam, metric %in% mets)
  if (!is.null(sexes)) d <- d |> filter(sex %in% sexes)
  d
}

# The decision figure: noise-to-signal falls with aggregation, real spatial
# signal falls with it too. Where the first stops improving faster than the
# second is the trade-off.
p_tradeoff <- pick("heterogeneity_mortality", c("NSR_1y", "NSR", "V_sig")) |>
  mutate(metric = recode(metric,
                         NSR_1y = "Noise-to-signal, single-year rate",
                         NSR    = "Noise-to-signal, 11-year mean rate",
                         V_sig  = "Signal variance between units")) |>
  group_by(metric) |> mutate(value = value / max(value, na.rm = TRUE)) |> ungroup() |>
  ggplot(aes(threshold, value, colour = metric)) +
  geom_line() + geom_point() +
  labs(y = "Relative to its own maximum", colour = NULL,
       title = "What aggregation buys and what it costs (mortality)",
       subtitle = "Poisson-corrected decomposition of between-unit variance in age-standardised death rates") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "ch2_020_tradeoff_noise_vs_signal.pdf"), th_scale(p_tradeoff),
       width = 9, height = 5.5)

p_cv <- main |>
  filter(family %in% c("stability_mortality", "stability_fertility"),
         metric %in% c("cv_median", "cv_q25", "cv_q75")) |>
  select(family, sex, threshold, metric, value) |>
  pivot_wider(names_from = metric, values_from = value) |>
  mutate(family = recode(family, stability_mortality = "Mortality",
                         stability_fertility = "Fertility (mothers)")) |>
  ggplot(aes(threshold, cv_median, colour = sex, fill = sex)) +
  geom_ribbon(aes(ymin = cv_q25, ymax = cv_q75), alpha = 0.15, colour = NA) +
  geom_line() + geom_point() + facet_wrap(~family, scales = "free_y") +
  labs(y = "CV of the age-standardised rate across years\n(median across units, IQR ribbon)",
       colour = NULL, fill = NULL,
       title = "Year-on-year instability falls as units are merged",
       subtitle = "The rationale for aggregating, measured")
ggsave(file.path(fig_dir, "ch2_020_stability_cv_by_threshold.pdf"), th_scale(p_cv),
       width = 10, height = 5.5)

p_sparse <- main |>
  filter(grepl("^sparsity_", family),
         metric %in% c("zero_cell_share", "lt5_cell_share", "zero_unit_year_share")) |>
  mutate(family = recode(family, sparsity_mortality = "Mortality",
                         sparsity_fertility = "Fertility (mothers)")) |>
  ggplot(aes(threshold, value, colour = metric)) +
  geom_line() + geom_point() + facet_wrap(~family) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Share of cells", colour = NULL,
       title = "Sparse and zero event counts",
       subtitle = "Cells are unit x year x sex x age, completed against the population grid") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "ch2_020_sparsity_zero_shares.pdf"), th_scale(p_sparse),
       width = 10, height = 5.5)

p_het <- main |>
  filter((family == "heterogeneity_mortality" & metric == "V_obs") |
         (family == "composition" &
            metric %in% c("within_unit_var_IMN", "share_units_multi_GM",
                          "pop_reclassified_GM_share", "pop_reclassified_area_share"))) |>
  group_by(family, metric) |> mutate(value = value / max(value, na.rm = TRUE)) |> ungroup() |>
  ggplot(aes(threshold, value, colour = metric)) +
  geom_line() + geom_point() +
  labs(y = "Relative to its own maximum", colour = NULL,
       title = "Heterogeneity absorbed by aggregation",
       subtitle = "Between-unit rate variance falls; deprivation and rural/urban mixing within units rises") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "ch2_020_heterogeneity_retention.pdf"), th_scale(p_het),
       width = 9, height = 5.5)

p_beta <- main |>
  filter(grepl("^heterogeneity_", family), metric %in% c("beta_IMN", "beta_IMN_se")) |>
  select(family, threshold, metric, value) |>
  pivot_wider(names_from = metric, values_from = value) |>
  mutate(family = recode(family, heterogeneity_mortality = "Mortality",
                         heterogeneity_fertility = "Fertility (mothers)")) |>
  ggplot(aes(threshold, beta_IMN)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey60") +
  geom_errorbar(aes(ymin = beta_IMN - 1.96 * beta_IMN_se,
                    ymax = beta_IMN + 1.96 * beta_IMN_se), width = 2000) +
  geom_line() + geom_point() + facet_wrap(~family, scales = "free_y") +
  labs(y = "Slope of the standardised rate on unit IMN\n(negative = more marginalized, higher rate)",
       title = "Deprivation gradient across thresholds",
       subtitle = "Higher IMN means LESS marginalized (CONAPO convention). Descriptive: aggregation can inflate |slope|.")
ggsave(file.path(fig_dir, "ch2_020_gradient_attenuation.pdf"), th_scale(p_beta),
       width = 10, height = 5.5)

p_pop <- main |>
  filter(family == "composition",
         metric %in% c("median_unit_pop", "median_unit_pop_merged", "median_unit_pop_singleton",
                       "q25_unit_pop", "q75_unit_pop")) |>
  ggplot(aes(threshold, value, colour = metric)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey60") +
  geom_line() + geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Unit population", colour = NULL,
       title = "Unit population against the threshold applied",
       subtitle = "Dashed line = the threshold itself. Merged units should sit just above it; singletons are pre-existing large municipalities.") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "ch2_020_unit_population_distribution.pdf"), th_scale(p_pop),
       width = 9, height = 5.5)

# --- The single summary panel: all three thesis criteria on one axis ----------
# Section 2.4 names three requirements but only quantifies none of them. This is
# the figure that puts them side by side: the two benefits and the one cost,
# each indexed to its own ungrouped (threshold = 0) value so they share a scale.
# One y-axis - the three series are deliberately made commensurate rather than
# given separate scales, which would make the crossings meaningless.
tradeoff_series <- bind_rows(
  pick("stability_mortality", "cv_median", sexes = "both") |>
    mutate(series = "Year-on-year instability (CV)", role = "Benefit"),
  pick("sparsity_mortality", "zero_cell_share") |>
    mutate(series = "Cells with zero deaths", role = "Benefit"),
  pick("heterogeneity_mortality", "V_sig") |>
    mutate(series = "Real variation between places", role = "Cost")) |>
  group_by(series) |>
  # Index to the ungrouped baseline. Asserted rather than assumed: if the 0 row
  # is ever dropped from THRESHOLDS this silently becomes a different figure.
  mutate(base = value[threshold == 0]) |>
  ungroup()
stopifnot(nrow(tradeoff_series) > 0, !anyNA(tradeoff_series$base),
          all(tradeoff_series$base > 0))
tradeoff_series <- tradeoff_series |> mutate(rel = value / base)

# Validated with the dataviz palette checker (light surface): all six checks pass,
# worst adjacent CVD dE 14.7. Do not substitute hues without re-running it.
tradeoff_cols <- c("Year-on-year instability (CV)" = "#3d7fb5",
                   "Cells with zero deaths"        = "#d4785f",
                   "Real variation between places"  = "#8a6bb0")

p_combined <- ggplot(tradeoff_series, aes(threshold, rel, colour = series)) +
  geom_vline(xintercept = 50000, linetype = 2, colour = "grey65") +
  annotate("text", x = 50000, y = 1.02, label = "current threshold",
           hjust = -0.05, size = 3.2, colour = "grey40") +
  geom_line(aes(linetype = role), linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(values = tradeoff_cols, name = NULL) +
  scale_linetype_manual(values = c(Benefit = "solid", Cost = "22"), name = NULL) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Relative to ungrouped municipalities",
       title = "What the population threshold buys, and what it costs",
       subtitle = paste("Mortality,", sprintf("%d-%d", min(ANALYSIS_YEARS), max(ANALYSIS_YEARS)),
                        "- the two benefits fall steeply then flatten;",
                        "the cost falls roughly in a straight line")) +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.margin = margin(t = -4))
ggsave(file.path(fig_dir, "ch2_020_threshold_tradeoff.png"), th_scale(p_combined),
       width = 9, height = 6, dpi = 150)

# =============================================================================
# 5. Console summary — this block is the source for docs/ch2_grouping_diagnostics.md
# =============================================================================
summ <- main |>
  filter((family == "composition" & metric %in% c("n_units", "median_unit_pop", "max_muns_per_unit")) |
         (family == "stability_mortality" & metric == "cv_median" & sex == "both") |
         (family == "sparsity_mortality" & metric %in% c("zero_cell_share", "lt5_cell_share")) |
         (family == "heterogeneity_mortality" & metric %in% c("V_obs", "NSR")) |
         (family == "composition" & metric %in% c("share_units_multi_GM", "pop_reclassified_area_share"))) |>
  mutate(key = paste(family, metric, sep = ".")) |>
  select(threshold, key, value) |>
  pivot_wider(names_from = key, values_from = value)

cat("\n============ ch2_020 grouping diagnostics ============\n")
cat(sprintf("window %s | %d municipalities | %d thresholds\n\n",
            sprintf("%d-%d", min(ANALYSIS_YEARS), max(ANALYSIS_YEARS)),
            nrow(mex_muni), length(THRESHOLDS)))
print(as.data.frame(summ), digits = 4, row.names = FALSE)
cat("\nFull tables: ", file.path(fig_dir, "ch2_020_grouping_diagnostics.csv"), "\n")
cat("This block is the ONLY source for docs/ch2_grouping_diagnostics.md.\n")
cat("=====================================================\n")
