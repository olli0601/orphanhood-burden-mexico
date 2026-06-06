# =============================================================================
# ch5_050_CI_orphanhoos.R  ·  Chapter 5 — Orphanhood estimation
# Uncertainty intervals for orphanhood via co-monotone Poisson resampling of births, deaths and populations.
# Reads input-data-processed/deaths_df_long.RDS (prefix g) -> credible intervals.
# =============================================================================

# ──────────────────────────────────────────────────────────────
#  Uncertainty intervals via co-monotone Poisson resampling
#  • Propagate noise in births, deaths, populations
#  • Recompute the entire pipeline each draw
#  • 95% UIs = 2.5th / 97.5th quantiles across draws
# ──────────────────────────────────────────────────────────────

library(data.table)
setDTthreads(parallel::detectCores())

# ---------- Helpers ----------
# co-monotone Poisson: same U by (stratum) across all years to mimic serial corr
comono_poisson <- function(lambda, u) qpois(pmin(pmax(u, 1e-12), 1-1e-12), lambda)

sim_counts <- function(DT, count_col, strata = c("group_id","sex","age"), year_col = "year"){
  DT <- copy(as.data.table(DT))
  setkeyv(DT, c(strata, year_col))
  # A single U per stratum -> perfectly rank-correlated over time
  u_tbl <- DT[, .(u = runif(1)), by = strata]
  DT <- u_tbl[DT, on = strata]
  DT[, (count_col) := comono_poisson(get(count_col), u)]
  DT[, u := NULL]
  DT[]
}

# build single-age parent population (≤79) once (you already did most of this)
parent_pop_single <- population_df %>%
  mutate(age_start = as.numeric(stringr::str_extract(age, "^\\d+")),
         age_end   = as.numeric(stringr::str_extract(age, "\\d+$"))) %>%
  rowwise() %>% mutate(age = list(seq(age_start, age_end))) %>%
  ungroup() %>% unnest(age) %>%
  filter(age >= 15, age < 60) %>%
  mutate(population = round(population / (age_end - age_start + 1))) %>%
  select(group_id, year, sex, age, population) %>%
  as.data.table()

# births (single-age) you already have in fertility_df as births by age
births_single <- fertility_df %>%                      # group_id,year,sex,age,births
  select(group_id, year, sex, age, births) %>%
  as.data.table()

# adult deaths (single-age, <80) for hazards and to aggregate into parent bands
deaths_single_base <- readRDS(paste0(g, "deaths_df_long.RDS")) %>%
  left_join(geo_info %>% select(mun, group_id), by = "mun") %>%
  group_by(group_id, year, sex, age) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  filter(as.numeric(age) < 60) %>%
  mutate(age = as.integer(age)) %>% as.data.table()

# child population (0–17) you already built as `population_children`
child_pop_single <- as.data.table(population_children)  # group_id,year,sex,age,population

# child deaths for survival_df come from mort_df (already read)
child_deaths_single <- mort_df %>%
  left_join(geo_info %>% select(mun, group_id), by = "mun") %>%
  group_by(group_id, year, sex, age) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  mutate(age = as.integer(age)) %>% as.data.table()

# band function reused
to_band_chr <- function(a){
  if (is.na(a) || a < 15 || a >= 80) return(NA_character_)
  sprintf("%02d-%02d", floor(a/5)*5, floor(a/5)*5 + 4)
}

# -------- One simulation draw: returns prevalence-by-sex and by-age rates --------
one_draw <- function(){
  # 1) Resample raw counts with co-monotone Poisson noise
  births_sim   <- sim_counts(births_single,     "births",   c("group_id","sex","age"))
  ppop_sim     <- sim_counts(parent_pop_single, "population", c("group_id","sex","age"))
  adeaths_sim  <- sim_counts(deaths_single_base,"deaths",   c("group_id","sex","age"))
  cpop_sim     <- sim_counts(child_pop_single,  "population", c("group_id","sex","age"))
  cdeaths_sim  <- sim_counts(child_deaths_single,"deaths",  c("group_id","sex","age"))
  
  # 2) Fertility (births / parent pop) on the simulated tables
  fert_sim <- births_sim[ppop_sim,
                         on = .(group_id, year, sex, age),
                         nomatch = 0][
                           , fertility_rate := births / population
                         ][!is.na(fertility_rate)]
  
  # 3) Child survival (per your step 4) on simulated deaths/pop
  surv_sim <- cdeaths_sim[cpop_sim,
                          on = .(group_id, year, sex, age),
                          nomatch = 0][
                            , .(hazard_1yr = deaths / population), 
                            by = .(group_id, year, age)
                          ][
                            , .(group_id, year, age, survival_prob = 1 - hazard_1yr)
                          ]
  
  # 4) Expected surviving children per parent age/child age (your Step 5)
  setDT(fert_sim); setDT(surv_sim)
  C_dt <- fert_sim[
    , .(child_age = 0:17),
    by = .(year, age, sex, group_id, fertility_rate, population)
  ][
    , `:=`(birth_year = year - child_age,
           survival_age = child_age + 1,
           parent_age_at_birth = age - child_age)
  ][parent_age_at_birth >= 15 & birth_year >= 1990 & birth_year <= 2023]
  
  # lookup fert at (birth_year, parent_age_at_birth)
  fert_lookup <- fert_sim[, .(birth_year = year, parent_age_at_birth = age, sex, group_id, fertility_rate)]
  C_dt[fert_lookup, on = .(birth_year, parent_age_at_birth, sex, group_id),
       fertility_rate := i.fertility_rate]
  
  # join survival
  surv_sim[, `:=`(birth_year = year, survival_age = age)]
  valid_keys <- unique(surv_sim[, .(group_id, birth_year, survival_age)])
  C_dt <- C_dt[valid_keys, on = .(group_id, birth_year, survival_age), nomatch = 0]
  C_dt[surv_sim, on = .(group_id, birth_year, survival_age),
       survival_prob := i.survival_prob]
  C_dt[, expected_children_raw := fertility_rate * survival_prob]
  C_dt[, parent_age_band := vapply(parent_age_at_birth, to_band_chr, character(1))]
  C_dt <- C_dt[!is.na(parent_age_band)]
  band_summary_sim <- C_dt[, .(expected_children = mean(expected_children_raw, na.rm = TRUE)),
                           by = .(year, sex, group_id, parent_age_band, child_age)]
  
  # 5) Parental deaths by 5-y band from simulated single-age deaths
  adeaths_sim[, band := vapply(age, to_band_chr, character(1))]
  deaths_band_sim <- adeaths_sim[!is.na(band) & as.integer(sub(".*-(\\d+)$", "\\1", band)) <= 60,
                                 .(deaths = sum(deaths)),
                                 by = .(group_id, year, sex, parent_band = band)]
  
  # 6) Adult 5-y hazard by band (weighted by simulated pop)
  haz5 <- adeaths_sim[ppop_sim, on = .(group_id, year, sex, age)][
    , mx := fifelse(population > 0, deaths / population, 0)
  ][, .(group_id, year, sex, age, hazard_5yr = 1 - exp(-5*mx))]
  
  haz_weighted_sim <- haz5[ppop_sim, on = .(group_id, year, sex, age)][
    , band := vapply(age, to_band_chr, character(1))
  ][!is.na(band),
    .(hazard_band = weighted.mean(hazard_5yr, w = population, na.rm = TRUE)),
    by = .(group_id, year, sex, age = band)]
  
  # 7) Double- and prior-orphan terms, O_new (your 7–9)
  O_death_dt <- band_summary_sim[deaths_band_sim,
                                 on = .(group_id, year, sex, parent_age_band = parent_band),
                                 nomatch = 0][
                                   , O_death := expected_children * deaths]
  O_death_dt[, opposite_sex := ifelse(sex == "male","female","male")]
  setkey(haz_weighted_sim, group_id, year, age, sex)
  setkey(O_death_dt, group_id, year, parent_age_band, opposite_sex)
  O_death_dt <- O_death_dt[haz_weighted_sim,
                           on = .(group_id,
                                  year,
                                  parent_age_band = age,
                                  opposite_sex   = sex),
                           nomatch = 0]
  setnames(O_death_dt, "hazard_band", "hazard_opp")
  O_death_dt[is.na(hazard_opp), hazard_opp := 0]
  O_death_dt[, O_double := hazard_opp * expected_children * deaths]
  
  # prior-orphan accumulation
  haz5[, hazard_1yr := 1 - (1 - hazard_5yr)^(1/5)]
  O_death_dt[, row_id := .I]
  O_death_dt[, midpoint_age := as.numeric(sub("^(\\d+)-.*$", "\\1", parent_age_band)) + 2]
  prev_exp <- O_death_dt[child_age > 1,
                         .(i = seq_len(child_age - 1)),
                         by = .(row_id, group_id, year, midpoint_age,
                                opp_sex = opposite_sex, expected_children, deaths)]
  prev_exp[, `:=`(target_year = year - i, target_age = midpoint_age - i)]
  setkey(haz5, group_id, year, age, sex)
  prev_exp <- haz5[prev_exp,
                   on = .(group_id, year = target_year, age = target_age, sex = opp_sex)]
  prev_exp[is.na(hazard_1yr), hazard_1yr := 0]
  prev_exp[, contrib := hazard_1yr * expected_children * deaths]
  prior_sums <- prev_exp[, .(O_prev = sum(contrib)), by = row_id]
  O_death_dt <- prior_sums[O_death_dt, on = .(row_id)]
  O_death_dt[is.na(O_prev), O_prev := 0]
  
  O_new_dt <- O_death_dt[, .(
    O_death  = sum(expected_children * deaths),
    O_double = sum(O_double),
    O_prev   = sum(O_prev)
  ), by = .(group_id, year, child_age, parent_age_band)][
    , O_new := O_death - 0.5*O_double - O_prev][]
  
  O_new_tot_sim <- O_new_dt[, .(O_new = sum(O_new)), by = .(group_id, year, child_age)]
  O_new_sex_sim <- O_death_dt[, .(O_new_sex = sum(expected_children * deaths - O_prev)),
                              by = .(group_id, year, sex, child_age)]
  
  # 8) Lifetime prevalence with simulated child survival (eq. 8)
  # national totals for O_new and child survival
  O_new_nat <- O_new_tot_sim[, .(O_new = sum(O_new)), by = .(year, child_age)]
  child_surv <- cdeaths_sim[cpop_sim, on = .(group_id, year, sex, age),
                            nomatch = 0][
                              , .(year, age, population, deaths)
                            ][
                              , .(surv = 1 - sum(deaths, na.rm=TRUE)/sum(population, na.rm=TRUE)),
                              by = .(year, age)
                            ]
  setkey(O_new_nat, year, child_age)
  setkey(child_surv, year, age)
  
  O_lifetime <- O_new_nat[
    , {
      y <- year; b <- child_age; S <- 1; prev <- 0
      for (i in 0:b){
        yy <- y - i; bb <- b - i
        onew <- O_new_nat[.(yy, bb), O_new, nomatch = 0]
        if (!is.na(onew)) prev <- prev + S * onew
        if (i < b){
          s_j <- child_surv[.(yy, bb), surv, nomatch = 0]
          S   <- S * ifelse(is.na(s_j), 1, s_j)
        }
      }
      .(Olifetime = prev)
    }, by = .(year, child_age)
  ]
  
  # by parent sex
  O_new_sex_nat <- O_new_sex_sim[, .(O_new = sum(O_new_sex)), by = .(year, sex, child_age)]
  setkey(O_new_sex_nat, year, sex, child_age)
  O_lifetime_sex <- O_new_sex_nat[
    , {
      y <- year; b <- child_age; s <- sex; S <- 1; prev <- 0
      for (i in 0:b){
        yy <- y - i; bb <- b - i
        onew <- O_new_sex_nat[.(yy, s, bb), O_new, nomatch = 0]
        if (!is.na(onew)) prev <- prev + S * onew
        if (i < b){
          s_j <- child_surv[.(yy, bb), surv, nomatch = 0]
          S   <- S * ifelse(is.na(s_j), 1, s_j)
        }
      }
      .(Olifetime = prev)
    }, by = .(year, sex, child_age)
  ]
  
  # collapse to prevalence totals (0–17) by sex and compute per-100 rates
  prev_sex_year <- O_lifetime_sex[, .(total_orphans = sum(Olifetime, na.rm = TRUE)),
                                  by = .(year, sex)]
  children_year <- cpop_sim[, .(children = sum(population, na.rm = TRUE)), by = year]
  rate_sex <- prev_sex_year[children_year, on = "year"
  ][, .(year, sex, rate_per_100 = 100 * total_orphans / children)]
  
  # prevalence by child age group
  age_map <- function(a) ifelse(a <= 4, "0-4", ifelse(a <= 9, "5-9", "10-17"))
  # NEW (compute the grouping var inside `by`)
  prev_age <- O_lifetime[
    , .(total_orphans = sum(Olifetime)),
    by = .(year, age_group = age_map(child_age))
  ]
  
  children_age <- cpop_sim[
    , .(total_children = sum(population)),
    by = .(year, age_group = age_map(age))
  ]
  rate_age <- prev_age[children_age, on = .(year, age_group)
  ][, .(year, age_group, rate_per_100 = 100 * total_orphans / total_children)]
  
  list(rate_sex = rate_sex, rate_age = rate_age)
}

# ==== Adaptive simulations with early stopping (paste here, replacing your 'Run simulations' block) ====
suppressPackageStartupMessages({
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("Please install future.apply: install.packages('future.apply')")
  }
})
library(future.apply)
library(data.table)

## --- settings ---------------------------------------------------------------
target_B <- 300L          # max number of draws you are willing to run
chunk    <- 25L           # how many draws per iteration
tol_rel  <- 0.03          # stop if BOTH CI ends move < 3% (relative) ...
tol_abs  <- 0.02          # ... OR < 0.02 per 100 children (absolute)
seed_base <- 20250815

## Parallel plan (safe on macOS/Windows/Linux)
plan(multisession, workers = max(1, parallel::detectCores() - 1))

## --- helpers ---------------------------------------------------------------
summarise_ui <- function(draws) {
  rsx <- rbindlist(lapply(draws, `[[`, "rate_sex"))
  rax <- rbindlist(lapply(draws, `[[`, "rate_age"))
  ui_sex <- rsx[, .(
    med = median(rate_per_100, na.rm = TRUE),
    lwr = quantile(rate_per_100, 0.025, na.rm = TRUE),
    upr = quantile(rate_per_100, 0.975, na.rm = TRUE)
  ), by = .(year, sex)][order(year, sex)]
  
  ui_age <- rax[, .(
    med = median(rate_per_100, na.rm = TRUE),
    lwr = quantile(rate_per_100, 0.025, na.rm = TRUE),
    upr = quantile(rate_per_100, 0.975, na.rm = TRUE)
  ), by = .(year, age_group)][order(year, age_group)]
  
  list(sex = ui_sex, age = ui_age)
}

ui_stable <- function(now, prev, tol_rel, tol_abs) {
  m <- merge(now, prev,
             by = intersect(names(now), names(prev)),
             suffixes = c(".now", ".prev"))
  if (nrow(m) == 0L) return(FALSE)
  # relative deltas (protect against 0 denominators)
  rel <- function(a_now, a_prev) abs(a_now - a_prev) / pmax(1e-9, abs(a_prev))
  rel_lwr <- rel(m$lwr.now, m$lwr.prev)
  rel_upr <- rel(m$upr.now, m$upr.prev)
  abs_lwr <- abs(m$lwr.now - m$lwr.prev)
  abs_upr <- abs(m$upr.now - m$upr.prev)
  
  # Stable if EVERY point passes either the relative or the absolute threshold on both ends
  all((rel_lwr < tol_rel | abs_lwr < tol_abs) &
        (rel_upr < tol_rel | abs_upr < tol_abs))
}

report_delta <- function(now, prev) {
  m <- merge(now, prev,
             by = intersect(names(now), names(prev)),
             suffixes = c(".now", ".prev"))
  if (nrow(m) == 0L) return(list(max_rel = NA_real_, max_abs = NA_real_))
  rel <- function(a_now, a_prev) abs(a_now - a_prev) / pmax(1e-9, abs(a_prev))
  max_rel <- max(rel(c(m$lwr.now, m$upr.now), c(m$lwr.prev, m$upr.prev)), na.rm = TRUE)
  max_abs <- max(abs(c(m$lwr.now, m$upr.now) - c(m$lwr.prev, m$upr.prev)), na.rm = TRUE)
  list(max_rel = max_rel, max_abs = max_abs)
}

## --- loop -------------------------------------------------------------------
accum   <- vector("list", 0L)
ui_prev <- NULL
b_done  <- 0L
set.seed(seed_base)

repeat {
  n_left <- target_B - b_done
  if (n_left <= 0L) break
  
  n_run <- min(chunk, n_left)
  message(sprintf("Running draws %d–%d of %d ...", b_done + 1L, b_done + n_run, target_B))
  
  sims <- future_lapply(
    seq_len(n_run),
    function(i) {
      # make each worker reproducible but distinct
      set.seed(seed_base + b_done + i)
      one_draw()
    },
    future.seed = TRUE
  )
  
  accum  <- c(accum, sims)
  b_done <- b_done + n_run
  
  ui_now <- summarise_ui(accum)
  
  if (!is.null(ui_prev)) {
    d_sex <- report_delta(ui_now$sex, ui_prev$sex)
    d_age <- report_delta(ui_now$age, ui_prev$age)
    message(sprintf("  checkpoint @ %d draws | max Δrel=%.3f, max Δabs=%.3f",
                    b_done,
                    max(d_sex$max_rel, d_age$max_rel, na.rm = TRUE),
                    max(d_sex$max_abs, d_age$max_abs, na.rm = TRUE)))
    
    stable <- ui_stable(ui_now$sex, ui_prev$sex, tol_rel, tol_abs) &&
      ui_stable(ui_now$age, ui_prev$age, tol_rel, tol_abs)
    
    if (stable) {
      message(sprintf("  ✅ early stop at %d draws: UIs are stable (tol_rel=%.3f, tol_abs=%.3f)",
                      b_done, tol_rel, tol_abs))
      ui_final <- ui_now
      break
    }
  }
  
  ui_prev <- ui_now
  ui_final <- ui_now  # keep latest in case we hit target_B exactly
}

# Final UI tables to use in plots:
ui_sex_final <- ui_final$sex
ui_age_final <- ui_final$age

# (Optional) keep legacy names if your plotting code expects ui_sex / ui_age
ui_sex <- copy(ui_sex_final)
ui_age <- copy(ui_age_final)

# quick peek
message(sprintf("Finished with %d draws.", b_done))
print(head(ui_sex))
print(head(ui_age))

# ========= Plot 1: Prevalence per 100 children — by parent sex =========
library(dplyr); library(ggplot2); library(scales)

ui_sex_plot <- ui_sex_final %>%
  mutate(sex = dplyr::recode(sex, female = "Mother", male = "Father"))

# Error bars (like your screenshot). Uncomment the ribbon if you want bands too.
ggplot(ui_sex_plot, aes(year, med, colour = sex, group = sex)) +
  # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = sex), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25, alpha = 0.85) +
  scale_colour_manual(values = c("Mother" = "#b2182b", "Father" = "#2166ac"),
                      name = "Deceased parent") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Orphanhood prevalence per 100 children by parent sex",
       y = "Per 100 children", x = NULL) +
  theme_classic(base_size = 14)


# ========= Plot 2: Prevalence per 100 children — by child age group =========
ui_age_plot <- ui_age_final %>%
  mutate(age_group = factor(age_group, levels = c("0-4","5-9","10-17")))

# Error bars (like your screenshot). Uncomment the ribbon if you want bands too.
ggplot(ui_age_plot, aes(year, med, colour = age_group, group = age_group)) +
  # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = age_group), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25, alpha = 0.85) +
  scale_color_manual(values = c("0-4" = "#a8dbc5", "5-9" = "#5ea77a", "10-17" = "#33673b"),
                     name = "Age of child",
                     labels = c("0–4 years", "5–9 years", "10–17 years")) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Orphanhood prevalence per 100 children by age group",
       y = "Per 100 children", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        panel.grid = element_blank())
