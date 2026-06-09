# =============================================================================
# ch4_310_nowcast_new.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Updated standalone nowcast prototype.
# Reads input-data-processed/{births_grouped_mun,population_grouped_mun,rural_urban_area}.*.
# =============================================================================

###############################################################################
# Now-casting births — stratum-specific version (municipality × age × sex)
# ---------------------------------------------------------------------------
# 1. Build delay triangle
# 2. Fit NB-GAM, keep mu_hat per stratum
# 3. Static global delay distribution
# 4. Attach delay info to each stratum row
# 5. Posterior-predictive simulation (vectorised)
# 6. Optional aggregation
###############################################################################

## --------------------------------------------------------------------------
## 0: SETTINGS
## --------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)       # bam()
library(gtools)     # rdirichlet()
library(parallel)
library(arrow)

D_max       <- 8
tail_id     <- D_max + 1
T_now       <- 2023
alpha_prior <- 0.5
n_sim       <- 2000
set.seed(20250707)

## --------------------------------------------------------------------------
## 1:  DATA IN + DELAY TRIANGLE
## --------------------------------------------------------------------------
births <- readRDS("input-data-processed/births_grouped_mun.RDS") |>
  rename(event_year   = year,
         reg_year     = year_reg,
         municipality = group_id,
         age_group    = age,
         n            = births) |>
  mutate(reg_year = as.numeric(reg_year),
         delay = reg_year - event_year)

rural_urban <- readRDS("input-data-processed/rural_urban_area.RDS")

pop_tbl <- readRDS("input-data-processed/population_grouped_mun.RDS") |>
  rename(event_year   = year,
         municipality = group_id,
         age_group    = age) |>
  filter(!age_group %in% c("00-04", "05-09", "10-14"))

triangle_tbl <- births |>
  mutate(delay = pmin(reg_year - event_year, D_max + 1)) |>
  filter(delay >= 0) |>
  complete(event_year,
           delay        = 0:(D_max + 1),
           municipality, sex, age_group,
           fill = list(n = 0L)) |>
  group_by(event_year, delay,
           municipality, sex, age_group) |>
  summarise(n = sum(n), .groups = "drop")

## --------------------------------------------------------------------------
## 2:  NEGATIVE-BINOMIAL GAM
## --------------------------------------------------------------------------
incidence_tbl <- triangle_tbl |>
  group_by(event_year, municipality, sex, age_group) |>
  summarise(events = sum(n), .groups = "drop") |>
  left_join(pop_tbl,
            by = c("event_year", "municipality", "sex", "age_group")) |>
  filter(population > 0) |>
  mutate(across(c(municipality, sex, age_group), as.factor)) |>
  drop_na(events, population)

nb_bam <- bam(
  events ~
    s(event_year, k = 10) +
    s(municipality, bs = "re") +
    s(sex,          bs = "re") +
    s(age_group,    bs = "re") +
    offset(log(population)),
  family     = nb(link = "log"),
  data       = incidence_tbl,
  method     = "fREML",
  discrete   = TRUE,
  nthreads   = detectCores(),
  chunk.size = 5e4,
  select     = TRUE
)

incidence_tbl <- incidence_tbl |>
  mutate(
    mu_hat = predict(
      nb_bam,
      newdata   = pick(everything()),
      type      = "response",
      na.action = na.pass
    )
  )

theta_nb <- nb_bam$family$getTheta(TRUE)

## --------------------------------------------------------------------------
## 3:  GLOBAL DELAY DISTRIBUTION  (Dirichlet-smoothed)
## --------------------------------------------------------------------------
delay_hist <- triangle_tbl |>
  group_by(delay) |>
  summarise(n = sum(n), .groups = "drop") |>
  complete(delay = 0:tail_id, fill = list(n = 0L)) |>
  arrange(delay) |>
  mutate(p_hat = (n + alpha_prior) /
           (sum(n) + alpha_prior * (tail_id + 1)))

p_delay     <- delay_hist$p_hat          # length = D_max + 2
cum_p_delay <- cumsum(p_delay)           # cumulative

## --------------------------------------------------------------------------
## 4:  ATTACH DELAY INFO TO EACH STRATUM ROW
## --------------------------------------------------------------------------
open_years <- (T_now - D_max):T_now

incidence_open <- incidence_tbl |>
  filter(event_year %in% open_years) |>
  mutate(
    d_today           = pmin(T_now - event_year, D_max),
    exp_reported_mean = mu_hat * cum_p_delay[d_today + 1]
  )

## --------------------------------------------------------------------------
## 5:  POSTERIOR-PREDICTIVE SIMULATION  (vectorised across strata)
## --------------------------------------------------------------------------
d_vec    <- incidence_open$d_today
cumprob  <- cum_p_delay[d_vec + 1]
valid    <- is.finite(incidence_open$mu_hat) & is.finite(cumprob)
n_val    <- sum(valid)

# Pre-allocate matrix: rows = strata, cols = simulations
sim_mat <- matrix(NA_real_, nrow = nrow(incidence_open), ncol = n_sim)

library(matrixStats)   # fast rowQuantiles()

## ---------- helper: running min/max of a given quantile -------------------
row_q_update <- function(q_prev, block, prob) {
  q_block <- rowQuantiles(block, probs = prob)
  if (prob < .5) pmin(q_prev, q_block) else pmax(q_prev, q_block)
}

## ---------- blocked simulation -------------------------------------------
block_size <- 500        # sims per block  (≈ 250 × 100k × 4 bytes ~ 95 MB)
n_blocks   <- ceiling(n_sim / block_size)

q05 <- rep(Inf,  nrow(incidence_open))   # initialise with extremes
q95 <- rep(-Inf, nrow(incidence_open))

for (b in seq_len(n_blocks)) {
  
  sims <- min(block_size, n_sim - (b - 1L) * block_size)
  if (sims == 0) break
  
  ## --- generate NB and Binomial draws only for valid rows -----------------
  mu_rep   <- rep(incidence_open$mu_hat[valid], sims)
  prob_rep <- rep(cumprob[valid],                    sims)
  
  N_star <- rnbinom(n_val * sims, mu = mu_rep, size = theta_nb)
  R_star <- rbinom(n_val * sims, size = N_star, prob = prob_rep)
  
  mat_val <- matrix(R_star, nrow = n_val, ncol = sims, byrow = FALSE)
  
  ## --- embed in a full-sized block so rowQuantiles lines up ---------------
  block_full <- matrix(NA_integer_,
                       nrow = nrow(incidence_open),
                       ncol = sims)
  block_full[valid, ] <- mat_val
  
  ## --- update running 5 % and 95 % ---------------------------------------
  q05 <- row_q_update(q05, block_full, 0.05)
  q95 <- row_q_update(q95, block_full, 0.95)
  
  rm(mu_rep, prob_rep, N_star, R_star, mat_val, block_full); gc()
  message("Finished block ", b, " of ", n_blocks,
          " (", sims, " simulations)")
}

## ---------- attach intervals ---------------------------------------------
incidence_open <- incidence_open %>%
  mutate(reported_95_lo = q05,
         reported_95_hi = q95)

## --------------------------------------------------------------------------
## 6:  OPTIONAL: SUMMARISE TO YEAR-TOTALS
## --------------------------------------------------------------------------
year_totals <- incidence_open |>
  group_by(event_year) |>
  summarise(
    total_hat      = sum(mu_hat,           na.rm = TRUE),
    reported_hat   = sum(exp_reported_mean, na.rm = TRUE),
    reported_95_lo = sum(reported_95_lo,    na.rm = TRUE),
    reported_95_hi = sum(reported_95_hi,    na.rm = TRUE),
    .groups = "drop"
  )




############ TAKE INTO ACCOUNT COVID 
###############################################################################
# Now-casting births — COVID-aware (pre / covid / post) delay curves
#   municipality × age_group × sex strata
###############################################################################
## 1 ─ Read data & build full delay triangle --------------------------------
births <- readRDS("input-data-processed/births_grouped_mun.RDS") %>%
  rename(event_year = year,
         reg_year   = year_reg,
         municipality = group_id,
         age_group    = age,
         n            = births) %>%
  mutate(reg_year = as.numeric(reg_year))

pop_tbl <- readRDS("input-data-processed/population_grouped_mun.RDS") %>%
  rename(event_year = year,
         municipality = group_id,
         age_group    = age) %>%
  filter(!age_group %in% c("00-04", "05-09", "10-14"))

triangle_tbl_covid <- births %>%
  mutate(delay = pmin(reg_year - event_year, D_max + 1L)) %>%  # tail clamped
  filter(delay >= 0) %>%
  complete(event_year,
           delay        = 0:(D_max + 1),
           municipality, sex, age_group,
           fill = list(n = 0L)) %>%
  group_by(event_year, delay,
           municipality, sex, age_group) %>%
  summarise(n = sum(n), .groups = "drop")

## 2 ─ NB-GAM with COVID bump -----------------------------------------------
incidence_tbl_covid <- triangle_tbl_covid %>%
  group_by(event_year, municipality, sex, age_group) %>%
  summarise(events = sum(n), .groups = "drop") %>%
  left_join(pop_tbl,
            by = c("event_year", "municipality", "sex", "age_group")) %>%
  filter(population > 0) %>%
  mutate(across(c(municipality, sex, age_group), as.factor),
         covid_post = factor(event_year >= 2020, levels = c(FALSE, TRUE))) %>%
  drop_na(events, population)

nb_bam_covid <- bam(
  events ~
    s(event_year, k = 8) +                     # long-term spline
    s(event_year, by = covid_post, k = 5) +    # extra wiggliness 2020+
    s(municipality, bs = "re") +
    s(sex,          bs = "re") +
    s(age_group,    bs = "re") +
    offset(log(population)),
  family     = nb(link = "log"),
  data       = incidence_tbl_covid,
  method     = "fREML",
  discrete   = TRUE,
  nthreads   = detectCores()
)

theta_nb <- nb_bam_covid$family$getTheta(TRUE)

incidence_tbl_covid <- incidence_tbl_covid %>%
  mutate(mu_hat = predict(nb_bam_covid,
                          newdata   = pick(everything()),
                          type      = "response"))

## 3 ─ Period-specific delay distribution (Dirichlet-smoothed) --------------
delay_hist_covid <- triangle_tbl_covid %>%
  mutate(period = case_when(event_year <= 2019      ~ "pre",
                            event_year %in% 2020:2021 ~ "covid",
                            TRUE                      ~ "post")) %>%
  group_by(period, delay) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(period) %>%
  mutate(p_hat = (n + alpha_prior) /
           (sum(n) + alpha_prior * (D_max + 2))) %>%
  arrange(period, delay) %>%
  mutate(cum_p_delay_covid = cumsum(p_hat)) %>%
  ungroup() %>%
  select(period, delay, cum_p_delay_covid)          # lookup table

## 4 ─ Attach delay info to every open stratum-year -------------------------
incidence_open_covid <- incidence_tbl_covid %>%
  filter(event_year >= T_now - D_max, event_year <= T_now) %>%
  mutate(delay  = pmin(T_now - event_year, D_max),
         period = case_when(event_year <= 2019      ~ "pre",
                            event_year %in% 2020:2021 ~ "covid",
                            TRUE                      ~ "post")) %>%
  left_join(delay_hist_covid, by = c("period", "delay")) %>%
  mutate(exp_reported_mean = mu_hat * cum_p_delay_covid)

## 5 ─ Posterior-predictive simulation (vectorised) -------------------------
d_vec   <- incidence_open_covid$delay
cumprob <- incidence_open_covid$cum_p_delay_covid
valid   <- is.finite(incidence_open_covid$mu_hat) & is.finite(cumprob)
n_val   <- sum(valid)

sim_mat_covid<- matrix(NA_real_, nrow = nrow(incidence_open_covid), ncol = n_sim)

library(matrixStats)

row_q_update <- function(q_prev, block, prob) {
  q_block <- rowQuantiles(block, probs = prob)
  if (prob < .5) pmin(q_prev, q_block) else pmax(q_prev, q_block)
}

block_size <- 250                          # sims per block
n_blocks   <- ceiling(n_sim / block_size)

q05 <- rep(Inf,  nrow(incidence_open_covid))
q95 <- rep(-Inf, nrow(incidence_open_covid))

for (b in seq_len(n_blocks)) {
  
  sims <- min(block_size, n_sim - (b - 1L) * block_size)
  if (sims == 0) break
  
  ## --- generate draws only for valid rows ---------------------------------
  mu_rep   <- rep(incidence_open_covid$mu_hat[valid], sims)
  prob_rep <- rep(cumprob[valid],                    sims)
  
  N_star <- rnbinom(n_val * sims, mu = mu_rep, size = theta_nb)
  R_star <- rbinom(n_val * sims, size = N_star, prob = prob_rep)
  
  mat_val <- matrix(R_star, nrow = n_val, ncol = sims, byrow = FALSE)
  
  ## --- pad to full size so row order matches ------------------------------
  block_full <- matrix(NA_integer_,
                       nrow = nrow(incidence_open_covid),
                       ncol = sims)
  block_full[valid, ] <- mat_val
  
  ## --- update running quantiles -------------------------------------------
  q05 <- row_q_update(q05, block_full, 0.05)
  q95 <- row_q_update(q95, block_full, 0.95)
  
  ## --- tidy up and print progress -----------------------------------------
  rm(mu_rep, prob_rep, N_star, R_star, mat_val, block_full); gc()
  message("Finished block ", b, " of ", n_blocks,
          " (", sims, " simulations)")
}

incidence_open_covid <- incidence_open_covid %>%
  mutate(reported_95_lo = q05,
         reported_95_hi = q95)


## (optional) aggregate to year level with BOTH sets of CIs -----------------
year_totals <- incidence_open_covid %>%
  group_by(event_year) %>%
  summarise(
    total_hat      = sum(mu_hat, na.rm = T),
    reported_hat   = sum(exp_reported_mean, na.rm = T),
    reported_95_lo = sum(reported_95_lo, na.rm = T),
    reported_95_hi = sum(reported_95_hi, na.rm = T),
    .groups = "drop"
  )



###############################################################################
# ONE-STOP CODE BLOCK:
#   • builds a tidy table with *observed* registrations vs *estimated* totals
#   • draws a line plot  +  a side-by-side bar plot
###############################################################################
library(dplyr)
library(tidyr)
library(ggplot2)

year_min <- 1990
year_max <- 2023

## 1 ─ Build tidy data (observed vs estimated) -------------------------------
plot_df <- triangle_tbl %>%                              # delay triangle
  filter(delay <= pmin(T_now - event_year, D_max)) %>%         # seen by today
  group_by(event_year) %>%
  summarise(Reported = sum(n), .groups = "drop") %>%
  left_join(year_totals %>%                                    # model totals
              select(event_year, Estimated = total_hat),
            by = "event_year") %>%
  filter(event_year >= year_min, event_year <= year_max) %>%   # keep 2015–23
  pivot_longer(cols = c(Reported, Estimated),
               names_to  = "series",
               values_to = "value")

plot_df
## 2 ─ Line plot -------------------------------------------------------------
line_plot <- ggplot(plot_df,
                    aes(x = event_year, y = value, colour = series)) +
  geom_line(linewidth = 1.1) +
  geom_point(linewidth = 2) +
  scale_colour_manual(values = c(Reported  = "#e15759",
                                 Estimated = "#4e79a7")) +
  labs(title  = "Births 2015–2023: observed registrations vs estimated total",
       x      = "Event year",
       y      = "Number of births",
       colour = "") +
  theme_minimal(base_size = 14)

## 3 ─ Bar plot --------------------------------------------------------------
bar_plot <- ggplot(plot_df,
                   aes(x   = factor(event_year),
                       y   = value,
                       fill = series)) +
  geom_col(position = position_dodge(width = 0.7),
           width    = 0.6) +
  scale_fill_manual(values = c(Reported  = "#e15759",
                               Estimated = "#4e79a7")) +
  labs(title = "Births 2015–2023: observed registrations vs estimated total",
       x     = "Event year",
       y     = "Number of births",
       fill  = "") +
  theme_minimal(base_size = 14)

## 4 ─ Display ---------------------------------------------------------------
print(line_plot)
print(bar_plot)




###############################################################################
# STACKED bar-plot (2015-2023)
#   • red   = registrations already observed (“Reported”)
#   • blue  = yet-to-come births  =  Estimated − Reported
#   • whisker = 95 % CI for the *total* number of births
###############################################################################
library(dplyr)
library(tidyr)
library(ggplot2)

year_min <- 1990
year_max <- 2023

## ── 1.  Observed registrations (up to T_now) -------------------------------
reported_obs <- triangle_tbl %>%
  filter(delay <= pmin(T_now - event_year, D_max)) %>%
  group_by(event_year) %>%
  summarise(Reported = sum(n), .groups = "drop")

## ── 2.  Join with model totals & build ‘remaining’ component ---------------
plot_df <- year_totals %>%                                   # step-6 object
  select(event_year, Estimated = total_hat) %>%              # model latent
  left_join(reported_obs, by = "event_year") %>%
  mutate(
    Remaining = pmax(Estimated - Reported, 0)                # blue segment
  ) %>%
  filter(event_year >= year_min, event_year <= year_max)


other_years <- triangle_tbl %>%                              # delay triangle
  filter(delay <= pmin(T_now - event_year, D_max)) %>%         # seen by today
  group_by(event_year) %>%
  summarise(Reported = sum(n), .groups = "drop") %>%
  filter(event_year <= 2014)

## ── 2b.  Add the fully-observed years (1990–2014) ─────────────────────────

complete_years <- other_years %>%          # only event_year + Reported
  mutate(
    Estimated = Reported,  # total = what we saw
    Remaining = 0,         # nothing left to come in
    var_est   = NA_real_,  # skip NB variance for these
    ci_lo     = Reported,  # CI collapses to point
    ci_hi     = Reported
  )

plot_df <- bind_rows(plot_df, complete_years) %>% 
  arrange(event_year)                        # keep chronological order

## ── 3.  Long format for stacked bars (unchanged) ──────────────────────────
stack_df <- plot_df %>% 
  select(event_year, Reported, Remaining) %>% 
  pivot_longer(-event_year,
               names_to  = "segment",
               values_to = "value")


## ── 2a.  Quick 95 % CI for the TOTAL (latent NB mean ± 1.96·sd) ------------
#   Var(NB) = μ + μ² / θ   ;  θ from the GAM fit
plot_df <- plot_df %>%
  mutate(
    var_est = Estimated + Estimated^2 / theta_nb,
    ci_lo   = pmax(Estimated - 1.96 * sqrt(var_est), 0),
    ci_hi   =        Estimated + 1.96 * sqrt(var_est)
  )

## ── 3.  Long format for stacked bars ---------------------------------------
stack_df <- plot_df %>%
  select(event_year, Reported, Remaining) %>%
  pivot_longer(-event_year,
               names_to  = "segment",
               values_to = "value")

## ── 4.  Stacked BAR plot with CI whiskers ----------------------------------
ggplot(stack_df,
       aes(x = factor(event_year),
           y = value,
           fill = segment)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(Reported  = "#4e79a7",   
                               Remaining = "#e15759")) +
  theme_minimal()+ 
  labs(title = "Reported vs model-estimated births",
                        x     = "Event year",
                        y     = "Number of births",
                        fill  = "") +
  theme_minimal(base_size = 14)





