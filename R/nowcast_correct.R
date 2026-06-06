# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(VGAM)

births <- readRDS("../datasets/births_new_mun.RDS") |>
  rename(event_year = year, reg_year = year_reg, municipality = group_id, age_group = age, n = births) |>
  mutate(reg_year = as.numeric(reg_year))

pop_tbl <- readRDS("../datasets/population_new_mun.RDS") |>
  rename(event_year = year, municipality = group_id, age_group = age) |>
  filter(!age_group %in% c("00-04", "05-09", "10-14"))

D_max <- 8
triangle_tbl <- births %>%
  # 3·1  work out the reporting delay
  mutate(delay = reg_year - event_year,
         delay = if_else(delay > D_max, D_max + 1L, delay)) %>%   # collapse the tail
  # 3·2  keep only plausible rows
  filter(delay >= 0L) %>%
  # 3·3  fill in explicit zero cells so the triangle is rectangular
  complete(event_year,
           delay = 0:(D_max + 1L),            # d_over is D_max+1
           municipality, sex, age_group,
           fill = list(n = 0L)) %>%
  # 3·4  aggregate (in case there were overlapping rows)
  group_by(event_year, delay,
           municipality, sex, age_group) %>%
  summarise(n = sum(n), .groups = "drop")

triangle_tbl %>%
  group_by(sex, delay) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  ggplot(aes(delay, n, colour = sex)) + geom_line()


# Aggregate the triangle to get the true annual counts ─────────────
incidence_tbl <- triangle_tbl %>%                 # from the previous step
  group_by(event_year, municipality, sex, age_group) %>%
  summarise(events = sum(n), .groups = "drop") %>%   # collapse delays
  left_join(pop_tbl,
            by = c("event_year", "municipality", "sex", "age_group")) %>%
  filter(population > 0)                             # keep valid strata

# Fit a negative-binomial GAM with a smooth year trend ─────────────
incidence_tbl <- incidence_tbl |>
  mutate(
    municipality = factor(municipality),
    sex          = factor(sex),
    age_group    = factor(age_group)
  ) |>
  drop_na(           # be explicit: keep only complete cases for model vars
    events, event_year,
    municipality, sex, age_group,
    population
  )

cores <- parallel::detectCores()

nb_bam <- bam(
  events ~
    s(event_year, k = 10) +
    s(municipality, bs = "re") +
    s(sex,          bs = "re") +
    s(age_group,    bs = "re") +
    offset(log(population)),
  
  family   = nb(link = "log"),
  data     = incidence_tbl,
  
  ## speed knobs
  method   = "fREML",            # fast REML optimiser
  discrete = TRUE,               # low-rank trick
  nthreads = cores,              # use all CPU cores
  chunk.size = 50000,           # rows per processing chunk
  select   = TRUE                # shrink over-complex terms
)


# ── 4.  Quick sanity checks ──────────────────────────────────────────────
summary(nb_bam)           # trend shape, over-dispersion ‘theta’, etc.
plot(nb_bam, pages = 1)   # visualise the smooth s(event_year)


# Fit the delay model static
tail_id <- D_max + 1       # “≥ D_max” column in the triangle
T_now   <- 2023            # calendar year you are running the check for

alpha_prior <- .5          # Dirichlet(½,…,½) prior for shrinkage

incidence_tbl <- incidence_tbl |>
  mutate(mu_hat = predict(nb_bam, type = "response"))

N_hat <- incidence_tbl |>
  group_by(event_year) |>
  summarise(N_hat = sum(mu_hat), .groups = "drop") |>
  tibble::deframe()                # named numeric vector: names = event years


delay_hist <- triangle_tbl |>
  group_by(delay) |>
  summarise(n = sum(n), .groups = "drop") |>
  complete(delay = 0:tail_id, fill = list(n = 0)) |>
  arrange(delay) |>
  mutate(p_hat = (n + alpha_prior) / (sum(n) + alpha_prior * (tail_id + 1)))

p_delay <- delay_hist$p_hat                     # vector length D_max+2
cum_p_delay <- cumsum(p_delay)                  # handy cumulative version

open_years <- (T_now - D_max):T_now
open_years <- open_years[open_years %in% names(N_hat)]

expected_partial <- sapply(open_years, function(y) {
  d <- T_now - y                       # delay column that is fully observed so far
  d <- pmin(d, D_max)                  # if y very old, clamp at full horizon
  N_hat[as.character(y)] * cum_p_delay[d + 1]
})
expected_partial


library(gtools)              # for rdirichlet()

theta <- nb_bam$family$getTheta(TRUE)   # NB size from the GAM
n_sim <- 5000                          # how many Monte-Carlo samples

pred_partial <- matrix(NA_real_,
                       nrow = length(open_years),
                       ncol = n_sim,
                       dimnames = list(open_years, NULL))

set.seed(20250707)
for (s in seq_len(n_sim)) {
  ## 4·1 draw one p*  (Dirichlet posterior)
  p_star <- as.numeric(rdirichlet(1, delay_hist$n + alpha_prior))
  cum_p  <- cumsum(p_star)
  
  ## 4·2 loop over event years
  for (y in open_years) {
    d <- T_now - y
    d <- pmin(d, D_max)
    
    ## 4·3 draw true total N*  (NB with same mean & dispersion)
    N_star <- rnbinom(1, mu = N_hat[as.character(y)], size = theta)
    
    ## 4·4 split into delays
    split_vec <- rmultinom(1, size = N_star, prob = p_star)[, 1]
    
    ## 4·5 store cumulative up to today
    pred_partial[as.character(y), s] <- sum(split_vec[seq_len(d + 1)])
  }
}

## 4·6 95 % predictive thresholds
thr_95 <- apply(pred_partial, 1, quantile, probs = .95)
thr_95

##########################################################
### Time-varying delay probabilities
##########################################################
library(dplyr)
library(tidyr)
library(VGAM)

## ---- 1  Collapse any strata ---------------------------------------------
delay_year <- triangle_tbl %>%                # long table
  group_by(event_year, delay) %>%             # drop muni/sex/age
  summarise(n = sum(n), .groups = "drop")

## ---- 2  Wide delay matrix  (one row = one event year) --------------------
delay_wide <- delay_year %>%
  pivot_wider(names_from  = delay,
              values_from = n,
              values_fill = list(n = 0)) %>%   # <- list, not bare 0
  arrange(event_year)                          # nice and tidy

## ---- 3  Response matrix Y  ----------------------------------------------
Y <- as.matrix(delay_wide[ , -1L])             # drop the year column
colnames(Y) <- paste0("d", colnames(Y))        # d0,d1,… for clarity

## ---- 4  Fit time-varying multinomial with VGAM --------------------------
multi_vglm <- vglm(
  Y ~ s(event_year, by = sex, df = 8), 
  family = multinomial,
  data   = delay_wide,
  maxit  = 50,
  crit   = "coef"              # converges a bit faster
)

## ---- 5  Fitted p_d(y) ----------------------------------------------------
p_fit <- fitted(multi_vglm, type = "response")   # matrix rows = years
rownames(p_fit) <- delay_wide$event_year

head(p_fit)

cum_p_year <- t(apply(p_fit, 1, cumsum))   # same dim as p_fit

expected_partial_tv <- sapply(open_years, function(y) {
  d <- T_now - y
  d <- pmin(d, D_max)          # clamp old years
  N_hat[as.character(y)] * cum_p_year[as.character(y), d + 1]
})

plot_tbl_new <- reported_tbl |>
  full_join(pred_tbl, by = "event_year") |>
  mutate(
    reported_so_far = replace_na(reported_so_far, 0),
    exp_reported    = expected_partial_tv[as.character(event_year)],
    unreported_est  = pmax(predicted_total - reported_so_far, 0)
  ) |>
  filter(event_year %in% open_years)


#################################################################
## ---------------- PLOTS ---------------------
#################################################################

open_years <- (T_now - D_max):T_now           # 2015 … 2023

# helper table: for each open year, the last delay column we can see today
current_lag <- tibble(
  event_year = open_years,
  max_delay  = pmin(T_now - open_years, D_max)
)

reported_tbl <- triangle_tbl |>
  inner_join(current_lag, by = "event_year") |>
  filter(delay <= max_delay) |>
  group_by(event_year) |>
  summarise(reported_so_far = sum(n), .groups = "drop")


pred_tbl <- tibble(
  event_year      = as.integer(names(N_hat)),
  predicted_total = as.numeric(N_hat)
)

plot_tbl <- reported_tbl |>
  full_join(pred_tbl, by = "event_year") |>
  mutate(
    reported_so_far = replace_na(reported_so_far, 0),
    unreported_est  = pmax(predicted_total - reported_so_far, 0)
  ) |>
  filter(event_year %in% open_years)

plot_tbl |>
  pivot_longer(c(reported_so_far, unreported_est),
               names_to  = "status",
               values_to = "births") |>
  ggplot(aes(x = factor(event_year), y = births, fill = status)) +
  geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +  # <- NEW
  scale_fill_manual(
    values = c(reported_so_far = "#4F83C4",
               unreported_est  = "#E07B91"),
    labels = c("Reported so far",
               "Estimated yet to be reported"),
    breaks = c("reported_so_far", "unreported_est"),                  # legend order
    name   = NULL
  ) +
  labs(
    title = "Now-cast of annual births (up to T = 2023)",
    x     = "Year of occurrence",
    y     = "Number of births"
  ) +
  theme_minimal(base_size = 12)


births_tets <- births |>
  group_by(event_year) |>
  summarise(n = sum(n), .groups = "drop")

ggplot(data = births_tets, aes(x=event_year, y = n))+ 
  geom_col(width = 0.8)



###########################################################################
### rolling-origin hind-casting
###########################################################################
################################################################################
# SET-UP -----------------------------------------------------------------------
################################################################################
library(dplyr)
library(tidyr)
library(mgcv)        # bam()
library(VGAM)        # vglm()
library(gtools)      # rdirichlet()
library(purrr)       # map_df()
library(yardstick)   # metrics
library(parallel)

## ---- 0  Load & pre-clean the datasets ---------------------------------------
births <- readRDS("../datasets/births_new_mun.RDS") |>
  rename(event_year  = year,
         reg_year    = year_reg,
         municipality = group_id,
         age_group    = age,
         n            = births) |>
  mutate(reg_year = as.numeric(reg_year))

pop_tbl <- readRDS("../datasets/population_new_mun.RDS") |>
  rename(event_year = year,
         municipality = group_id,
         age_group    = age) |>
  filter(!age_group %in% c("00-04", "05-09", "10-14"))

D_max   <- 8L        # longest explicit delay column
tail_id <- D_max + 1 # index of the ≥ D_max bucket
alpha_prior <- 0.5   # Dirichlet(½, … , ½) shrinkage

################################################################################
# 1.  Triangle construction (once, up-front) -----------------------------------
################################################################################
triangle_tbl <- births |>
  mutate(delay = pmin(reg_year - event_year, D_max + 1L)) |>
  filter(delay >= 0L) |>
  complete(event_year,
           delay     = 0:(D_max + 1L),
           municipality, sex, age_group,
           fill      = list(n = 0L)) |>
  group_by(event_year, delay,
           municipality, sex, age_group) |>
  summarise(n = sum(n), .groups = "drop")

################################################################################
# 2.  Utility helpers ----------------------------------------------------------
################################################################################

## -- 2·1  Censor triangle as of T_now -----------------------------------------
censor_triangle <- function(tri_tbl, T_now, D_max) {
  tri_tbl |>
    mutate(
      delay_today = pmin(T_now - event_year, D_max + 1L)
    ) |>
    filter(event_year <= T_now,
           delay <= delay_today) |>
    select(-delay_today)
}

## -- 2·2  Build incidence table from a *censored* triangle --------------------
make_incidence_tbl <- function(tri_cut, pop_tbl) {
  tri_cut |>
    group_by(event_year, municipality, sex, age_group) |>
    summarise(events = sum(n), .groups = "drop") |>
    left_join(pop_tbl,
              by = c("event_year", "municipality", "sex", "age_group")) |>
    filter(population > 0) |>
    mutate(
      municipality = factor(municipality),
      sex          = factor(sex),
      age_group    = factor(age_group)
    )
}

## -- 2·3  Static Dirichlet delay fit (posterior α = n + α₀) -------------------
fit_delay_static <- function(tri_cut, alpha_prior, D_max) {
  delay_hist <- tri_cut |>
    group_by(delay) |>
    summarise(n = sum(n), .groups = "drop") |>
    complete(delay = 0:(D_max + 1L), fill = list(n = 0L)) |>
    arrange(delay)
  
  list(
    alpha_post = delay_hist$n + alpha_prior,
    p_hat      = (delay_hist$n + alpha_prior) /
      (sum(delay_hist$n) + alpha_prior * (D_max + 2L))
  )
}

## -- 2·4  Time-varying VGAM delay fit ----------------------------------------
fit_delay_tv <- function(tri_cut, D_max) {
  delay_year <- tri_cut |>
    group_by(event_year, delay) |>
    summarise(n = sum(n), .groups = "drop") |>
    pivot_wider(names_from  = delay,
                values_from = n,
                values_fill = 0) |>
    arrange(event_year)
  
  Y <- as.matrix(delay_year[ , -1L])
  colnames(Y) <- paste0("d", colnames(Y))
  
  multi_vglm <- vglm(
    Y ~ s(event_year, df = 8),
    family = multinomial,
    data   = delay_year,
    maxit  = 50,
    crit   = "coef"
  )
  
  list(vglm_fit = multi_vglm,
       event_years = delay_year$event_year)
}

################################################################################
# 3.  Core now-cast for one vintage -------------------------------------------
################################################################################
run_nowcast <- function(T_now,
                        D_max       = 8L,
                        n_sim       = 4000L,
                        delay_type  = c("static", "tv"),
                        tri_tbl     = triangle_tbl,
                        pop_tbl     = pop_tbl,
                        alpha_prior = 0.5) {
  
  delay_type <- match.arg(delay_type)
  
  ## ---- 3·1  Snapshot the data ----------------------------------------------
  tri_cut <- censor_triangle(tri_tbl, T_now, D_max)
  incidence_tbl_cut <- make_incidence_tbl(tri_cut, pop_tbl)
  
  ## ---- 3·2  Fit incidence GAM ----------------------------------------------
  nb_bam_cut <- bam(
    events ~
      s(event_year, k = 10) +
      s(municipality, bs = "re") +
      s(sex,          bs = "re") +
      s(age_group,    bs = "re") +
      offset(log(population)),
    family   = nb(link = "log"),
    data     = incidence_tbl_cut,
    method   = "fREML",
    discrete = TRUE,
    nthreads = detectCores(),
    chunk.size = 50000,
    select   = TRUE
  )
  
  incidence_tbl_cut <- incidence_tbl_cut |>
    mutate(mu_hat = predict(nb_bam_cut, type = "response"))
  
  N_hat <- incidence_tbl_cut |>
    group_by(event_year) |>
    summarise(N_hat = sum(mu_hat), .groups = "drop") |>
    tibble::deframe()
  
  ## ---- 3·3  Fit delay model -------------------------------------------------
  if (delay_type == "static") {
    delay_fit <- fit_delay_static(tri_cut, alpha_prior, D_max)
  } else {
    delay_fit <- fit_delay_tv(tri_cut, D_max)
  }
  
  ## ---- 3·4  Predict via Monte-Carlo ----------------------------------------
  theta <- nb_bam_cut$family$getTheta(TRUE)
  open_years <- (T_now - D_max):T_now
  pred_mat   <- matrix(NA_real_,
                       nrow = length(open_years),
                       ncol = n_sim,
                       dimnames = list(open_years, NULL))
  
  set.seed(20250801)
  for (s in seq_len(n_sim)) {
    
    ## ---- 3·4·1 draw delay probabilities p* ---------------------------------
    if (delay_type == "static") {
      p_star <- as.numeric(rdirichlet(1, delay_fit$alpha_post))
    } else {
      p_hat_year <- fitted(delay_fit$vglm_fit, type = "response")
      rownames(p_hat_year) <- delay_fit$event_years
      p_star <- NULL  # create inside loop below, varies by event_year
    }
    cum_fun <- function(v) cumsum(v)[seq_len(D_max + 1L) + 1L]
    
    ## ---- 3·4·2 loop over event years --------------------------------------
    for (y in open_years) {
      d <- pmin(T_now - y, D_max)
      
      ## draw N*
      N_star <- rnbinom(1, mu = N_hat[as.character(y)], size = theta)
      
      ## split into delay buckets
      if (delay_type == "static") {
        split_vec <- rmultinom(1, size = N_star, prob = p_star)[ , 1]
        cum_seen  <- sum(split_vec[seq_len(d + 1)])
      } else {
        p_star_y  <- p_hat_year[as.character(y), ]
        split_vec <- rmultinom(1, size = N_star, prob = p_star_y)[ , 1]
        cum_seen  <- sum(split_vec[seq_len(d + 1)])
      }
      
      pred_mat[as.character(y), s] <- cum_seen
    }
  }
  
  ## ---- 3·5 95 % predictive upper bound -------------------------------------
  upr_95 <- apply(pred_mat, 1, quantile, probs = 0.95, na.rm = TRUE)
  
  tibble(
    event_year = open_years,
    pred_total = N_hat[as.character(open_years)],
    upr_95     = upr_95,
    T_now      = T_now
  )
}

################################################################################
# 4.  Rolling-origin hind-cast -------------------------------------------------
################################################################################
vintages <- 2010:2019      # choose any range you like

hindcast_tbl <- map_dfr(
  vintages,
  run_nowcast,
  delay_type = "static"        # "static" or "tv"
)

## ---- 4·1  attach truth & score ---------------------------------------------
truth_tbl <- births |>
  group_by(event_year) |>
  summarise(events = sum(n), .groups = "drop")

scored <- hindcast_tbl |>
  left_join(truth_tbl, by = "event_year")

score_summary <- metric_set(rmse, mape, interval_accuracy)(
  truth    = scored$events,
  estimate = scored$pred_total,
  lower    = NA,
  upper    = scored$upr_95
)

print(score_summary)
