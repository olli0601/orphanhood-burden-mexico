# =============================================================================
# ch4_270_newapp_nowcast.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Alternative nowcast: Poisson plug-in Stan model with reduce_sum threading, producing national delay-adjusted birth totals.
# Reads input-data-processed/{population_grouped_mun,births_grouped_mun}.RDS, generates+compiles a Stan model -> output/ch4/*.png + fit RDS.
# =============================================================================
source("R/rates.R")

# nowcast_bayes_yearly_nowcast_only_reduce_sum.R
# Full Bayesian NOWCAST (yearly), plug-in c_t (YEARS), no forecasts.
# Poisson likelihood version with reduce_sum threading (CmdStanR).
# Works in Google Colab paths (/content/...).

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(splines)
  library(posterior)
  library(stringr)
  library(ggplot2)
})

set.seed(42)

# ======================= CONFIG ==============================================
# Fix "today": last day of 2023
today_date <- as.Date("2023-12-31")
year_now   <- 2023

# Paths (edit if needed)
pop_path    <- "input-data-processed/population_grouped_mun.RDS"
births_path <- "input-data-processed/births_grouped_mun.RDS"

# Model controls
alpha_dirich     <- 1       # Dirichlet smoothing for π_d
ref_year_for_std <- 2023    # reference year for national standardization
age_df           <- 8       # spline df over age groups (categorical index)
year_df          <- 10      # spline df over event_year

chains        <- 4
iter_warmup   <- 1000
iter_sample   <- 2000
adapt_delta   <- 0.9
max_treedepth <- 12

# Nowcast window and completeness cutoff
target_years <- 2016:2023
ct_cutoff    <- 0.97

# --- QuickTry: run a tiny fit first to sanity-check everything ---------------
QuickTry       <- FALSE         # set FALSE for full run
QuickTry_n_mun <- 25            # keep ~N municipalities
QuickTry_years <- 2016:2023     # keep a few years for speed

# ======================= LOAD & CLEAN DATA ===================================
# Population per municipality, age, sex (exclude youngest groups)
pop_tbl <- readRDS(pop_path) |>
  rename(event_year   = year,
         municipality = group_id,
         age_group    = age) |>
  filter(!age_group %in% c("00-04","05-09","10-14")) |>
  mutate(
    event_year   = as.integer(event_year),
    age_group    = str_trim(as.character(age_group)),
    sex          = str_trim(as.character(sex)),
    municipality = str_trim(as.character(municipality)),
    population   = as.numeric(population)
  )

# Births with registration year; delay in YEARS
births <- readRDS(births_path) |>
  rename(event_year   = year,
         reg_year     = year_reg,
         municipality = group_id,
         age_group    = age,
         n            = births) |>
  mutate(
    event_year   = as.integer(event_year),
    reg_year     = as.integer(reg_year),
    delay        = reg_year - event_year,    # YEARS
    age_group    = str_trim(as.character(age_group)),
    sex          = str_trim(as.character(sex)),
    municipality = str_trim(as.character(municipality)),
    n            = as.integer(n)
  ) |>
  filter(!age_group %in% c("00-04","05-09","10-14"))

# Optional QuickTry subsetting (apply to both data sources)
if (QuickTry) {
  mun_keep <- pop_tbl |>
    distinct(municipality) |>
    slice_head(n = QuickTry_n_mun) |>
    pull(municipality)
  pop_tbl  <- pop_tbl  |> filter(municipality %in% mun_keep,
                                 event_year %in% QuickTry_years)
  births   <- births   |> filter(municipality %in% mun_keep,
                                 event_year %in% QuickTry_years,
                                 reg_year    %in% (min(QuickTry_years):(year_now)))
}

stopifnot(n_distinct(pop_tbl$sex) == 2)

# ======================= NATIONAL FERTILITY (your pipeline) ===================
births_national <- births %>%
  group_by(event_year, age_group, sex) %>%
  summarise(births = sum(n, na.rm = TRUE), .groups = "drop")

pop_national <- pop_tbl %>%
  group_by(event_year, age_group, sex) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

fert_national <- births_national %>%
  left_join(pop_national, by = c("event_year","age_group","sex")) %>%
  filter(!is.na(population), population > 0, !is.na(births)) %>%
  mutate(fert_rate = births / population)

compute_national_std_rate_age_gender <- function(data, ref_year = 2023) {
  pop_ref <- data %>%
    filter(event_year %in% !!ref_year) %>%
    select(sex, age_group, event_year, population) %>%
    group_by(sex, age_group) %>%
    summarise(population = mean(population, na.rm = TRUE), .groups = "drop")
  
  p_nat <- pop_ref %>%
    group_by(sex, age_group) %>%
    summarise(p_nat = sum(population, na.rm = TRUE), .groups = "drop")
  
  p_nat_total <- sum(pop_ref$population, na.rm = TRUE)
  
  std_rate <- data %>%
    select(event_year, age_group, sex, births, population, fert_rate) %>%
    left_join(p_nat, by = c("age_group","sex")) %>%
    mutate(p_nat_total = p_nat_total) %>%
    mutate(std_rate = (p_nat / p_nat_total) * fert_rate) %>%
    group_by(age_group, sex, event_year) %>%
    summarise(std_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
  
  std_rate
}

fert_nat_input <- fert_national %>%
  left_join(pop_national, by = c("event_year","age_group","sex"),
            suffix = c("", ".nat")) %>%
  transmute(event_year, age_group, sex,
            births = births,
            population = population,
            fert_rate = fert_rate)

std_fert_national <- compute_national_std_rate_age_gender(fert_nat_input,
                                                          ref_year_for_std)

# ======================= GLOBAL DELAY PMF π_d (YEARS) =========================
delay_counts <- births %>%
  filter(delay >= 0) %>%
  count(delay, wt = n, name = "total_n") %>%
  arrange(delay)

if (nrow(delay_counts) == 0) stop("No nonnegative delays found to build π_d.")
all_delays <- tibble(delay = 0:max(delay_counts$delay))
delay_counts <- all_delays %>%
  left_join(delay_counts, by = "delay") %>%
  mutate(total_n = replace_na(total_n, 0)) %>%
  arrange(delay) %>%
  mutate(
    smoothed_count = total_n + alpha_dirich,
    pi_d   = smoothed_count / sum(smoothed_count),
    cum_pi = cumsum(pi_d)
  )

pi_vec <- delay_counts$pi_d
cum_pi <- delay_counts$cum_pi
Dmax   <- length(pi_vec) - 1L

# ======================= c_t by event_year (YEARS elapsed as of 2023-12-31) ===
ct_tbl <- tibble(event_year = sort(unique(births$event_year))) %>%
  mutate(
    d_years = pmax(0L, pmin(Dmax, year_now - event_year)),
    c_t     = cum_pi[d_years + 1L]
  ) %>%
  select(event_year, c_t, d_years)

# ======================= R_today (yearly; sum delays <= d_years) ==============
R_today <- births %>%
  inner_join(ct_tbl, by = "event_year") %>%
  filter(delay <= d_years) %>%
  group_by(event_year, age_group, sex, municipality) %>%
  summarise(R_today = sum(n), .groups = "drop") %>%
  left_join(ct_tbl, by = "event_year")

# ======================= OFFSETS: pop + std_rate + c_t ========================
off_tbl <- pop_tbl %>%
  left_join(std_fert_national, by = c("event_year","age_group","sex")) %>%
  mutate(std_rate = replace_na(std_rate, 0))

df_fit <- R_today %>%
  left_join(off_tbl, by = c("event_year","age_group","sex","municipality")) 

if (nrow(df_fit) == 0) stop("df_fit ended up empty after joins/filters.")

# ======================= KEYS: master mapping to avoid NAs ====================
mun_key <- pop_tbl %>%
  distinct(municipality) %>% arrange(municipality) %>%
  mutate(mun_id = dplyr::row_number())
age_key <- pop_tbl %>% distinct(age_group) %>% arrange(age_group)
sex_key <- pop_tbl %>% distinct(sex) %>% arrange(sex)

df_fit <- df_fit %>%
  mutate(
    municipality = str_trim(as.character(municipality)),
    age_group    = str_trim(as.character(age_group)),
    sex          = str_trim(as.character(sex))
  ) %>%
  semi_join(mun_key, by = "municipality") %>%
  semi_join(age_key, by = "age_group") %>%
  semi_join(sex_key, by = "sex") %>%
  left_join(mun_key, by = "municipality") %>%
  mutate(
    municipality = factor(municipality, levels = mun_key$municipality),
    age_group    = factor(age_group,   levels = age_key$age_group),
    sex          = factor(sex,         levels = sex_key$sex)
  )

# stopifnot(!anyNA(df_fit$municipality))
stopifnot(!anyNA(df_fit$mun_id))
stopifnot(nlevels(df_fit$sex) == 2)

sex_lv <- levels(df_fit$sex)
df_fit <- df_fit %>%
  mutate(
    sex01        = as.numeric(sex == sex_lv[2]),  # numeric 0/1 for Stan vector
    age_id       = as.integer(age_group),
    log_pop      = log(pmax(population, 1e-9)),
    log_std      = log(pmax(std_rate,   1e-12)),
    log_c        = log(pmax(c_t,        1e-12)),
    log_off_today  = log_pop + log_std + log_c,   # combined offsets
    log_off_lambda = log_pop + log_std
  )

stopifnot(all(is.finite(df_fit$log_pop)))
stopifnot(all(is.finite(df_fit$log_std)))
stopifnot(all(is.finite(df_fit$log_c)))
stopifnot(all(is.finite(df_fit$R_today)))

# ======================= SPLINE BASES (age & event_year) ======================
B_age_fit  <- bs(df_fit$age_id,     df = age_df,  intercept = FALSE)
B_year_fit <- bs(df_fit$event_year, df = year_df, intercept = FALSE)
K_age  <- ncol(B_age_fit)
K_year <- ncol(B_year_fit)
M      <- nrow(mun_key)

# ======================= STAN MODEL (Poisson + reduce_sum) ====================
stan_code <- '
functions {
  real pois_partial_sum(array[] int y_slice, int start, int end,
                        vector log_off_today, vector sex01,
                        matrix B_age, vector beta_age,
                        matrix B_year, vector beta_year,
                        real alpha, vector b_m, array[] int mun, real beta_sex) {
    int n = end - start + 1;
    vector[n] eta = alpha
                  + B_age[start:end,]  * beta_age
                  + B_year[start:end,] * beta_year
                  + beta_sex * sex01[start:end]
                  + b_m[mun[start:end]];
    return poisson_log_lpmf(y_slice | log_off_today[start:end] + eta);
  }
}
data {
  int<lower=1> N;
  array[N] int<lower=0> y;       // R^(today)
  vector[N] log_off_today;       // log_pop + log_std + log_c
  vector[N] log_off_lambda;      // log_pop + log_std (for GQ)
  vector[N] sex01;               // numeric 0/1

  int<lower=1> K_age;
  matrix[N, K_age] B_age;

  int<lower=1> K_year;
  matrix[N, K_year] B_year;

  int<lower=1> M;
  array[N] int<lower=1, upper=M> mun;

  int<lower=1> grainsize;
}
parameters {
  real alpha;
  vector[K_age]  beta_age;
  vector[K_year] beta_year;
  real beta_sex;

  vector[M] z_m;                 // non-centered municipal RE
  real<lower=0> sigma_m;
}
transformed parameters {
  vector[M] b_m = sigma_m * z_m;
}
model {
  // Priors
  alpha      ~ normal(0, 1.5);
  beta_age   ~ normal(0, 1);
  beta_year  ~ normal(0, 1);
  beta_sex   ~ normal(0, 1);
  z_m        ~ normal(0, 1);
  sigma_m    ~ exponential(1);

  // Threaded likelihood via reduce_sum
  target += reduce_sum(pois_partial_sum, y, grainsize,
                       log_off_today, sex01, B_age, beta_age,
                       B_year, beta_year, alpha, b_m, mun, beta_sex);
}
generated quantities {
  vector[N] eta = alpha
                + B_age  * beta_age
                + B_year * beta_year
                + beta_sex * sex01
                + b_m[mun];

  vector[N] mu_lambda = exp(log_off_lambda + eta);   // totals Λ (c_t=1)
  vector[N] mu_today  = exp(log_off_today  + eta);   // as-of-today mean

  array[N] int yrep;
  for (n in 1:N) yrep[n] = poisson_rng(mu_today[n]);
}
'
stan_file <- "nowcast_pois_plugin_yearly_nowcast_only_reduce_sum.stan"
writeLines(stan_code, stan_file)

# Prepare Stan data (with grainsize tuned to threads)
n_cores <- parallel::detectCores()
chains  <- min(4, n_cores)
threads <- max(1, floor(n_cores / chains))
grainsize <- max(200L, as.integer(floor(nrow(df_fit) / max(1, threads * 8))))

stan_data <- list(
  N = nrow(df_fit),
  y = df_fit$R_today,
  log_off_today  = df_fit$log_off_today,
  log_off_lambda = df_fit$log_off_lambda,
  sex01 = df_fit$sex01,
  K_age = K_age,
  B_age = unclass(as.matrix(B_age_fit)),
  K_year = K_year,
  B_year = unclass(as.matrix(B_year_fit)),
  M = M,
  mun = df_fit$mun_id,
  grainsize = grainsize
)

# Compile with threading enabled
mod <- cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))

# Helpful inits (no phi)
init_fun <- function(chain_id) {
  list(
    alpha = 0,
    beta_age  = rep(0, K_age),
    beta_year = rep(0, K_year),
    beta_sex  = 0,
    z_m       = rep(0, M),
    sigma_m   = 0.5
  )
}

# Sample (threads per chain used by reduce_sum)
fit <- mod$sample(
  data = stan_data,
  chains = chains, parallel_chains = chains,
  threads_per_chain = threads,
  iter_warmup = iter_warmup, iter_sampling = iter_sample,
  seed = 42, adapt_delta = adapt_delta, max_treedepth = max_treedepth,
  init = init_fun
)

# ======================= NOWCAST FROM POSTERIOR OF Λ (2017–2023) =============
draws <- fit$draws()

cell_map <- df_fit %>%
  mutate(cell_id = dplyr::row_number()) %>%
  select(cell_id, event_year, sex, municipality, age_group, c_t)

sel <- cell_map %>%
  filter(event_year %in% target_years, c_t < ct_cutoff)

Lambda_draws   <- posterior::as_draws_matrix(fit$draws("mu_lambda")) |> as.matrix()
mu_today_draws <- posterior::as_draws_matrix(fit$draws("mu_today"))  |> as.matrix()

if (nrow(sel) == 0) {
  warning("No cells in 2017–2023 with c_t < ", ct_cutoff, ". Skipping nowcast aggregation.")
  saveRDS(list(fit = fit,
               delay_counts = delay_counts,
               ct_tbl = ct_tbl,
               std_fert_national = std_fert_national),
          "input-data-processed/nowcast_bayes_yearly_NOWCAST_ONLY_outputs.rds")
  quit(save = "no")
}

stopifnot(ncol(Lambda_draws) == nrow(df_fit))
L_sel <- Lambda_draws[, sel$cell_id, drop = FALSE]
R_sel <- mu_today_draws[, sel$cell_id, drop = FALSE]

# Aggregate in draw space to year × sex × municipality
grp <- interaction(sel$event_year, sel$sex, sel$municipality, drop = TRUE)
G   <- model.matrix(~ 0 + grp)            # N_sel x G_groups
L_agg <- L_sel %*% G
R_agg <- R_sel %*% G

idx <- sel %>%
  mutate(grp = grp) %>%
  group_by(grp) %>%
  summarise(event_year = first(event_year),
            sex = first(sex),
            municipality = first(municipality),
            .groups = "drop")
col_order <- sub("^grp", "", colnames(G))
idx <- idx[match(col_order, as.character(idx$grp)), c("event_year","sex","municipality")]

qfun <- function(v) c(mean = mean(v), median = median(v),
                      p2.5 = quantile(v, .025), p97.5 = quantile(v, .975))
Lambda_nowcast_summary <- t(apply(L_agg, 2, qfun)) %>% as_tibble() %>%
  bind_cols(idx, .) %>%
  mutate(Rtoday_mean   = colMeans(R_agg),
         Remaining_mean = colMeans(L_agg) - colMeans(R_agg))

# Fix odd quantile colnames like "p2.5.2.5%" -> "lower", "p97.5.97.5%" -> "upper"
fix_qnames <- function(df) {
  nm <- names(df)
  nm <- sub("^p2\\.5.*$",  "lower", nm)
  nm <- sub("^p97\\.5.*$", "upper", nm)
  names(df) <- nm
  df
}
Lambda_nowcast_summary  <- fix_qnames(Lambda_nowcast_summary)

# National aggregates (sum over municipalities)
grp_nat <- interaction(sel$event_year, sel$sex, drop = TRUE)
G_nat   <- model.matrix(~ 0 + grp_nat)
L_nat   <- L_sel %*% G_nat
R_nat   <- R_sel %*% G_nat
nat_idx <- sel %>%
  mutate(grp_nat = grp_nat) %>%
  group_by(grp_nat) %>%
  summarise(event_year = first(event_year), sex = first(sex), .groups = "drop")
nat_col_order <- sub("^grp_nat", "", colnames(G_nat))
nat_idx <- nat_idx[match(nat_col_order, as.character(nat_idx$grp_nat)),
                   c("event_year","sex")]
Lambda_nowcast_nat <- t(apply(L_nat, 2, qfun)) %>% as_tibble() %>%
  bind_cols(nat_idx, .) %>%
  mutate(Rtoday_mean   = colMeans(R_nat),
         Remaining_mean = colMeans(L_nat) - colMeans(R_nat))
Lambda_nowcast_nat      <- fix_qnames(Lambda_nowcast_nat)

# ======================= SAVE OUTPUTS ========================================
saveRDS(list(
  fit = fit,
  delay_counts = delay_counts,
  ct_tbl = ct_tbl,
  std_fert_national = std_fert_national,
  nowcast_municipal = Lambda_nowcast_summary,
  nowcast_national  = Lambda_nowcast_nat,
  ran_quicktry = QuickTry
), "input-data-processed/nowcast_bayes_yearly_NOWCAST_ONLY_outputs.rds")

message("input-data-processed/\nDone. Saved: nowcast_bayes_yearly_NOWCAST_ONLY_outputs.rds")

# ======================= VISUALIZATION: YEARLY BARPLOTS =======================
# Observed-by-today totals to stack as blue bars
obs_nat <- R_today %>%
  group_by(event_year, sex) %>%
  summarise(R_obs = sum(R_today), .groups = "drop")

nat_plot <- Lambda_nowcast_nat %>%
  filter(event_year %in% 2017:2023) %>%
  rename(med = median) %>%
  left_join(obs_nat, by = c("event_year","sex")) %>%
  mutate(
    R_obs = coalesce(R_obs, 0),
    not_reported_med = pmax(0, med - R_obs)
  )

nat_bars <- nat_plot %>%
  select(event_year, sex, R_obs, not_reported_med) %>%
  tidyr::pivot_longer(c(R_obs, not_reported_med),
                      names_to = "part", values_to = "value") %>%
  mutate(part = dplyr::recode(part,
                              R_obs = "Registered by 2023-12-31",
                              not_reported_med = "Not yet reported (median)"))

p_nat <- ggplot() +
  geom_col(data = nat_bars,
           aes(x = event_year, y = value, fill = part),
           width = 0.8) +
  geom_ribbon(data = nat_plot,
              aes(x = event_year, ymin = lower, ymax = upper),
              inherit.aes = FALSE, alpha = 0.20, fill = "#5ab4ac") +
  geom_line(data = nat_plot,
            aes(x = event_year, y = med),
            inherit.aes = FALSE, linewidth = 1.0, color = "#01665e") +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Registered by 2023-12-31" = "#2c7fb8",
                               "Not yet reported (median)" = "#f46d43")) +
  labs(title = "Births by event year — nowcast as of 2023-12-31 (National)",
       x = "Event year", y = "Births", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor.x = element_blank())

print(p_nat)
ggsave("output/ch4/nowcast_yearly_barplot_NATIONAL.png", p_nat, width = 11, height = 7, dpi = 300)

# --- MUNICIPALITY plot helper -------------------------------------------------
plot_nowcast_municipality <- function(municipality_name) {
  obs_m <- R_today %>%
    filter(municipality == municipality_name) %>%
    group_by(event_year, sex) %>%
    summarise(R_obs = sum(R_today), .groups = "drop")
  
  lam_m <- Lambda_nowcast_summary %>%
    filter(municipality == municipality_name) %>%
    select(event_year, sex, median, `p2.5`, `p97.5`)
  
  if (nrow(lam_m) == 0) {
    stop("No nowcast summary for municipality: ", municipality_name)
  }
  
  plot_df <- lam_m %>%
    left_join(obs_m, by = c("event_year","sex")) %>%
    mutate(
      R_obs = coalesce(R_obs, 0),
      not_reported_med = pmax(0, median - R_obs)
    )
  
  bars <- plot_df %>%
    select(event_year, sex, R_obs, not_reported_med) %>%
    pivot_longer(c(R_obs, not_reported_med),
                 names_to = "part", values_to = "value") %>%
    mutate(part = recode(part,
                         R_obs = "Registered by 2023-12-31",
                         not_reported_med = "Not yet reported (median)"))
  
  p <- ggplot() +
    geom_col(data = bars,
             aes(x = event_year, y = value, fill = part),
             width = 0.8) +
    geom_ribbon(data = lam_m,
                aes(x = event_year, ymin = `p2.5`, ymax = `p97.5`),
                inherit.aes = FALSE, alpha = 0.20, fill = "#5ab4ac") +
    geom_line(data = lam_m,
              aes(x = event_year, y = median),
              inherit.aes = FALSE, linewidth = 1.0, color = "#01665e") +
    facet_wrap(~ sex, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c("Registered by 2023-12-31" = "#2c7fb8",
                                 "Not yet reported (median)" = "#f46d43")) +
    labs(title = paste0("Births by event year — nowcast as of 2023-12-31\nMunicipality: ",
                        municipality_name),
         x = "Event year", y = "Births", fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          panel.grid.minor.x = element_blank())
  
  outfile <- paste0("nowcast_yearly_barplot_MUNI_", municipality_name, ".png")
  ggsave(outfile, p, width = 11, height = 7, dpi = 300)
  message("Saved: ", outfile)
  invisible(p)
}

# ======================= NATIONAL SERIES 1990–2023 ============================
qfun <- function(v) c(mean = mean(v), median = median(v),
                      p2.5 = quantile(v, .025), p97.5 = quantile(v, .975))

Lambda_draws_all <- posterior::as_draws_matrix(fit$draws("mu_lambda")) |> as.matrix()
stopifnot(ncol(Lambda_draws_all) == nrow(df_fit))

cell_map_all <- df_fit %>%
  mutate(cell_id = dplyr::row_number()) %>%
  select(cell_id, event_year, sex)

grp_nat_all <- interaction(cell_map_all$event_year, cell_map_all$sex, drop = TRUE)
G_nat_all   <- model.matrix(~ 0 + grp_nat_all)              # Ncells x (years×sex)
L_nat_all   <- Lambda_draws_all %*% G_nat_all               # draws x groups

nat_idx_all <- cell_map_all %>%
  mutate(grp_nat_all = grp_nat_all) %>%
  group_by(grp_nat_all) %>%
  summarise(event_year = first(event_year), sex = first(sex), .groups = "drop")

nat_col_order <- sub("^grp_nat_all", "", colnames(G_nat_all))
nat_idx_all   <- nat_idx_all[match(nat_col_order, as.character(nat_idx_all$grp_nat_all)),
                             c("event_year","sex")]

Lambda_nat_all <- t(apply(L_nat_all, 2, qfun)) %>% as_tibble() %>%
  bind_cols(nat_idx_all, .)

fix_qnames <- function(df) {
  nm <- names(df)
  nm <- sub("^p2\\.5.*$",  "lower", nm)
  nm <- sub("^p97\\.5.*$", "upper", nm)
  names(df) <- nm
  df
}
Lambda_nat_all <- fix_qnames(Lambda_nat_all) %>% rename(med = median)

obs_nat_full <- R_today %>%
  group_by(event_year, sex) %>%
  summarise(R_obs = sum(R_today), .groups = "drop")

nat_full <- Lambda_nat_all %>%
  left_join(obs_nat_full, by = c("event_year","sex")) %>%
  left_join(ct_tbl, by = "event_year") %>%
  mutate(
    R_obs            = coalesce(R_obs, 0),
    not_reported_med = pmax(0, med - R_obs),
    incomplete       = c_t < 0.99
  ) %>%
  filter(event_year >= 1990, event_year <= 2023)

bars_full <- nat_full %>%
  select(event_year, sex, R_obs, not_reported_med) %>%
  tidyr::pivot_longer(c(R_obs, not_reported_med),
                      names_to = "part", values_to = "value") %>%
  mutate(part = dplyr::recode(part,
                              R_obs = "Registered by 2023-12-31",
                              not_reported_med = "Not yet reported (median)"))

p_nat_full <- ggplot() +
  geom_col(data = bars_full,
           aes(x = event_year, y = value, fill = part),
           width = 0.8) +
  geom_ribbon(data = nat_full,
              aes(x = event_year, ymin = lower, ymax = upper),
              inherit.aes = FALSE, alpha = 0.20, fill = "#5ab4ac") +
  geom_line(data = nat_full,
            aes(x = event_year, y = med),
            inherit.aes = FALSE, linewidth = 1.0, color = "#01665e") +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Registered by 2023-12-31" = "#2c7fb8",
                               "Not yet reported (median)" = "#f46d43")) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 2)) +
  labs(title = "Births by event year — nowcast as of 2023-12-31 (National, 1990–2023)",
       x = "Event year", y = "Births", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor.x = element_blank())

print(p_nat_full)
ggsave("output/ch4/nowcast_yearly_barplot_NATIONAL_1990_2023.png",
       p_nat_full, width = 12, height = 8, dpi = 300)

# === NATIONAL TOTAL (no sex facets): aggregate over sex in draw space ===
grp_year_only <- factor(cell_map_all$event_year)
G_year_only   <- model.matrix(~ 0 + grp_year_only)          # Ncells x years
L_year_only   <- Lambda_draws_all %*% G_year_only           # draws x years

yr_col_order  <- sub("^grp_year_only", "", colnames(G_year_only))
nat_total_idx <- tibble(event_year = as.integer(yr_col_order))

nat_total <- t(apply(L_year_only, 2, qfun)) %>%
  as_tibble() %>%
  bind_cols(nat_total_idx, .) %>%
  fix_qnames() %>%
  rename(med = median) %>%
  left_join(ct_tbl, by = "event_year") %>%
  arrange(event_year)

obs_nat_total <- R_today %>%
  group_by(event_year) %>%
  summarise(R_obs = sum(R_today), .groups = "drop")

nat_total <- nat_total %>%
  left_join(obs_nat_total, by = "event_year") %>%
  mutate(
    R_obs            = coalesce(R_obs, 0),
    not_reported_med = pmax(0, med - R_obs),
    incomplete       = c_t < 0.99
  )

bars_total <- nat_total %>%
  select(event_year, R_obs, not_reported_med) %>%
  tidyr::pivot_longer(c(R_obs, not_reported_med),
                      names_to = "part", values_to = "value") %>%
  mutate(part = dplyr::recode(part,
                              R_obs = "Registered by 2023-12-31",
                              not_reported_med = "Not yet reported (median)"))

p_nat_total <- ggplot() +
  geom_col(data = bars_total,
           aes(x = event_year, y = value, fill = part),
           width = 0.8) +
  geom_ribbon(data = nat_total,
              aes(x = event_year, ymin = lower, ymax = upper),
              inherit.aes = FALSE, alpha = 0.20, fill = "#5ab4ac") +
  geom_line(data = nat_total,
            aes(x = event_year, y = med),
            inherit.aes = FALSE, linewidth = 1.0, color = "#01665e") +
  scale_fill_manual(values = c("Registered by 2023-12-31" = "#2c7fb8",
                               "Not yet reported (median)" = "#f46d43")) +
  scale_x_continuous(breaks = seq(min(nat_total$event_year),
                                  max(nat_total$event_year), by = 2)) +
  labs(title = "Births by event year — nowcast as of 2023-12-31 (National total)",
       x = "Event year", y = "Births", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor.x = element_blank())

print(p_nat_total)
ggsave("output/ch4/nowcast_yearly_barplot_NATIONAL_TOTAL.png",
       p_nat_total, width = 12, height = 7.5, dpi = 300)

# ---- Posterior predictive for realized FINAL totals (sum over sex) ----
Yfinal_year <- matrix(
  rpois(length(L_year_only), lambda = as.numeric(L_year_only)),
  nrow = nrow(L_year_only), byrow = FALSE
)

qfun_pred <- function(v) c(pred_med = median(v),
                           p2.5 = quantile(v, .025),
                           p97.5 = quantile(v, .975))
pred_total <- t(apply(Yfinal_year, 2, qfun_pred)) %>% as_tibble()

fix_qnames <- function(df) {
  nm <- names(df)
  nm <- sub("^p2\\.5.*$",  "lower", nm)
  nm <- sub("^p97\\.5.*$", "upper", nm)
  names(df) <- nm
  df
}
pred_total <- fix_qnames(pred_total)

yr_col_order  <- sub("^grp_year_only", "", colnames(G_year_only))
nat_total_pred <- tibble(event_year = as.integer(yr_col_order)) %>%
  bind_cols(pred_total) %>%
  left_join(obs_nat_total, by = "event_year") %>%
  mutate(
    R_obs = coalesce(R_obs, 0),
    not_reported_med = pmax(0, pred_med - R_obs)
  )

bars_total <- nat_total_pred %>%
  select(event_year, R_obs, not_reported_med) %>%
  tidyr::pivot_longer(c(R_obs, not_reported_med),
                      names_to = "part", values_to = "value") %>%
  mutate(part = dplyr::recode(part,
                              R_obs = "Registered by 2023-12-31",
                              not_reported_med = "Not yet reported (median)"
  ))

p_nat_total_band <- ggplot() +
  geom_col(data = bars_total,
           aes(x = event_year, y = value, fill = part),
           width = 0.8) +
  geom_ribbon(data = nat_total_pred,
              aes(x = event_year, ymin = lower, ymax = upper),
              inherit.aes = FALSE, alpha = 0.20) +
  geom_line(data = nat_total_pred,
            aes(x = event_year, y = pred_med),
            inherit.aes = FALSE, linewidth = 1.0) +
  scale_fill_manual(values = c("Registered by 2023-12-31" = "#2c7fb8",
                               "Not yet reported (median)" = "#f46d43")) +
  labs(title = "Births by event year — nowcast as of 2023-12-31 (National total)",
       x = "Event year", y = "Births", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor.x = element_blank())

print(p_nat_total_band)
ggsave("output/ch4/nowcast_yearly_barplot_NATIONAL_TOTAL_with_pred_band.png",
       p_nat_total_band, width = 12, height = 7.5, dpi = 300)
