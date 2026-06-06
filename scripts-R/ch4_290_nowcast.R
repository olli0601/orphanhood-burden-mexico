# =============================================================================
# ch4_290_nowcast.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Standalone nowcast exploration (early prototype).
# Reads input-data-processed/fert.RDS.
# =============================================================================

# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(nimble)
library(scales)

# Set working directory and load data
births_df <- readRDS("input-data-processed/fert.RDS") |> filter(year >= 1990)

# Standardize factors
births_df <- births_df |>
  mutate(
    sex = factor(sex, levels = c("male", "female")),
    age = factor(age, levels = sort(unique(age)))  # Customize order if needed
  )

# Choose D_max for births (99% completeness)
D_max_births <- births_df %>%                       # original data
  group_by(year, delay) %>%                         # 1. totals per year-delay
  summarise(n = sum(tot_births), .groups = "drop") %>%
  group_by(year) %>%                                # 2. work year-by-year
  arrange(delay, .by_group = TRUE) %>%              #    order delays within year
  mutate(cum_prop = cumsum(n) / sum(n), .after = n) %>%
  ungroup()%>%
  filter(cum_prop >= 0.99) %>%
  summarise(D = min(delay)) %>% pull(D)

# Full time range and delays
years <- min(births_df$year):max(births_df$year)
delay_range_births <- 0:D_max_births

# Aggregate delay counts per stratum
delay_births <- births_df %>%
  group_by(year, group_id, sex, age, delay) %>%
  summarise(z = sum(tot_births), .groups = "drop")

# Extract exposure per stratum (no delay)
exposure_births <- births_df %>%
  group_by(year, group_id, sex, age) %>%
  summarise(exposure = first(tot_population), .groups = "drop")

# Create full stratum-delay grid
grid_births <- expand_grid(
  year  = years,
  group_id = unique(births_df$group_id),
  sex   = levels(births_df$sex),
  age   = levels(births_df$age),
  delay = delay_range_births
)


# Join counts and exposures; pivot to triangle format
triangle_births <- grid_births %>%
  left_join(delay_births, by = c("year", "group_id", "sex", "age", "delay")) %>%
  left_join(exposure_births, by = c("year", "group_id", "sex", "age")) %>%
  replace_na(list(z = 0)) %>%
  pivot_wider(names_from = delay, names_prefix = "z_", values_from = z, values_fill = 0) %>%
  mutate(y_tot = rowSums(across(starts_with("z_"))))


triangle_births <- triangle_births %>%
  mutate(
    sex = factor(sex, levels = c("male", "female")),  # define levels explicitly
    idx_sex = as.integer(sex),
    idx_age = as.integer(factor(age)),
    idx_grp = as.integer(factor(group_id)),
    idx_row = row_number()
  )
triangle_births <- triangle_births %>% filter(!is.na(group_id))


Zmat   <- as.matrix(select(triangle_births, starts_with("z_")))
N_rows <- nrow(Zmat)
Dp1    <- ncol(Zmat)  # D + 1

expos <- triangle_births$exposure
expos[is.na(expos)] <- median(expos, na.rm = TRUE)  # Simple imputation

nim_data <- list(
  Z     = Zmat,
  y_tot = triangle_births$y_tot,
  E     = expos,
  sex   = triangle_births$idx_sex,
  age   = triangle_births$idx_age,
  grp   = triangle_births$idx_grp
)

nim_consts <- list(
  N      = nrow(Zmat),
  Dp1    = ncol(Zmat),
  N_grp  = max(triangle_births$idx_grp),
  N_age  = max(triangle_births$idx_age)
)


#------------------------------------------------------------------------------
dnbinom_mu <- nimbleFunction(
  run = function(x = integer(0), size = double(0), mu = double(0), log = integer(0)) {
    returnType(double(0))
    prob <- size / (size + mu)
    return(dnbinom(x, size = size, prob = prob, log = log))
  }
)

rnbinom_mu <- nimbleFunction(
  run = function(n = integer(0), size = double(0), mu = double(0)) {
    returnType(integer(0))
    prob <- size / (size + mu)
    return(rnbinom(n = n, size = size, prob = prob))
  }
)

registerDistributions(list(
  dnbinom_mu = list(
    BUGSdist = "dnbinom_mu(size, mu)",
    types = c('value = integer(0)', 'size = double(0)', 'mu = double(0)'),
    discrete = TRUE
  )
))

#------------------------------------------------------------------------------
dbetabin <- nimbleFunction(
  run = function(x = integer(0), size = double(0), prob = double(0), phi = double(0), log = integer(0)) {
    returnType(double(0))
    
    alpha <- prob * phi
    beta  <- (1 - prob) * phi
    
    if (x < 0 || x > size) {
      return(if (log) -Inf else 0.0)
    }
    
    log_prob <- lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1) +
      lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta)
    
    if (log) return(log_prob)
    else return(exp(log_prob))
  }
)

rbetabin <- nimbleFunction(
  run = function(n = integer(0), size = double(0), prob = double(0), phi = double(0)) {
    returnType(integer(0))
    
    alpha <- prob * phi
    beta  <- (1 - prob) * phi
    p <- rbeta(1, alpha, beta)
    return(rbinom(1, size = size, prob = p))
  }
)

registerDistributions(list(
  dbetabin = list(
    BUGSdist = "dbetabin(size, prob, phi)",
    types = c("value = integer(0)", "size = double(0)", "prob = double(0)", "phi = double(0)"),
    discrete = TRUE
  )
))


#------------------------------------------------------------------------------
code <- nimbleCode({
  
  # ---------------- PRIORS ----------------
  beta0 ~ dnorm(0, sd = 10)
  
  for (s in 1:2) {
    alpha_sex[s] ~ dnorm(0, sd = 5)
  }
  
  for (a in 1:N_age) {
    beta_age[a] ~ dnorm(0, sd = 5)
  }
  
  for (g in 1:N_grp) {
    u_grp[g]   ~ dnorm(0, sd = sigma_u)
    theta[g]   ~ dgamma(2, 0.02)
  }
  
  sigma_u ~ dunif(0, 10)
  
  for (d in 1:Dp1) {
    phi[d] ~ dgamma(2, 0.02)
  }
  
  sigma_psi ~ dunif(0, 10)
  
  # -------- RW(1) on delay curves (psi) --------
  for (g in 1:N_grp) {
    psi_raw[g, 1] ~ dnorm(0, sd = 5)
    psi[g, 1] <- exp(psi_raw[g, 1])
    S[g, 1] <- ilogit(psi[g, 1])
    
    for (d in 2:Dp1) {
      delta_psi[g, d] ~ dnorm(0, sd = sigma_psi)
      psi_raw[g, d] <- psi_raw[g, d - 1] + delta_psi[g, d]
      psi[g, d] <- psi[g, d - 1] + exp(psi_raw[g, d])
      S[g, d] <- ilogit(psi[g, d])
    }
    
    S_diff[g, 1] <- S[g, 1]
    for (d in 2:Dp1) {
      S_diff[g, d] <- S[g, d] - S[g, d - 1]
    }
  }
  
  
  # ---------------- LIKELIHOOD ----------------
  for (i in 1:N) {
    log(lambda[i]) <- beta0 +
      alpha_sex[sex[i]] +
      beta_age[age[i]] +
      u_grp[grp[i]] +
      log(E[i])
    
    y_tot[i] ~ dnbinom_mu(size = theta[grp[i]], mu = lambda[i])
    
    y_remaining[i, 1] <- y_tot[i]  # 1-based index
    
    Z[i, 1] ~ dbetabin(size = y_remaining[i, 1],
                       prob = S_diff[grp[i], 1],
                       phi = phi[1])
    
    for (d in 2:Dp1) {
      y_remaining[i, d] <- y_remaining[i, d - 1] - Z[i, d - 1]
      Z[i, d] ~ dbetabin(size = y_remaining[i, d],
                         prob = S_diff[grp[i], d],
                         phi = phi[d])
    }
  }
  
})


cmodel <- nimbleModel(code, data = nim_data, constants = nim_consts, inits = NULL)


