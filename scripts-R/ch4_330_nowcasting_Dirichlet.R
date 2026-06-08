# =============================================================================
# ch4_330_nowcasting_Dirichlet.R  ·  Chapter 4 — Delay-adjusted nowcasting
# NIMBLE Dirichlet delay-distribution nowcasting exploration.
# Reads input-data-processed/mort_by_grouped_mun.RDS.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(nimble)
library(mgcv)
library(purrr)

# Load dataset and filter
deaths_df <- readRDS("input-data-processed/mort_by_grouped_mun.RDS") |>
  filter(year >= 1990)

# Aggregate delay distribution by region
delay_dist_region <- deaths_df |>
  group_by(group_id, delay) |>
  summarise(total = sum(tot_deaths), .groups = "drop") |>
  arrange(group_id, delay) |>
  group_by(group_id) |>
  mutate(
    cum_total = cumsum(total),
    cum_prop = cum_total / sum(total)
  ) |>
  ungroup()

# Empirical D_max by region (99% cutoff)
alpha <- 0.99
D_max_region <- delay_dist_region |>
  filter(cum_prop >= alpha) |>
  group_by(group_id) |>
  slice(1) |>
  select(group_id, D_max = delay)

# Build full delay cube
df_agg <- deaths_df |>
  group_by(year, group_id, delay) |>
  summarise(count = sum(tot_deaths), .groups = "drop")

years <- sort(unique(df_agg$year))
regions <- sort(unique(df_agg$group_id))
delays <- 0:max(df_agg$delay)

z_tsd <- array(0, dim = c(length(years), length(regions), length(delays)),
               dimnames = list(year = years, region = regions, delay = delays))

for (i in seq_along(years)) {
  for (j in seq_along(regions)) {
    tmp <- df_agg |> filter(year == years[i], group_id == regions[j])
    z_tsd[i, j, as.character(tmp$delay)] <- tmp$count
  }
}

# Prepare constants
T <- dim(z_tsd)[1]
S <- dim(z_tsd)[2]
D <- dim(z_tsd)[3]
N <- T * S
index <- expand.grid(t = 1:T, s = 1:S)
z_matrix <- array(as.vector(z_tsd), dim = c(N, D))

# Generate smoothers using mgcv::jagam
time_points <- rep(1:T, times = S)
region_ids <- rep(1:S, each = T)
spline_data <- data.frame(time = time_points, region = factor(region_ids))

jagam_model <- jagam(
  formula = ~ s(time, by = region, bs = "ps", k = 10),
  data = spline_data,
  file = tempfile(fileext = ".jags")
)


# Custom probit inverse for NIMBLE
probit_inverse <- nimbleFunction(
  run = function(x = double(0)) {
    returnType(double(0))
    return(pnorm(x))
  }
)
assign("probit_inverse", probit_inverse, envir = .GlobalEnv)

# Define model
gdm_code <- nimbleCode({
  for (n in 1:N) {
    # Latent total counts
    y[n] ~ dpois(lambda[n])
    log(lambda[n]) <- beta0 + region_effect[region[n]] + time_smooth[time_index[n]]
    
    # Survivor model for cumulative probability
    S[n, 0] <- 0
    for (d in 1:D) {
      eta[n, d] <- delay_intercept[d] +
        delay_region[region[n], d] +
        delay_smooth[time_index[n], d]
      S[n, d] <- probit_inverse(eta[n, d])
    }
    
    for (d in 1:D) {
      p[n, d] <- (S[n, d] - S[n, d - 1]) / (1 - S[n, d - 1])
      z[n, d] ~ dbetabin(mu = p[n, d], phi = phi[d], size = rem[n, d])
    }
    
    rem[n, 1] <- y[n]
    for (d in 1:(D - 1)) {
      rem[n, d + 1] <- rem[n, d] - z[n, d]
    }
  }
  
  # Priors
  beta0 ~ dnorm(0, 0.01)
  for (s in 1:S) {
    region_effect[s] ~ dnorm(0, 0.01)
    for (d in 1:D) {
      delay_region[s, d] ~ dnorm(0, 0.01)
    }
  }
  for (d in 1:D) {
    delay_intercept[d] ~ dnorm(0, 0.01)
    phi[d] ~ dgamma(2, 0.02)
  }
  
  # Smooth effects priors
  for (k in 1:ncol(jagam_model$jags.data$X)) {
    beta_smooth[k] ~ dnorm(0, sd = 10)
  }
  for (n in 1:N) {
    time_smooth[time_index[n]] <- inprod(jagam_model$jags.data$X[n, 1:ncol(jagam_model$jags.data$X)], beta_smooth[1:ncol(jagam_model$jags.data$X)])
  }
  
  for (t in 1:T) {
    for (d in 1:D) {
      delay_smooth[t, d] ~ dnorm(0, 0.01)
    }
  }
})

# Prepare data and constants
constants <- list(
  N = N, D = D, S = S, T = T,
  region = index$s,
  time_index = index$t
)

data <- list(
  z = z_matrix
)

# Initial values
inits <- list(
  y = rowSums(z_matrix) + 1,
  beta0 = 0,
  region_effect = rep(0, S),
  delay_intercept = rep(0, D),
  delay_region = matrix(0, S, D),
  delay_smooth = matrix(0, T, D),
  phi = rep(10, D),
  beta_smooth = rep(0, ncol(jagam_model$jags.data$X))
)

# Register beta-binomial and build model
assign("dbetabin", dbetabin, envir = .GlobalEnv)
registerDistributions(list(dbetabin = list(BUGSdist = "dbetabin(mu, phi, size)", discrete = TRUE)))

gdm_model <- nimbleModel(
  code = gdm_code,
  constants = constants,
  data = data,
  inits = inits
)

Cgdm <- compileNimble(gdm_model)

# MCMC setup
conf <- configureMCMC(Cgdm, monitors = c("y", "lambda", "phi", "beta0"))
Rmcmc <- buildMCMC(conf)
Cmcmc <- compileNimble(Rmcmc, project = Cgdm)

# Run MCMC
samples <- runMCMC(Cmcmc, niter = 10000, nburnin = 2000, thin = 5, nchains = 2)
