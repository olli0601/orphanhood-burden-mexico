# =============================================================================
# ch4_040_nowcast_model_code.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Model-code module for the lightweight nowcast pipeline: defines the model/likelihood functions sourced by the runner.
# Sourced by ch4_050_nowcast_run_model.R (no side effects).
# =============================================================================

library(nimble)

# Custom beta-binomial
dbetabin <- nimbleFunction(
  run = function(x = integer(0), mu = double(0), phi = double(0), size = double(0), log = integer(0)) {
    returnType(double(0))
    if (x < 0 || x > size) return(-Inf)
    alpha <- mu * phi
    beta <- (1 - mu) * phi
    log_prob <- lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1) +
      lgamma(x + alpha) + lgamma(size - x + beta) - lgamma(size + alpha + beta) +
      lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta)
    if (log) return(log_prob) else return(exp(log_prob))
  }
)
assign("dbetabin", dbetabin, envir = .GlobalEnv)

registerDistributions(list(
  dbetabin = list(BUGSdist = "dbetabin(mu, phi, size)", discrete = TRUE)
))

# Probit inverse
probit_inverse <- nimbleFunction(
  run = function(x = double(0)) {
    returnType(double(0))
    return(pnorm(x))
  }
)
assign("probit_inverse", probit_inverse, envir = .GlobalEnv)

# GDM Survivor model with corrected constant names
gdm_survivor_code <- nimbleCode({
  for (n in 1:N) {
    y[n] ~ dpois(lambda[n])
    log(lambda[n]) <- beta0 + region_effect[region[n]] + time_effect[year_index[n]]
    S[n, 1] <- 0  
    
    for (d in 1:n_delays) {
      eta[n, d] <- delay_intercept[d] + delay_smooth[year_index[n], d] + delay_region[region[n], d]
      S[n, d + 1] <- probit_inverse(eta[n, d])
    }
    
    for (d in 1:n_delays) {
      p[n, d] <- (S[n, d + 1] - S[n, d]) / (1 - S[n, d])
      z[n, d] ~ dbetabin(mu = p[n, d], phi = phi[d], size = rem[n, d])
    }
    
    rem[n, 1] <- y[n]
    for (d in 1:(n_delays - 1)) {
      rem[n, d + 1] <- rem[n, d] - z[n, d]
    }
  }
  
  beta0 ~ dnorm(0, 0.01)
  
  for (s in 1:n_regions) {
    region_effect[s] ~ dnorm(0, 0.01)
    for (d in 1:n_delays) {
      delay_region[s, d] ~ dnorm(0, 0.01)
    }
  }
  
  for (t in 1:n_years) {
    time_effect[t] ~ dnorm(0, 0.01)
    for (d in 1:n_delays) {
      delay_smooth[t, d] ~ dnorm(0, 0.01)
    }
  }
  
  for (d in 1:n_delays) {
    delay_intercept[d] ~ dnorm(0, 0.01)
    phi[d] ~ dgamma(2, 0.02)
  }
})

