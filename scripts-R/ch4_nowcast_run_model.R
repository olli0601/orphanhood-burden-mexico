library(nimble)
setwd("/Users/elsafarinella/Desktop/Orphanhood Mexico/R code")

# Load data
z_tsd <- readRDS("output/z_tsd.rds")
years <- readRDS("output/years.rds")
regions <- readRDS("output/groups.rds")
delays <- readRDS("output/delays.rds")

n_years <- length(years)
n_regions <- length(regions)
n_delays <- length(delays)
N <- n_years * n_regions

# Flatten to 2D matrix for z[n, d]
z_matrix <- matrix(0, nrow = N, ncol = n_delays)
index_map <- expand.grid(year = 1:n_years, region = 1:n_regions)

for (n in 1:N) {
  t <- index_map$year[n]
  s <- index_map$region[n]
  z_matrix[n, ] <- z_tsd[t, s, ]
}

# Initial values
y_init <- rowSums(z_matrix) + 1

# Constants
constants <- list(
  N = N,
  n_years = n_years,
  n_regions = n_regions,
  n_delays = n_delays,
  year_index = index_map$year,
  region = index_map$region
)

# Data
data <- list(z = z_matrix)

# Inits
inits <- list(
  y = y_init,
  beta0 = 0,
  region_effect = rep(0, n_regions),
  time_effect = rep(0, n_years),
  delay_intercept = rep(0, n_delays),
  delay_region = matrix(0, n_regions, n_delays),
  delay_smooth = matrix(0, n_years, n_delays),
  phi = rep(10, n_delays)
)

# Load model code
source("ch4_nowcast_model_code.R")

Sys.setenv(
  SDKROOT = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
  CXX = "clang++",
  CXXFLAGS = "-arch arm64 -std=gnu++17"
)

nimbleOptions(useSpecificInterfaceForModelValues = TRUE)

# Build and compile model
model <- nimbleModel(code = gdm_survivor_code, constants = constants, data = data, inits = inits)
compiled_model <- compileNimble(model, showCompilerOutput = TRUE)

# MCMC setup
conf <- configureMCMC(compiled_model, monitors = c("y", "lambda", "phi", "region_effect", "time_effect"))
Rmcmc <- buildMCMC(conf)
compiled_mcmc <- compileNimble(Rmcmc, project = compiled_model)

# Run MCMC
samples <- runMCMC(compiled_mcmc, niter = 10000, nburnin = 2000, thin = 5, nchains = 2)

# Save output
dir.create("results", showWarnings = FALSE)
saveRDS(samples, "results/posterior_samples.rds")
