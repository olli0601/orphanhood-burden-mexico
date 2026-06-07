# =============================================================================
# ch4_020_fertility_stan.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Hierarchical Bayesian Poisson nowcast for BIRTHS: municipality incidence anchored to the national age-sex fertility curve with a delay/completeness offset (CmdStan).
# Reads input-data-processed/{fert,geo_info,fertility_bias_data}.RDS, scripts-R/fertility_v1.stan -> writes output/ch4/*_fit.RDS.
# =============================================================================

# JOINT MODEL FOR NATIONAL COUNT AND MUNICIPALITY BIRTHS

suppressMessages({
  library(tidyverse); library(reshape2); library(cmdstanr); library(bayesplot)
})
source("R/aux.R")

data <- readRDS(file = "input-data-processed/fertility_bias_data.RDS")

fert      <- readRDS(file = "input-data-processed/fert.RDS")
geo_info  <- readRDS(file = "input-data-processed/geo_info.RDS")

# National fertility
nat_fert <- fert %>% dplyr::select(year, mun, sex, age, births, population) %>% group_by(year, sex, age) %>% summarise(births = sum(births), population = sum(population))

# Aggregated population based on the census year (i.e., 2018)
pop_2023    <- fert %>% filter(year == 2023) %>% dplyr::select(mun, sex, age, population)
p_nat       <- pop_2023 %>% group_by(sex, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
p_nat_mat   <- acast(p_nat, age ~ sex, value.var = "p_nat") # (A x G)
p_nat_total <- sum(pop_2023$population)
p_nat_mat_prop <- p_nat_mat / p_nat_total
p_nat_mat_prop_fem <- p_nat_mat_prop[, "female"]; p_nat_mat_prop_fem <- as.matrix(p_nat_mat_prop_fem[!is.na(p_nat_mat_prop_fem)]); colnames(p_nat_mat_prop_fem) <- "Female"; 
p_nat_mat_prop_mal <- p_nat_mat_prop[,   "male"]; p_nat_mat_prop_mal <- as.matrix(p_nat_mat_prop_mal[!is.na(p_nat_mat_prop_mal)]); colnames(p_nat_mat_prop_mal) <-   "Male"; 

mpi <- fert %>% filter(year == 2023, sex == "female", age == "15-19") %>% rename("mpi"="IMN") # Between 0 and 1
mpi <- mpi  %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% dplyr::select(mpi, capital)
mpi_capital <- mpi %>% filter(capital == 1) %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname()
mpi <- mpi %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname()

std_fertility_rate <- fert %>% dplyr::select(mun, year, age, sex, births, population, fert_rate)
std_fertility_rate <- std_fertility_rate %>% left_join(y = p_nat, by = c("age", "sex"))
std_fertility_rate <- std_fertility_rate %>% mutate(p_nat_total = p_nat_total)
std_fertility_rate <- std_fertility_rate %>% mutate(std_fertility_rate = ((p_nat / p_nat_total) * fert_rate))
std_fertility_rate <- std_fertility_rate %>% group_by(mun, year) %>% summarise(std_fertility_rate = sum(std_fertility_rate)) %>% ungroup()
std_fertility_rate <- std_fertility_rate %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")
std_fertility_rate_mat <- acast(std_fertility_rate, mun ~ year, value.var = "std_fertility_rate") # (L x Y)

std_fertility_rate_capital <- std_fertility_rate %>% filter(capital == 1) %>% dplyr::select(-capital)
std_fertility_rate_capital_mat <- acast(std_fertility_rate_capital, mun ~ year, value.var = "std_fertility_rate") # (C x Y)

##############################
# Stan model
##############################

stan_directory <- "src/stan/"
p <- "fertility_v1.stan"
m <- cmdstan_model(paste(stan_directory, p, sep = ""))

# Construct `data_list`
Y <- length(unique(fert$year))   # Total number of years
A_fem <- length(unique(fert[fert$sex == "Female", ]$age)) # Total number of age groups
A_mal <- length(unique(fert[fert$sex ==   "Male", ]$age)) # Total number of age groups
G <- length(unique(fert$sex)) # Total number of genders 
L <- length(unique(fert$mun))    # Total number of municipalities
C <- sum(geo_info$capital)       # Total number of capitals (or departments)

nat_fert_fem <- nat_fert %>% filter(sex == "Female") %>% dplyr::select(-sex)
nat_fert_mal <- nat_fert %>% filter(sex ==   "Male") %>% dplyr::select(-sex)
births_array_nat_fem <- acast(nat_fert_fem, year ~ age, value.var = "births") # (Y x A_fem)
births_array_nat_mal <- acast(nat_fert_mal, year ~ age, value.var = "births") # (Y x A_mal)
popula_array_nat_fem <- acast(nat_fert_fem, year ~ age, value.var = "population") # (Y x A_fem)
popula_array_nat_mal <- acast(nat_fert_mal, year ~ age, value.var = "population") # (Y x A_mal)

# Create data file for STAN
data_list <- list(
  # COUNTS
  Y = Y,
  A_fem = A_fem,
  A_mal = A_mal,
  G = G,
  L = L,
  C = C,
  
  # NATIONAL LEVEL
  births_nat_fem     = births_array_nat_fem, # Birth counts (Y x A_fem)
  births_nat_mal     = births_array_nat_mal, # Birth counts (Y x A_mal)
  population_nat_fem = popula_array_nat_fem, # Population   (Y x A_fem)
  population_nat_mal = popula_array_nat_mal, # Population   (Y x A_mal)
  
  # MUNICIPALITY LEVEL
  std_fertility_rate_capital = std_fertility_rate_capital_mat, # Pre-computed standardized fertility rates in the capitals (C x Y)
  proportion_pop_nat_fem = c(p_nat_mat_prop_fem),
  proportion_pop_nat_mal = c(p_nat_mat_prop_mal),
  
  # OTHERS
  age_value_fem = range_0_1(1:A_fem), # Age group values (A_fem)
  age_value_mal = range_0_1(1:A_mal), # Age group values (A_mal)
  mpi_municip = mpi,          # MPI in all municipalities (L)
  mpi_capital = mpi_capital   # MPI in the capitals (C)
)

# Fit the model
fitted_model <- m$sample(data = data_list,
                         seed = 1,             # Set seed for reproducibility
                         chains = 4,           # Number of Markov chains
                         parallel_chains = 4,  # Number of parallel chains
                         iter_warmup = 18000,  # Number of warm up iterations
                         iter_sampling = 2000, # Number of sampling iterations
                         thin = 4)             # Thinning (period between saved samples) to save memory

summ <- fitted_model$summary()

fitted_model$save_object(file = paste("output/ch4/", strsplit(p, "\\.")[[1]][1], "_fit.RDS", sep = ""))

d <- fitted_model$draws(variables = NULL, inc_warmup = FALSE, format = "draws_matrix")
saveRDS(object = list(data = data_list, draws = d), file = paste("output/ch4/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))

if (TRUE) { mcmc_trace(d, pars = c("alpha_0[1]", "fertility_rate_fem[1]", "inv_log_fertility_rate_nat_fem[1,1]", "std_fertility_rate_nat[1]",
                                   "gp_sigma_fem", "gp_sigma_mal", "gp_length_scale_fem", "gp_length_scale_mal")) }

