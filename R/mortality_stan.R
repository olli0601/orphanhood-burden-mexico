# JOINT MODEL FOR NATIONAL COUNT AND MUNICIPALITY DEATHS

source("header.R")
source("utils.R")

mort      <- readRDS(file = "../datasets/mort.RDS")
geo_info  <- readRDS(file = "../datasets/geo_info_new_mun.RDS")
mort <- mort |>
  filter(year >=1990)

mort <- mort |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))
mort <- mort |>
  filter(!(sex == "male" & age == "80-84"))

# National mortality
nat_mort <- mort %>% 
  dplyr::select(year, group_id, sex, age, tot_deaths, tot_population) %>% 
  group_by(year, sex, age) %>% 
  summarise(deaths = sum(tot_deaths), population = sum(tot_population))

# Aggregated population based on the census year (i.e., 2023)
pop_2023    <- mort %>% 
  filter(year == 2023) %>% 
  dplyr::select(group_id, sex, age, tot_population)

p_nat       <- pop_2023 %>% 
  group_by(sex, age) %>% 
  summarise(p_nat = sum(tot_population)) %>% 
  ungroup()

p_nat_mat   <- acast(p_nat, age ~ sex, value.var = "p_nat") # (A x G)
p_nat_total <- sum(pop_2023$tot_population, na.rm = TRUE)
p_nat_mat_prop <- p_nat_mat / p_nat_total
p_nat_mat_prop_fem <- p_nat_mat_prop[, "female"]; p_nat_mat_prop_fem <- as.matrix(p_nat_mat_prop_fem[!is.na(p_nat_mat_prop_fem)]); colnames(p_nat_mat_prop_fem) <- "Female"; 
p_nat_mat_prop_mal <- p_nat_mat_prop[,   "male"]; p_nat_mat_prop_mal <- as.matrix(p_nat_mat_prop_mal[!is.na(p_nat_mat_prop_mal)]); colnames(p_nat_mat_prop_mal) <-   "Male"; 

index_marg <- readRDS("../datasets/index_marg.RDS")
mpi <- left_join(mort, 
                 index_marg, 
                 by="group_id")
mpi <- mpi %>% 
  filter(year == 2023, sex == "female", age == "15-19") %>%
  rename("mpi"="marg_index_weighted")

mpi <- mpi  %>% 
  left_join(y = geo_info[, c("group_id", "capital")], 
            by = "group_id") %>% 
  dplyr::select(mpi, capital)

mpi_capital <- mpi %>% 
  filter(capital == 1) %>% 
  dplyr::select(mpi) %>% 
  c() %>% 
  unlist() %>% 
  unname()

mpi <- mpi %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname()

mort$mort_rate <- mort$tot_deaths / mort$tot_population

std_mortality_rate <- mort %>% 
  dplyr::select(group_id, year, age, sex, tot_deaths, tot_population, mort_rate)

std_mortality_rate <- std_mortality_rate %>% 
  left_join(y = p_nat, by = c("age", "sex"))

std_mortality_rate <- std_mortality_rate %>% mutate(p_nat_total = p_nat_total)

std_mortality_rate <- std_mortality_rate %>% 
  mutate(std_mortality_rate = ((p_nat / p_nat_total) * mort_rate))

std_mortality_rate <- std_mortality_rate %>% 
  group_by(group_id, year) %>% summarise(std_mortality_rate = sum(std_mortality_rate)) %>% 
  ungroup()

std_mortality_rate <- std_mortality_rate %>% 
  left_join(y = geo_info[, c("group_id", "capital")], by = "group_id")

std_mortality_rate_mat <- acast(std_mortality_rate, group_id ~ year, value.var = "std_mortality_rate") # (L x Y)

std_mortality_rate_capital <- std_mortality_rate %>% filter(capital == 1) %>% dplyr::select(-capital)
std_mortality_rate_capital_mat <- acast(std_mortality_rate_capital, group_id ~ year, value.var = "std_mortality_rate") # (C x Y)

##############################
# Stan model
##############################

stan_directory <- "../STAN/"
p <- "mortality_v1.stan"
m <- cmdstan_model(paste(stan_directory, p, sep = ""))

# Construct `data_list`
Y <- length(unique(mort$year))   # Total number of years
A_fem <- length(unique(mort[mort$sex == "female", ]$age)) # Total number of age groups
A_mal <- length(unique(mort[mort$sex ==   "male", ]$age)) # Total number of age groups
G <- length(unique(mort$sex)) # Total number of genders 
L <- length(unique(mpi))    # Total number of municipalities
C <- sum(geo_info$capital)       # Total number of capitals (or departments)

nat_mort_fem <- nat_mort %>% filter(sex == "female") %>% dplyr::select(-sex)
nat_mort_mal <- nat_mort %>% filter(sex ==   "male") %>% dplyr::select(-sex)
deaths_array_mort_fem <- acast(nat_mort_fem, year ~ age, value.var = "deaths") # (Y x A_fem)
deaths_array_mort_mal <- acast(nat_mort_mal, year ~ age, value.var = "deaths") # (Y x A_mal)
popula_array_mort_fem <- acast(nat_mort_fem, year ~ age, value.var = "population") # (Y x A_fem)
popula_array_mort_mal <- acast(nat_mort_mal, year ~ age, value.var = "population") # (Y x A_mal)

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
  deaths_nat_fem     = deaths_array_mort_fem,
  deaths_nat_mal     = deaths_array_mort_mal,
  population_nat_fem = popula_array_mort_fem,  
  population_nat_mal = popula_array_mort_mal,  
  
  # MUNICIPALITY LEVEL
  std_death_rate_capital = std_mortality_rate_capital_mat,  
  proportion_pop_nat_fem = c(p_nat_mat_prop_fem),            
  proportion_pop_nat_mal = c(p_nat_mat_prop_mal),             
  
  # OTHERS
  age_value_fem = range_0_1(1:A_fem),
  age_value_mal = range_0_1(1:A_mal),
  mpi_municip = mpi,
  mpi_capital = mpi_capital
)


# Fit the model
fitted_model <- m$sample(data = data_list,
                         seed = 1,             # Set seed for reproducibility
                         chains = 4,           # Number of Markov chains
                         parallel_chains = 4,  # Number of parallel chains
                         iter_warmup = 1800,  # Number of warm up iterations
                         iter_sampling = 200, # Number of sampling iterations
                         thin = 4)             # Thinning (period between saved samples) to save memory

summ <- fitted_model$summary()
posterior::summarise_draws(fitted_model$draws())
fitted_model$diagnostic_summary()
fitted_model$cmdstan_diagnose()



fitted_model$save_object(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_fit.RDS", sep = ""))

d <- fitted_model$draws(variables = NULL, inc_warmup = FALSE, format = "draws_matrix")
saveRDS(object = list(data = data_list, draws = d), file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))

if (TRUE) { mcmc_trace(d, pars = c("alpha_0[1]", "mortality_rate_fem[1]", "inv_log_mortality_rate_nat_fem[1,1]", "std_mortality_rate_nat[1]",
                                   "gp_sigma_fem", "gp_sigma_mal", "gp_length_scale_fem", "gp_length_scale_mal")) }

