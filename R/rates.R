# Standardised rate computation helpers
# Helper functions split from the former R/utils.R
# =============================================================================

compute_std_fert_rate <- function(data) {
  # Set fertility-specific column names
  ctg <- "tot_births"
  rte <- "fert_rate"
  
  # Step 1: Build a reference population by group/sex/age
  pop_ref <- data |>
    dplyr::select(group_id, sex, age, population) |>
    group_by(group_id, sex, age) |>
    summarise(population = mean(population), .groups = "drop")
  
  # Step 2: Compute national standard population (by sex and age)
  p_nat <- pop_ref |>
    group_by(sex, age) |>
    summarise(p_nat = sum(population), .groups = "drop")
  
  p_nat_total <- sum(p_nat$p_nat)
  
  # Step 3: Calculate standardized fertility rate
  std_rate <- data |>
    left_join(p_nat, by = c("sex", "age")) |>
    mutate(std_rate = (p_nat / p_nat_total) * !!sym(rte)) |>
    group_by(group_id, year, sex) |>
    summarise(std_fert_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
  
  return(std_rate)
}

#-------------------------------------------------------------------------------
compute_std_mort_rate <- function(data) {
  ctg <- "tot_deaths"
  rte <- "mort_rate"
  
  # Step 1: Create reference population from 2023
  pop_ref <- data |>
    dplyr::select(group_id, sex, age, population) |>
    group_by(group_id, sex, age) |>
    summarise(population = mean(population), .groups = "drop")
  
  # Step 2: Build national standard population
  p_nat <- pop_ref |>
    group_by(sex, age) |>
    summarise(p_nat = sum(population), .groups = "drop")
  
  p_nat_total <- sum(p_nat$p_nat)
  
  # Step 3: Compute standardised mortality rate
  std_rate <- data |>
    left_join(p_nat, by = c("sex", "age")) |>
    mutate(std_rate = (p_nat / p_nat_total) * !!sym(rte)) |>
    group_by(group_id, year, sex) |>
    summarise(std_mort_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
  
  return(std_rate)
}

#-------------------------------------------------------------------------------


compute_std_rate_age_gender <- function (data, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")                 
  rte <- ifelse("deaths" %in% cns, "mort_rate", "fert_rate") 
  
  pop_ref  <- data %>% filter(year %in% 2018) %>% dplyr::select(group_id, sex, age, year, population) %>% group_by(group_id, sex, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(sex, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("group_id", "year", "age", "sex", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "sex"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(group_id, age, sex, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
  
  std_rate
}

#-------------------------------------------------------------------------------
compute_national_std_rate_age_gender <- function (data, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")                 
  rte <- ifelse("deaths" %in% cns, "mort_rate", "fert_rate") 
  
  pop_ref  <- data %>% filter(year %in% 2023) %>% dplyr::select(sex, age, year, population) %>% group_by(sex, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(sex, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("year", "age", "sex", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "sex"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(age, sex, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
  
  std_rate
}

#-------------------------------------------------------------------------------

compute_rate <- function (count, pop, ...) { 
  r <- (count / pop) 
  r[is.nan(r)] <- 0 # 0/0 
  # r[is.infinite(r)] <- 0 # x/0, x > 0
  r
}

#-------------------------------------------------------------------------------
range_0_1 <- function (x, ...) { (x - min(x)) / (max(x) - min(x)) }


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
min_max_norm <- function(x) {
  # TODO: check that reconstruction okay — original helper was not ported.
  # Assumed to be standard min-max scaling to [0, 1].
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#-------------------------------------------------------------------------------
compute_std_rate <- function(data, ref_year = 2020) {
  # TODO: check that reconstruction okay — original helper was not ported (the
  # compute_std_rate in ch1_030 uses a different schema). Reconstructed as a
  # direct age-sex standardisation of municipality death/birth rates to the
  # national `ref_year` population. Returns one row per (mun, year) with column
  # `std_mort_rate` (deaths) or `std_fert_rate` (births), matching plot_std_rate().
  is_mort <- "deaths" %in% names(data)
  cnt <- if (is_mort) "deaths" else "births"
  out <- if (is_mort) "std_mort_rate" else "std_fert_rate"
  ref <- data |>
    dplyr::filter(year == ref_year) |>
    dplyr::group_by(sex, age) |>
    dplyr::summarise(ref_pop = sum(population, na.rm = TRUE), .groups = "drop")
  tot_ref <- sum(ref$ref_pop, na.rm = TRUE)
  data |>
    dplyr::mutate(.rate = .data[[cnt]] / population) |>
    dplyr::left_join(ref, by = c("sex", "age")) |>
    dplyr::group_by(mun, year) |>
    dplyr::summarise("{out}" := sum(.rate * ref_pop, na.rm = TRUE) / tot_ref,
                     .groups = "drop")
}
