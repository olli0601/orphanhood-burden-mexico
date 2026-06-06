#-------------------------------------------------------------------------------
preprocess_fertility <- function(file) {
  # Extract year from the filename
  year <- str_extract(basename(file), "[0-9]{4}")
  
  # Read the file with appropriate delimiter depending on the year
  if (year == "2017") {
    # 1. Read 2017 file with ';' delimiter
    df <- read_delim(
      file,
      delim = ";",
      escape_double = FALSE,
      trim_ws = TRUE
    )
  } else {
    # 1. Check the file type (CSV or DBF)
    file_extension <- tools::file_ext(file)
    
    if (file_extension == "csv") {
      # 1a. Read the CSV file
      df <- read_csv(file) 
    } else if (file_extension == "dbf") {
      # 1b. Read the DBF file
      library(foreign)
      df <- read.dbf(file, as.is = FALSE)
    } else {
      stop("Unsupported file type. Please provide a CSV or DBF file.")
    }
  } 
 
  # 2. Standardize column names and filter invalid values
  df <- df |>
    rename_with(tolower) |> 
    select(ent_resid, mun_resid, edad_madn, edad_padn, ano_nac) |>
    rename(
      state      = ent_resid,
      mun        = mun_resid,
      age_mother = edad_madn,
      age_father = edad_padn,
      year       = ano_nac
    ) |>
    filter(mun != 999, state != 99, year != 9999) |>
    mutate(
      mun   = str_pad(mun, 3, pad = "0"),
      state = str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun)
    )
  
  # 3. Process female births (age groups 15–64)
  births_f <- df |>
    select(mun, year, age_mother) |>
    mutate(age = as.numeric(age_mother)) |>
    filter(age >= 15, age <= 64) |>
    mutate(age = paste0(
      15 + 5 * floor((age - 15) / 5), "-", 
      19 + 5 * floor((age - 15) / 5)
    )) |>
    select(year, age, mun) |>
    mutate(sex = "female")
  
  # 4. Process male births (age groups 15–84)
  births_m <- df |>
    select(mun, year, age_father) |>
    mutate(age = as.numeric(age_father)) |>
    filter(age >= 15, age <= 84) |>
    mutate(age = paste0(
      15 + 5 * floor((age - 15) / 5), "-", 
      19 + 5 * floor((age - 15) / 5)
    )) |>
    select(year, age, mun) |>
    mutate(sex = "male")
  
  # 5. Combine female and male births, aggregate counts
  births_final <- bind_rows(births_f, births_m) |>
    group_by(year, sex, age, mun) |>
    summarise(births = n(), .groups = "drop")
  
  return(births_final)
}

#-------------------------------------------------------------------------------
preprocess_fertility_long <- function(file) {
  year <- str_extract(basename(file), "[0-9]{4}")
  
  if (year == "2017") {
    df <- read_delim(
      file,
      delim = ";",
      escape_double = FALSE,
      trim_ws = TRUE
    )
  } else {
    file_extension <- tools::file_ext(file)
    
    if (file_extension == "csv") {
      df <- read_csv(file)
    } else if (file_extension == "dbf") {
      library(foreign)
      df <- read.dbf(file, as.is = FALSE)
    } else {
      stop("Unsupported file type. Please provide a CSV or DBF file.")
    }
  }
  
  df <- df |>
    rename_with(tolower) |>
    select(ent_resid, mun_resid, edad_madn, edad_padn, ano_nac) |>
    rename(
      state      = ent_resid,
      mun        = mun_resid,
      age_mother = edad_madn,
      age_father = edad_padn,
      year       = ano_nac
    ) |>
    filter(mun != 999, state != 99, year != 9999) |>
    mutate(
      mun   = str_pad(mun, 3, pad = "0"),
      state = str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun)
    )
  
  births_f <- df |>
    select(mun, year, age_mother) |>
    mutate(age = as.numeric(age_mother)) |>
    filter(age >= 15, age <= 64) |>
    select(year, age, mun) |>
    mutate(sex = "female")
  
  births_m <- df |>
    select(mun, year, age_father) |>
    mutate(age = as.numeric(age_father)) |>
    filter(age >= 15, age <= 84) |>
    select(year, age, mun) |>
    mutate(sex = "male")
  
  births_final <- bind_rows(births_f, births_m) |>
    group_by(year, sex, age, mun) |>
    summarise(births = n(), .groups = "drop")
  
  return(births_final)
}

#-------------------------------------------------------------------------------
preprocess_mortality <- function(file) {
  # 1. Check the file type (CSV or DBF)
  file_extension <- tools::file_ext(file)
  
  if (file_extension == "csv") {
    # 1a. Read the CSV file
    df <- read_csv(file) |>
      select(ent_resid, mun_resid, sexo, edad, anio_ocur) |>
      rename(
        state = ent_resid,
        mun   = mun_resid,
        sex   = sexo,
        age   = edad,
        year  = anio_ocur
      )
  } else if (file_extension == "dbf") {
    # 1b. Read the DBF file
    library(foreign)
    df <- read.dbf(file, as.is = FALSE) |>
      rename_with(tolower) |> 
      select(ent_resid, mun_resid, sexo, edad, anio_ocur) |>
      rename(
        state = ent_resid,
        mun   = mun_resid,
        sex   = sexo,
        age   = edad,
        year  = anio_ocur
      )
  } else {
    stop("Unsupported file type. Please provide a CSV or DBF file.")
  }
  
  # 2. Convert sex codes to labels ("1" -> "male", "2" -> "female")
  df <- df |>
    mutate(sex = case_when(
      sex == "1" ~ "male",
      sex == "2" ~ "female",
      TRUE       ~ NA_character_
    ))
  
  # 3. Filter out rows with invalid municipality, state, sex, or year values
  df <- df |>
    filter(mun != 999, state != 99, !is.na(sex), year != 9999)
  
  # 4. Format municipality and state codes and combine them
  df <- df |>
    mutate(
      mun   = str_pad(mun, 3, pad = "0"),
      state = str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun)
    )
  
  # 5. Filter and reformat age
  df <- df |>
    filter(
      (sex == "female" & age >= 4001 & age <= 4064) |
        (sex == "male"   & age >= 4001 & age <= 4085)
    ) |>
    mutate(age = as.numeric(age %% 100))
  
  # 6. Create age groups: keep 1-15 as is, group age > 15
  df <- df |>
    mutate(age = case_when(
      age >= 1 & age <= 14 ~ as.character(age),  # keep as is
      age == 15 ~ "15",  # single age group at the boundary
      sex == "female" & age > 15 & age <= 64 ~ paste0(
        15 + 5 * floor((age - 15) / 5), "-", 
        19 + 5 * floor((age - 15) / 5)
      ),
      sex == "male" & age > 15 & age <= 84 ~ paste0(
        15 + 5 * floor((age - 15) / 5), "-", 
        19 + 5 * floor((age - 15) / 5)
      ),
      TRUE ~ NA_character_
    ))
  
  
  # 7. Combine female and male datasets, convert variables to factors and aggregate
  df_final <- df |>
    group_by(year, sex, age, mun) |>
    summarise(deaths = n(), .groups = "drop")
  
  return(df_final)
}

#-------------------------------------------------------------------------------
preprocess_mortality_long <- function(file) {
  # ── 1. Read CSV or DBF ──────────────────────────────────────────────────
  file_extension <- tools::file_ext(file)
  
  if (file_extension == "csv") {
    df <- readr::read_csv(file, show_col_types = FALSE) |>
      dplyr::select(ent_resid, mun_resid, sexo, edad, anio_ocur) |>
      dplyr::rename(
        state = ent_resid,
        mun   = mun_resid,
        sex   = sexo,
        age   = edad,
        year  = anio_ocur
      )
  } else if (file_extension == "dbf") {
    df <- foreign::read.dbf(file, as.is = FALSE) |>
      dplyr::rename_with(tolower) |>
      dplyr::select(ent_resid, mun_resid, sexo, edad, anio_ocur) |>
      dplyr::rename(
        state = ent_resid,
        mun   = mun_resid,
        sex   = sexo,
        age   = edad,
        year  = anio_ocur
      )
  } else {
    stop("Unsupported file type. Please provide a CSV or DBF file.")
  }
  
  # ── 2. Re-code sex ──────────────────────────────────────────────────────
  df <- df |>
    dplyr::mutate(
      sex = dplyr::case_when(
        sex == "1" ~ "male",
        sex == "2" ~ "female",
        TRUE       ~ NA_character_
      )
    )
  
  # ── 3. Basic validity checks ────────────────────────────────────────────
  df <- df |>
    dplyr::filter(mun != 999, state != 99, !is.na(sex), year != 9999)
  
  # ── 4. Harmonise municipality codes ─────────────────────────────────────
  df <- df |>
    dplyr::mutate(
      mun   = stringr::str_pad(mun,   3, pad = "0"),
      state = stringr::str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun)
    )
  
  # ── 5. Keep valid single-age deaths and convert to numeric age ──────────
  df <- df |>
    dplyr::filter(
      (sex == "female" & age >= 4015 & age <= 4064) |
        (sex == "male"   & age >= 4015 & age <= 4085)
    ) |>
    dplyr::mutate(age = as.numeric(age %% 100))
  
  # ── 6. Aggregate by single age (no banding) ─────────────────────────────
  df_long <- df |>
    dplyr::group_by(year, sex, age, mun) |>
    dplyr::summarise(deaths = dplyr::n(), .groups = "drop")
  
  return(df_long)
}
#-------------------------------------------------------------------------------

preprocess_orphans <- function(file) {
  file_extension <- tools::file_ext(file)
  
  if (file_extension == "csv") {
    df <- readr::read_csv(file) |>
      dplyr::select(ent_resid, mun_resid, sexo, edad, anio_ocur) |>
      dplyr::rename(
        state = ent_resid,
        mun   = mun_resid,
        sex   = sexo,
        age   = edad,
        year  = anio_ocur
      )
  } else if (file_extension == "dbf") {
    df <- foreign::read.dbf(file, as.is = FALSE) |>
      dplyr::rename_with(tolower) |>
      dplyr::select(ent_resid, mun_resid, sexo, edad, anio_ocur) |>
      dplyr::rename(
        state = ent_resid,
        mun   = mun_resid,
        sex   = sexo,
        age   = edad,
        year  = anio_ocur
      )
  } else {
    stop("Unsupported file type. Please provide a CSV or DBF file.")
  }
  
  df <- df |>
    dplyr::mutate(
      sex = dplyr::case_when(
        sex == "1" ~ "male",
        sex == "2" ~ "female",
        TRUE       ~ NA_character_
      )
    ) |>
    dplyr::filter(
      mun != 999,
      state != 99,
      !is.na(sex),
      year != 9999,
      age <= 4017  # Filter age under 4017 only
    ) |>
    dplyr::mutate(
      mun   = stringr::str_pad(mun, 3, pad = "0"),
      state = stringr::str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun)
    ) |>
    dplyr::mutate(
      age = dplyr::case_when(
        age >= 4001 & age <= 4017 ~ age %% 100,
        age < 4001 ~ 0L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::group_by(year, sex, age, mun) |>
    dplyr::summarise(deaths = dplyr::n(), .groups = "drop")
  
  return(df)
}

#-------------------------------------------------------------------------------
correct_year <- function(df){
  df <- df %>%
    filter(year != 99) %>%
    mutate(
      year = as.numeric(paste0("19", str_pad(as.character(year), width = 2, side = "left", pad = "0")))
    )
  return(df)
}

#-------------------------------------------------------------------------------
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

plot_std_rate_old <- function (data, tt = "", y_lim = NULL, ...) {
  y_ran <- range(data$std_mort_rate)
  ggplot(data = data) +
    geom_point(mapping = aes(x = mpi, y = std_mort_rate, color = capital, size = capital)) + 
    scale_color_manual(name = "", values = c("#FF000099", "#0000FF99"), labels = c("Non-capital", "Department capital")) +
    scale_size_manual(name = "", values = c(1, 3), labels = c("Non-capital", "Department capital")) +
    geom_smooth(data = data[data$capital == 1, ], mapping = aes(x = mpi, y = std_mort_rate), method = "lm", formula = y ~ x, se = TRUE, color = "blue", linewidth = 0.5, linetype = "solid") + 
    geom_smooth(data = data[data$capital == 0, ], mapping = aes(x = mpi, y = std_mort_rate), method = "lm", formula = y ~ x, se = TRUE, color = "red",  linewidth = 0.5, linetype = "solid") +  
    #scale_x_continuous(breaks = seq(0.25, 1, 0.25), labels = seq(0.25, 1, 0.25), limits = c(0.25, 1), expand = c(0, 0)) +
    #scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
    labs(title = tt, x = "MPI", y = "Standardised rate") +
    # scale_x_log10() +
    # { if (!is.na(y_lim[1])) ylim(y_lim) } + 
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 12))
}


#-------------------------------------------------------------------------------

plot_std_rate <- function(data, tt = "", y_lim = NULL, ...) {
  # Detect which standardized rate is in the data
  y_var <- if ("std_mort_rate" %in% names(data)) "std_mort_rate" else "std_fert_rate"
  y_label <- if (y_var == "std_mort_rate") "Standardised mortality rate" else "Standardised fertility rate"
  
  # Dynamically get y-range if not supplied
  y_ran <- range(data[[y_var]], na.rm = TRUE)
  
  ggplot(data = data) +
    geom_point(aes(x = mpi, y = .data[[y_var]], color = capital, size = capital)) +
    scale_color_manual(
      name = "", 
      values = c("#FF000099", "#0000FF99"), 
      labels = c("Non-capital", "Department capital")
    ) +
    scale_size_manual(
      name = "", 
      values = c(1, 3), 
      labels = c("Non-capital", "Department capital")
    ) +
    geom_smooth(
      data = data[data$capital == 1, ], 
      aes(x = mpi, y = .data[[y_var]]), 
      method = "lm", 
      formula = y ~ x, 
      se = TRUE, 
      color = "blue", 
      linewidth = 0.5
    ) +
    geom_smooth(
      data = data[data$capital == 0, ], 
      aes(x = mpi, y = .data[[y_var]]), 
      method = "lm", 
      formula = y ~ x, 
      se = TRUE, 
      color = "red", 
      linewidth = 0.5
    ) +
    labs(
      title = tt,
      x = "MPI",
      y = y_label
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 12)
    )
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
group_small_municipalities <- function(mex_muni, threshold = 30000) {
  library(sf)
  library(dplyr)
  
  muni_ref <- mex_muni %>%
    mutate(
      total_pop = ifelse(is.na(total_pop), 0, total_pop),
      small = total_pop < threshold
    ) %>%
    st_make_valid()
  
  # Get neighbors using spatial touches
  neighbors <- sf::st_touches(muni_ref)
  
  # Initialize group ID with municipality code
  muni_ref$group_id <- muni_ref$mun
  
  # Loop over small municipalities
  small_indices <- which(muni_ref$small == TRUE)
  
  for (i in small_indices) {
    neighbors_i <- neighbors[[i]]
    
    if (length(neighbors_i) == 0) next
    
    pops <- muni_ref$total_pop[neighbors_i]
    
    if (all(is.na(pops))) next
    
    chosen <- neighbors_i[which.max(pops)]
    
    muni_ref$group_id[i] <- muni_ref$group_id[chosen]
  }
  
  # Aggregate geometries and population by group_id
  aggregated_muni <- muni_ref %>%
    group_by(group_id) %>%
    summarise(
      geometry = st_union(geometry),
      total_pop = sum(total_pop, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(aggregated_muni)
}


#-------------------------------------------------------------------------------
group_until_threshold_old <- function(mex_muni, threshold = 30000, max_iter = 50, stall_limit = 3) {
  library(sf)
  library(dplyr)
  
  # Step 1: Prepare input
  muni_ref <- mex_muni %>%
    mutate(
      total_pop = ifelse(is.na(total_pop), 0, total_pop),
      group_id = mun
    ) %>%
    st_make_valid()
  
  last_n_small <- Inf
  stall_counter <- 0
  
  for (iter in seq_len(max_iter)) {
    message("Iteration ", iter)
    
    # Step 2: Aggregate current groups
    aggregated <- muni_ref %>%
      group_by(group_id) %>%
      summarise(
        geometry = st_union(geometry),
        total_pop = sum(total_pop, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Step 3: Identify small groups
    small_groups <- aggregated %>%
      filter(total_pop < threshold)
    
    n_small <- nrow(small_groups)
    message("  Small groups remaining: ", n_small)
    
    # Stop if all groups meet the threshold
    if (n_small == 0) {
      message("All groups meet the population threshold.")
      break
    }
    
    # Stall detection
    if (n_small == last_n_small) {
      stall_counter <- stall_counter + 1
      message("  No progress. Stall count: ", stall_counter)
    } else {
      stall_counter <- 0
    }
    
    # Fallback if stall threshold exceeded
    if (stall_counter >= stall_limit) {
      warning("Stall limit reached. Applying fallback merge using nearest *large* group...")
      
      small_indices <- which(aggregated$total_pop < threshold)
      large_indices <- which(aggregated$total_pop >= threshold)
      
      if (length(large_indices) == 0) {
        warning("No large groups available for fallback merging. Stopping.")
        break
      }
      
      # Find nearest large group for each small group
      nearest_idx <- st_nearest_feature(aggregated[small_indices, ], aggregated[large_indices, ])
      
      for (i in seq_along(small_indices)) {
        source_id <- aggregated$group_id[small_indices[i]]
        target_id <- aggregated$group_id[large_indices[nearest_idx[i]]]
        
        if (source_id != target_id) {
          muni_ref <- muni_ref %>%
            mutate(
              group_id = ifelse(group_id == source_id, target_id, group_id)
            )
        }
      }
      
      stall_counter <- 0
      last_n_small <- Inf
      next
    }
    
    last_n_small <- n_small
    
    # Step 4: Compute group-level neighbors
    neighbors <- st_touches(aggregated)
    
    # Step 5: Merge small groups into largest neighbor
    for (i in seq_len(n_small)) {
      current_group_id <- small_groups$group_id[i]
      current_index <- which(aggregated$group_id == current_group_id)
      neighbor_ids <- neighbors[[current_index]]
      
      if (length(neighbor_ids) == 0) next
      
      neighbor_pops <- aggregated$total_pop[neighbor_ids]
      chosen_index <- neighbor_ids[which.max(neighbor_pops)]
      chosen_group_id <- aggregated$group_id[chosen_index]
      
      muni_ref <- muni_ref %>%
        mutate(
          group_id = ifelse(group_id == current_group_id, chosen_group_id, group_id)
        )
    }
  }
  
  # Final aggregation
  aggregated <- muni_ref %>%
    group_by(group_id) %>%
    summarise(
      geometry = st_union(geometry),
      total_pop = sum(total_pop, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    aggregated = aggregated,
    muni_ref_with_groups = muni_ref
  ))
}

#-------------------------------------------------------------------------------
group_until_threshold <- function(mex_muni, threshold = 30000, max_iter = 50) {
  library(sf)
  library(dplyr)
  library(units)
  
  # Initialize: clean and prepare base layer
  muni_ref <- mex_muni |>
    mutate(
      total_pop = if_else(is.na(total_pop), 0, total_pop),
      group_id = as.character(mun)
    ) |>
    st_make_valid()
  
  for (iter in seq_len(max_iter)) {
    # Pre-validate before group merge
    muni_ref <- muni_ref |>
      filter(
        st_is_valid(geometry),
        st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"),
        st_area(geometry) > set_units(0, "m^2")
      )
    
    # Merge by group_id
    aggregated <- muni_ref |>
      group_by(group_id) |>
      summarise(
        geometry = suppressWarnings(st_union(geometry)),
        total_pop = sum(total_pop, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(geometry = st_make_valid(geometry)) |>
      filter(
        st_is_valid(geometry),
        st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"),
        st_area(geometry) > set_units(0, "m^2")
      )
    
    # Identify small groups
    candidates <- aggregated |> filter(total_pop < threshold)
    if (nrow(candidates) == 0) {
      message("All groups meet the population threshold.")
      break
    }
    
    message("Iteration ", iter, ": Found ", nrow(candidates), " small municipality(ies).")
    
    # Process each small group
    for (i in seq_len(nrow(candidates))) {
      candidate_id <- candidates$group_id[i]
      candidate_index <- which(aggregated$group_id == candidate_id)
      candidate_geom <- aggregated[candidate_index, ]
      
      # Find neighbors via small buffer
      touches_idx <- st_touches(st_buffer(candidate_geom, 1e-8), aggregated)[[1]]
      touches_idx <- touches_idx[touches_idx != candidate_index]
      
      # Fallback to nearest neighbor
      if (length(touches_idx) == 0) {
        dists <- st_distance(candidate_geom, aggregated)
        dists[candidate_index] <- Inf
        touches_idx <- which.min(dists)
      }
      
      # Merge with neighbor with lowest population
      neighbor_idx <- touches_idx[which.min(aggregated$total_pop[touches_idx])]
      target_id <- aggregated$group_id[neighbor_idx]
      
      message("  Merging municipality ", candidate_id, " into neighbor ", target_id)
      
      muni_ref <- muni_ref |>
        mutate(
          group_id = if_else(
            !is.na(group_id) & group_id == candidate_id,
            target_id,
            group_id
          )
        )
    }
  }
  
  # Final aggregation
  aggregated_final <- muni_ref |>
    group_by(group_id) |>
    summarise(
      geometry = suppressWarnings(st_union(geometry)),
      total_pop = sum(total_pop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(geometry = st_make_valid(geometry)) |>
    filter(
      st_is_valid(geometry),
      st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"),
      st_area(geometry) > set_units(0, "m^2")
    )
  
  list(
    aggregated = aggregated_final,
    muni_ref_with_groups = muni_ref
  )
}

#-------------------------------------------------------------------------------
preprocess_fertility_day <- function(file) {
  ## --- 0. identify year & import ------------------------------------------------
  year <- stringr::str_extract(basename(file), "[0-9]{4}")
  
  if (year == "2017") {
    df <- readr::read_delim(file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  } else {
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- readr::read_csv(file)
    } else if (ext == "dbf") {
      df <- foreign::read.dbf(file, as.is = FALSE)
    } else {
      stop("Unsupported file type – supply a CSV or DBF")
    }
  }
  
  ## --- 1. normalise names -------------------------------------------------------
  df <- dplyr::rename_with(df, tolower)
  
  # Determine which column stores the month of occurrence
  month_occ_col <- intersect(
    names(df),
    c("met_nacim", "mes_nacim", "mes_nac")   # 2015+: met_nacim  |  pre-2015: mes_nac
  )
  if (length(month_occ_col) == 0)
    stop("No month-of-occurrence column (met_nacim / mes_nacim / mes_nac) found")
  
  ## --- 2. select / rename wanted variables --------------------------------------
  df <- df |>
    dplyr::select(
      ent_resid, mun_resid,
      edad_madn, edad_padn,
      ano_nac,
      !!month_occ_col,           # month of occurrence
      mes_reg          # registration date
    ) |>
    dplyr::rename(
      state       = ent_resid,
      mun         = mun_resid,
      age_mother  = edad_madn,
      age_father  = edad_padn,
      year        = ano_nac,
      #day_occ     = dia_nac,
      month_occ   = !!month_occ_col,
      #day_reg     = dia_reg,
      month_reg   = mes_reg
    ) |>
    dplyr::filter(
      mun   != 999,
      state != 99,
      year  != 9999,
      dplyr::between(day_occ,   1, 31),
      dplyr::between(month_occ, 1, 12),
      dplyr::between(day_reg,   1, 31),
      dplyr::between(month_reg, 1, 12)
    ) |>
    dplyr::mutate(
      mun   = stringr::str_pad(mun, 3, pad = "0"),
      state = stringr::str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun),
      date_occ = lubridate::make_date(year, month_occ),
      date_reg = lubridate::make_date(year, month_reg)
    )
  
  ## --- 3. build 5-year age groups & aggregate -----------------------------------
  births_f <- df |>
    dplyr::transmute(
      mun, date_occ, date_reg,
      sex = "female",
      age = as.numeric(age_mother)
    ) |>
    dplyr::filter(dplyr::between(age, 15, 64)) |>
    dplyr::mutate(
      age = paste0(
        15 + 5 * floor((age - 15) / 5), "-", 
        19 + 5 * floor((age - 15) / 5)
      )
    )
  
  births_m <- df |>
    dplyr::transmute(
      mun, date_occ, date_reg,
      sex = "male",
      age = as.numeric(age_father)
    ) |>
    dplyr::filter(dplyr::between(age, 15, 84)) |>
    dplyr::mutate(
      age = paste0(
        15 + 5 * floor((age - 15) / 5), "-", 
        19 + 5 * floor((age - 15) / 5)
      )
    )
  
  dplyr::bind_rows(births_f, births_m) |>
    dplyr::group_by(mun, sex, age, date_occ, date_reg) |>
    dplyr::summarise(births = dplyr::n(), .groups = "drop")
}

#-------------------------------------------------------------------------------
preprocess_fertility_month_old <- function(file) {
  ## --- 0. identify file-year & import ------------------------------------------
  file_year <- stringr::str_extract(basename(file), "\\d{4}")
  
  df <- if (file_year == "2017") {
    readr::read_delim(file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  } else {
    switch(tools::file_ext(file),
           csv = readr::read_csv(file, show_col_types = FALSE),
           dbf = foreign::read.dbf(file, as.is = FALSE),
           stop("Unsupported file type – supply a CSV or DBF")
    )
  }
  
  ## --- 1. normalise column names -----------------------------------------------
  df <- dplyr::rename_with(df, tolower)
  
  ## --- 2. locate month / day columns -------------------------------------------
  month_occ_col <- intersect(names(df), c("met_nacim", "mes_nacim", "mes_nac"))
  month_reg_col <- intersect(names(df), c("mes_reg", "mes_regis", "mes_regisn"))
  day_occ_col   <- intersect(names(df), c("dia_nacim", "dia_nac"))
  day_reg_col   <- intersect(names(df), c("dia_regis", "dia_reg"))
  
  if (length(month_occ_col) == 0 || length(month_reg_col) == 0)
    stop("Month column for occurrence or registration not found")
  
  ## --- 3. select & rename -------------------------------------------------------
  df <- df |>
    dplyr::select(
      ent_resid, mun_resid,
      edad_madn, edad_padn,
      ano_nac,
      all_of(month_occ_col), all_of(month_reg_col),
      tidyselect::any_of(day_occ_col), tidyselect::any_of(day_reg_col)
    ) |>
    dplyr::rename(
      state       = ent_resid,
      mun         = mun_resid,
      age_mother  = edad_madn,
      age_father  = edad_padn,
      year_occ    = ano_nac,          # <- birth year (may be 2-digit)
      month_occ   = !!month_occ_col,
      month_reg   = !!month_reg_col,
      !!if (length(day_occ_col)) setNames(day_occ_col, "day_occ"),
      !!if (length(day_reg_col)) setNames(day_reg_col, "day_reg")
    )
  
  ## --- 4. fix two-digit years (only 1990-1997) ---------------------------------
  df <- df |>
    dplyr::mutate(
      year_occ = as.numeric(year_occ),
      year_occ = dplyr::case_when(
        year_occ %in% 90:97 ~ year_occ + 1900,
        year_occ == 99      ~ NA_real_,
        TRUE                ~ year_occ
      ),
      year_reg = year_occ          # no separate column in source → assume same year
    )
  
  ## --- 5. validity checks -------------------------------------------------------
  df <- df |>
    dplyr::filter(
      !is.na(year_occ), mun != 999, state != 99, year_occ != 9999,
      dplyr::between(month_occ, 1, 12),
      dplyr::between(month_reg, 1, 12)
    )
  
  ## --- 6. harmonise administrative codes ---------------------------------------
  df <- df |>
    dplyr::mutate(
      mun   = stringr::str_pad(mun,   3, pad = "0"),
      state = stringr::str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun)
    )
  
  ## --- 7. age bands & aggregation ----------------------------------------------
  make_band <- function(a, upper) {
    a <- as.numeric(a)
    dplyr::case_when(
      a < 15 | a > upper ~ NA_character_,
      TRUE ~ sprintf("%d-%d",
                     15 + 5 * floor((a - 15) / 5),
                     19 + 5 * floor((a - 15) / 5))
    )
  }
  
  births <- dplyr::bind_rows(
    df |>
      dplyr::transmute(
        mun, year_occ, month_occ, year_reg, month_reg,
        sex  = "female",
        age  = make_band(age_mother, 64)
      ),
    df |>
      dplyr::transmute(
        mun, year_occ, month_occ, year_reg, month_reg,
        sex  = "male",
        age  = make_band(age_father, 84)
      )
  ) |>
    tidyr::drop_na(age) |>
    dplyr::group_by(mun, sex, age,
                    year_occ, month_occ,
                    year_reg, month_reg) |>
    dplyr::summarise(births = dplyr::n(), .groups = "drop")
  
  births
}
#-------------------------------------------------------------------------------
preprocess_fertility_month <- function(file) {
  
  ## --- 0. identify file-year & import ----------------------------------------
  file_year <- stringr::str_extract(basename(file), "\\d{4}") |> as.integer()
  
  df <- if (file_year == 2017) {
    readr::read_delim(file, delim = ";", escape_double = FALSE,
                      trim_ws = TRUE, show_col_types = FALSE)
  } else {
    switch(tools::file_ext(file),
           csv = readr::read_csv(file, show_col_types = FALSE),
           dbf = foreign::read.dbf(file, as.is = FALSE),
           stop("Unsupported file type – supply a CSV or DBF")
    )
  }
  
  ## --- 1. normalise column names --------------------------------------------
  df <- dplyr::rename_with(df, tolower)
  
  ## --- 2. locate month / day columns ----------------------------------------
  month_occ_col <- intersect(names(df), c("met_nacim", "mes_nacim", "mes_nac"))
  month_reg_col <- intersect(names(df), c("mes_reg", "mes_regis", "mes_regisn"))
  day_occ_col   <- intersect(names(df), c("dia_nacim", "dia_nac"))
  day_reg_col   <- intersect(names(df), c("dia_regis", "dia_reg"))
  
  if (length(month_occ_col) == 0 || length(month_reg_col) == 0)
    stop("Month column for occurrence or registration not found")
  
  ## --- 3. select & rename ----------------------------------------------------
  df <- df |>
    dplyr::select(
      ent_resid, mun_resid,
      edad_madn, edad_padn,
      ano_nac,
      all_of(month_occ_col), all_of(month_reg_col),
      tidyselect::any_of(day_occ_col), tidyselect::any_of(day_reg_col)
    ) |>
    dplyr::rename(
      state       = ent_resid,
      mun         = mun_resid,
      age_mother  = edad_madn,
      age_father  = edad_padn,
      year_occ    = ano_nac,
      month_occ   = !!month_occ_col,
      month_reg   = !!month_reg_col,
      !!if (length(day_occ_col)) setNames(day_occ_col, "day_occ"),
      !!if (length(day_reg_col)) setNames(day_reg_col, "day_reg")
    )
  
  ## --- 4. FIX two-digit years only for 1990-1997 ----------------------------
  df <- df |>
    dplyr::mutate(year_occ = as.numeric(year_occ))          # numeric first
  
  if (file_year %in% 1990:1997) {
    df <- df |>
      dplyr::rename(year = year_occ) |>                     # <-- temp name
      correct_year() |>                                     # apply YOUR helper
      dplyr::rename(year_occ = year)                        # restore name
  } else {
    # later files: just turn sentinel 99 into NA
    df <- df |>
      dplyr::mutate(year_occ = dplyr::na_if(year_occ, 99))
  }
  
  # Registration year is not in the early files → copy the (now-fixed) occ year
  df <- df |>
    dplyr::mutate(year_reg = year_occ)
  
  ## --- 5. validity checks ----------------------------------------------------
  df <- df |>
    dplyr::filter(
      !is.na(year_occ),
      mun   != 999,
      state != 99,
      year_occ != 9999,
      dplyr::between(month_occ,  1, 12),
      dplyr::between(month_reg,  1, 12)
    )
  
  ## --- 6. harmonise administrative codes ------------------------------------
  df <- df |>
    dplyr::mutate(
      mun   = stringr::str_pad(mun,   3, pad = "0"),
      state = stringr::str_pad(state, 2, pad = "0"),
      mun   = paste0(state, mun)
    )
  
  ## --- 7. age bands & aggregation -------------------------------------------
  make_band <- function(a, upper) {
    a <- as.numeric(a)
    dplyr::case_when(
      a < 15 | a > upper ~ NA_character_,
      TRUE               ~ sprintf("%d-%d",
                                   15 + 5 * floor((a - 15) / 5),
                                   19 + 5 * floor((a - 15) / 5))
    )
  }
  
  births <- dplyr::bind_rows(
    df |>
      dplyr::transmute(
        mun, year_occ, month_occ, year_reg, month_reg,
        sex  = "female",
        age  = make_band(age_mother, 64)
      ),
    df |>
      dplyr::transmute(
        mun, year_occ, month_occ, year_reg, month_reg,
        sex  = "male",
        age  = make_band(age_father, 84)
      )
  ) |>
    tidyr::drop_na(age) |>
    dplyr::group_by(
      mun, sex, age,
      year_occ, month_occ,
      year_reg, month_reg
    ) |>
    dplyr::summarise(births = dplyr::n(), .groups = "drop")
  
  return(births)
}

