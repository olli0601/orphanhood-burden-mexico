# old-R/deprecated_helpers.R
# Deprecated/superseded helper variants extracted from the R/ helper files.
# Kept for reference only; not sourced by any active script.
# ============================================================================

# --- from R/preprocess_fertility.R ---
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

# --- from R/plots.R ---
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

# --- from R/municipality_grouping.R ---
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

