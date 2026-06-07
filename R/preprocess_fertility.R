# Fertility preprocessing helpers
# Helper functions split from the former R/utils.R
# =============================================================================

correct_year <- function(df){
  df <- df %>%
    filter(year != 99) %>%
    mutate(
      year = as.numeric(paste0("19", str_pad(as.character(year), width = 2, side = "left", pad = "0")))
    )
  return(df)
}

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
