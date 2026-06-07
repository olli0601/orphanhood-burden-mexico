# Mortality preprocessing helpers
# Helper functions split from the former R/utils.R
# =============================================================================

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
  # Keep parental ages only: females 15-64, males 15-84 (EDAD coded 40xx = years).
  df <- df |>
    filter(
      (sex == "female" & age >= 4015 & age <= 4064) |
        (sex == "male"   & age >= 4015 & age <= 4084)
    ) |>
    mutate(age = as.numeric(age %% 100))

  # 6. Five-year age groups, 15-19 ... ; age 15 folds into 15-19 (matches the
  #    supplied mort_YYYY data — verified bit-for-bit for adult bands).
  df <- df |>
    mutate(age = case_when(
      sex == "female" & age >= 15 & age <= 64 ~ paste0(
        15 + 5 * floor((age - 15) / 5), "-",
        19 + 5 * floor((age - 15) / 5)
      ),
      sex == "male" & age >= 15 & age <= 84 ~ paste0(
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


#-------------------------------------------------------------------------------
correct_mortality_year <- function(df) {
  # TODO: check that reconstruction okay — original helper was not ported.
  # Mirror of correct_year(): the early mortality files store the occurrence
  # year as 2 digits (90-98); convert to 19xx and drop the year==99 sentinel.
  df %>%
    dplyr::filter(year != 99) %>%
    dplyr::mutate(year = as.numeric(paste0(
      "19", stringr::str_pad(as.character(year), width = 2, side = "left", pad = "0"))))
}
