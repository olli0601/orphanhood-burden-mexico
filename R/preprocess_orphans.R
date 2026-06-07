# Orphans preprocessing helper
# Helper functions split from the former R/utils.R
# =============================================================================

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
