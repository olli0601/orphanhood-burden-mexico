# =============================================================================
# ch1_012_build_year_panels.R  ·  Chapter 1 — Raw microdata -> per-year panels
# Convert the raw INEGI microdata (extracted by ch1_011 / downloaded by ch1_010)
# into the per-year panels the rest of the pipeline starts from. This is the
# step that was missing: preprocess_fertility() / preprocess_mortality() already
# implement the conversion, but no script called them, so
# input-data-processed/{fertility,mortality} datasets/ could never be rebuilt.
#
# Reads : input-data-raw/births/NACIM<YY>.dbf                       (1985-2016)
#         input-data-raw/births/natalidad_<YYYY>/conjunto_de_datos/ (2017-2024)
#         input-data-raw/deaths/defunciones_base_datos_<YYYY>*/DEFUN<YY>.dbf (1990-2019)
#         input-data-raw/deaths/conjunto_de_datos/                  (2020-2024)
# Writes: input-data-processed/fertility datasets/fert_<YYYY>.RDS   (1985-2024)
#         input-data-processed/mortality datasets/mort_<YYYY>.RDS   (1990-2024)
# Run    : after ch1_010 + ch1_011; before ch1_015.
#
# NOTE ON YEARS. Each file holds the events REGISTERED in that year, so the
# filename year is the registration year (`year_reg`, added here) while `year`
# from the microdata is the year of OCCURRENCE and can be much earlier — that
# gap is the registration delay Chapter 3/4 analyse. The 1985-1997 births and
# 1990-1997 deaths files store the occurrence year as 2 DIGITS, and we
# deliberately leave it that way: the panels are consumed through
# load_year_panels() (R/load_year_panels.R:17-20), which converts 2-digit years
# and drops the 99 sentinel at load time. Calling correct_year() here would
# double-correct.
#
# Runtime: slow (~30 DBFs read whole into memory, plus 200MB+ CSVs). Completed
# years are skipped on re-run; set force <- TRUE to rebuild.
# =============================================================================
suppressMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(foreign)
  source("R/preprocess_fertility.R")
  source("R/preprocess_mortality.R")
})

raw  <- "input-data-raw"
proc <- "input-data-processed"
fert_dir <- file.path(proc, "fertility datasets")
mort_dir <- file.path(proc, "mortality datasets")
dir.create(fert_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(mort_dir, recursive = TRUE, showWarnings = FALSE)

force <- FALSE   # TRUE = rebuild years whose .RDS already exists

# --- path resolution --------------------------------------------------------
# Resolved per year from explicit rules rather than a recursive glob, because:
#   (a) the raw tree is littered with decoys a glob would happily pick up -
#       catalogue DBFs (ENTMUN.dbf, CATEMLN*.dbf) sit in the same flat births
#       folder as the microdata, ~180 files live under natalidad_*/catalogos/,
#       and deaths/conjunto_de_datos/ contains an 81 MB
#       defunciones_registradas_2018_bitacora_de_cambios.csv;
#   (b) INEGI filenames drift unpredictably (deaths 2020 uses the masculine
#       "registrados"; deaths 2023 has a "_csv" infix BEFORE the extension;
#       deaths 2024 is "registradas24_csv"; births switch from "natalidad_<YYYY>"
#       to "enr<YYYY>" in 2023), so patterns cannot be constructed reliably.
# Extension case also varies (NACIM16.DBF, DEFUN99/13/15/18/19.DBF, several
# .CSV), so every candidate is probed case-insensitively.

# Return the first candidate that exists, trying upper/lower-case extensions.
.resolve <- function(candidates) {
  for (p in candidates) {
    variants <- unique(c(p,
      sub("[.]([A-Za-z]+)$", ".\\U\\1", p, perl = TRUE),   # -> .DBF / .CSV
      sub("[.]([A-Za-z]+)$", ".\\L\\1", p, perl = TRUE)))  # -> .dbf / .csv
    hit <- variants[file.exists(variants)]
    if (length(hit)) return(hit[[1]])
  }
  NA_character_
}

births_path <- function(y) {
  yy <- sprintf("%02d", y %% 100)
  if (y <= 2016) {
    .resolve(file.path(raw, "births", sprintf("NACIM%s.dbf", yy)))
  } else {
    # "natalidad_<YYYY>" through 2022, "enr<YYYY>" from 2023 on.
    d <- file.path(raw, "births", sprintf("natalidad_%d", y), "conjunto_de_datos")
    .resolve(c(
      file.path(d, sprintf("conjunto_de_datos_natalidad_%d.csv", y)),
      file.path(d, sprintf("conjunto_de_datos_enr%d.csv", y))
    ))
  }
}

deaths_path <- function(y) {
  yy <- sprintf("%02d", y %% 100)
  if (y <= 2019) {
    # DBF era. Directory suffix "_dbf" is present up to 2014 and absent 2015-2019.
    # 2012-2019 ALSO exist as CSVs in deaths/conjunto_de_datos/; the DBF is used
    # deliberately so the whole 1990-2019 span comes from one consistent source.
    .resolve(c(
      file.path(raw, "deaths", sprintf("defunciones_base_datos_%d_dbf", y), sprintf("DEFUN%s.dbf", yy)),
      file.path(raw, "deaths", sprintf("defunciones_base_datos_%d",     y), sprintf("DEFUN%s.dbf", yy))
    ))
  } else {
    d <- file.path(raw, "deaths", "conjunto_de_datos")
    .resolve(c(
      file.path(d, sprintf("conjunto_de_datos_defunciones_registradas_%d.csv", y)),
      file.path(d, sprintf("conjunto_de_datos_defunciones_registrados_%d.csv", y)),   # 2020
      file.path(d, sprintf("conjunto_de_datos_defunciones_registradas_%d_csv.csv", y)),# 2023
      file.path(d, sprintf("conjunto_de_datos_defunciones_registradas%s_csv.csv", yy)) # 2024
    ))
  }
}

# --- build one panel --------------------------------------------------------
# Adds year_reg (the registration year, i.e. the file's year) so the output
# matches the schema load_year_panels() and ch1_015 expect:
#   year, sex, age, mun, births|deaths, year_reg
build_panel <- function(y, resolver, preprocess, out_dir, prefix, value_col) {
  out <- file.path(out_dir, sprintf("%s_%d.RDS", prefix, y))
  if (file.exists(out) && !force) {
    message(sprintf("  [skip] %s_%d already built", prefix, y))
    return(invisible(NA_integer_))
  }
  f <- resolver(y)
  if (is.na(f)) {
    message(sprintf("  [MISS] %d : no raw microdata file found", y))
    return(invisible(NA_integer_))
  }
  res <- tryCatch({
    d <- preprocess(f)
    d$year_reg <- y
    saveRDS(d, out)
    n <- sum(d[[value_col]], na.rm = TRUE)
    message(sprintf("  [ok  ] %s_%d  %7d rows  %9d %s  <- %s",
                    prefix, y, nrow(d), n, value_col, basename(f)))
    n
  }, error = function(e) {
    message(sprintf("  [FAIL] %d (%s) : %s", y, basename(f), conditionMessage(e)))
    NA_integer_
  })
  invisible(res)
}

# --- run -------------------------------------------------------------------
birth_years <- 1985:2024
death_years <- 1990:2024

message("== fertility panels (registered births) ==")
fert_n <- vapply(birth_years, function(y)
  as.numeric(build_panel(y, births_path, preprocess_fertility, fert_dir, "fert", "births")),
  numeric(1))

message("== mortality panels (registered deaths) ==")
mort_n <- vapply(death_years, function(y)
  as.numeric(build_panel(y, deaths_path, preprocess_mortality, mort_dir, "mort", "deaths")),
  numeric(1))

# --- summary ---------------------------------------------------------------
n_fert <- length(list.files(fert_dir, pattern = "[.]RDS$"))
n_mort <- length(list.files(mort_dir, pattern = "[.]RDS$"))
message(sprintf("\nch1_012: %d/%d fertility panels, %d/%d mortality panels present.",
                n_fert, length(birth_years), n_mort, length(death_years)))

missing_fert <- setdiff(birth_years, as.integer(str_extract(list.files(fert_dir, pattern = "[.]RDS$"), "[0-9]{4}")))
missing_mort <- setdiff(death_years, as.integer(str_extract(list.files(mort_dir, pattern = "[.]RDS$"), "[0-9]{4}")))
if (length(missing_fert) || length(missing_mort)) {
  if (length(missing_fert)) message("  missing fertility years: ", paste(missing_fert, collapse = ", "))
  if (length(missing_mort)) message("  missing mortality years: ", paste(missing_mort, collapse = ", "))
  stop("ch1_012: incomplete panels - see [MISS]/[FAIL] lines above. ",
       "Downstream scripts would silently produce partial results.", call. = FALSE)
}
message("Done. Next: scripts-R/ch1_015_bootstrap_from_processed.R")
