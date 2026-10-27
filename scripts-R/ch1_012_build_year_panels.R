# =============================================================================
# ch1_012_build_year_panels.R  ·  Chapter 1 — Raw microdata -> per-year panels
# Convert the raw INEGI microdata into the per-year panels the rest of the
# pipeline starts from, via preprocess_fertility() / preprocess_mortality().
#
# Reads : input-data-raw/births/NACIM<YY>.dbf                       (1985-2016)
#         input-data-raw/births/natalidad_<YYYY>/conjunto_de_datos/ (2017-2024)
#         input-data-raw/deaths/defunciones_base_datos_<YYYY>*/DEFUN<YY>.dbf (1990-2019)
#         input-data-raw/deaths/conjunto_de_datos/                  (2020-2024)
# Writes: input-data-processed/fertility datasets/fert_<YYYY>.RDS   (1985-2024)
#         input-data-processed/mortality datasets/mort_<YYYY>.RDS   (1990-2024)
# Run    : after ch1_010 + ch1_011; before ch1_015.
#
# Years: each file holds events REGISTERED that year (`year_reg`, added here);
# `year` is the year of occurrence. The 1985-1997 births and 1990-1997 deaths
# store it as 2 digits; load_year_panels() converts it at load time, so it is
# deliberately left alone here.
#
# A year whose panel matches its manifest (rows, event total) is skipped;
# ORPHAN_FORCE_PANELS=1 rebuilds all (~11 min).
# =============================================================================
suppressMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(foreign)
  source("R/preprocess_fertility.R")
  source("R/preprocess_mortality.R")
  source("R/manifest.R")
})

raw  <- "input-data-raw"
proc <- "input-data-processed"
fert_dir <- file.path(proc, "fertility datasets")
mort_dir <- file.path(proc, "mortality datasets")
dir.create(fert_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(mort_dir, recursive = TRUE, showWarnings = FALSE)

force <- env_flag("ORPHAN_FORCE_PANELS")
manifest_file <- file.path(proc, ".manifest", "ch1_012.csv")

# --- path resolution --------------------------------------------------------
# Explicit per-year candidates rather than a glob: the raw tree holds decoys
# (catalogue DBFs beside the births microdata, an 81 MB 2018 change-log CSV
# among the deaths), and INEGI filenames drift between years. Extension case
# varies too (.DBF/.dbf, .CSV/.csv), so both are probed.

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
# Schema: year, sex, age, mun, births|deaths, year_reg.
build_panel <- function(y, resolver, preprocess, out_dir, prefix, value_col) {
  id  <- sprintf("%s_%d", prefix, y)
  out <- file.path(out_dir, sprintf("%s.RDS", id))

  if (file.exists(out) && !force) {
    chk <- panel_manifest_check(out, id, value_col, manifest_file)
    if (chk$ok) {
      message(sprintf("  [skip] %s verified (%s rows)", id, format(chk$rows, big.mark = ",")))
      return(invisible(chk$total))
    }
    message(sprintf("  [redo] %s : %s", id, chk$reason))
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
    table_manifest_upsert(manifest_file, id, nrow(d), n)
    message(sprintf("  [ok  ] %s  %7d rows  %9d %s  <- %s",
                    id, nrow(d), n, value_col, basename(f)))
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
