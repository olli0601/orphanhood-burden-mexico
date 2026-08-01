# =============================================================================
# notebooks/export_ch1_data.R
# Dump the Chapter-1 stage (raw microdata -> per-year panels -> ch1_015
# intermediates) into open formats for notebooks/ch1_inputs_verification.ipynb.
# Read-only w.r.t. the pipeline: writes ONLY into input-data-processed/py-export/.
#
# Run from the repo root:  pixi run Rscript notebooks/export_ch1_data.R
# =============================================================================

suppressMessages({
  library(dplyr)
  library(arrow)
  library(stringr)
})

proc    <- "input-data-processed"
out_dir <- file.path(proc, "py-export")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(..., "\n")

# --- 1. Per-year panels: one row per source file -----------------------------
# Summarised in R because the panels are the raw, uncorrected form: the 2-digit
# occurrence year and the `99` unknown-year sentinel are still present here, and
# the notebook needs to see that without re-implementing the correction.
panel_summary <- function(subdir, prefix, value_col) {
  files <- sort(list.files(file.path(proc, subdir), pattern = "[.]RDS$", full.names = TRUE))
  bind_rows(lapply(files, function(f) {
    d <- readRDS(f)
    yr <- as.integer(str_extract(basename(f), "[0-9]{4}"))
    v  <- d[[value_col]]
    data.frame(
      source          = basename(f),
      year_reg        = yr,
      rows            = nrow(d),
      events          = sum(v),
      two_digit_year  = as.integer(any(d$year < 100)),
      sentinel_rows   = sum(d$year == 99),
      sentinel_events = sum(v[d$year == 99]),
      min_year        = min(d$year),
      max_year        = max(d$year),
      n_mun           = dplyr::n_distinct(d$mun)
    )
  }))
}
fert_panels <- panel_summary("fertility datasets", "fert", "births")
mort_panels <- panel_summary("mortality datasets", "mort", "deaths")
write_parquet(fert_panels, file.path(out_dir, "panel_summary_fertility.parquet"))
write_parquet(mort_panels, file.path(out_dir, "panel_summary_mortality.parquet"))
say("panels:", nrow(fert_panels), "fertility files,", nrow(mort_panels), "mortality files")

# --- 2. Concatenated event tables --------------------------------------------
births <- readRDS(file.path(proc, "births.RDS"))
deaths <- readRDS(file.path(proc, "deaths.RDS"))
write_parquet(births, file.path(out_dir, "births.parquet"))
write_parquet(deaths, file.path(out_dir, "deaths.parquet"))
say("births:", nrow(births), "rows | deaths:", nrow(deaths), "rows")

# --- 2b. Raw -> panel reconciliation for one reference year -------------------
# The panels are far smaller than the raw files, and the reason is a deliberate
# age restriction rather than data loss. Account for it exactly on one year, read
# straight from the DBF, so the notebook can show where every record goes.
ref_dbf <- "input-data-raw/deaths/defunciones_base_datos_2019/DEFUN19.DBF"
if (file.exists(ref_dbf)) {
  d <- foreign::read.dbf(ref_dbf, as.is = TRUE)
  names(d) <- tolower(names(d))
  ay  <- ifelse(d$edad >= 4000 & d$edad <= 4120, d$edad - 4000, NA)
  unk <- d$edad %in% c(4998, 4999)
  sx  <- ifelse(d$sexo == 1, "male", ifelse(d$sexo == 2, "female", NA))
  inband <- (((sx == "female" & ay >= 15 & ay <= 64) |
              (sx == "male"   & ay >= 15 & ay <= 84)) | unk) & !is.na(sx)
  inband[is.na(inband)] <- FALSE
  bad_geo <- inband & (d$mun_resid == 999 | d$ent_resid == 99 | d$anio_ocur == 9999)

  recon <- data.frame(
    step = c("raw records in DEFUN19.DBF",
             "sex not coded 1/2",
             "known age 0-14 (incl. infant codes < 4000)",
             "female 65+ (outside the maternal window)",
             "male 85+ (outside the paternal window)",
             "unknown municipality/state of residence, or unknown year",
             "kept in mort_2019.RDS"),
    records = c(nrow(d),
                -sum(is.na(sx)),
                -sum((!is.na(ay) & ay < 15) | d$edad < 4000),
                -sum(sx == "female" & ay >= 65, na.rm = TRUE),
                -sum(sx == "male"   & ay >= 85, na.rm = TRUE),
                -sum(bad_geo, na.rm = TRUE),
                sum(readRDS(file.path(proc, "mortality datasets", "mort_2019.RDS"))$deaths))
  )
  write_parquet(recon, file.path(out_dir, "raw_reconciliation_deaths_2019.parquet"))
  say("raw 2019 reconciliation: ", nrow(d), " raw ->", recon$records[nrow(recon)], "kept")
} else {
  say("SKIPPED raw reconciliation:", ref_dbf, "not extracted (run ch1_011)")
}

# --- 2c. Raw parental-age distribution ----------------------------------------
# The processed panels have no mothers above the 50-54 band. Export the raw
# single-year age distribution so the notebook can show whether that is a filter
# of ours or a property of the source.
raw_births <- "input-data-raw/births/natalidad_2019/conjunto_de_datos/conjunto_de_datos_natalidad_2019.csv"
if (file.exists(raw_births)) {
  nb <- readr::read_csv(raw_births, show_col_types = FALSE, progress = FALSE)
  names(nb) <- tolower(names(nb))
  age_dist <- bind_rows(
    data.frame(parent = "mother", age = suppressWarnings(as.numeric(nb$edad_madn))),
    data.frame(parent = "father", age = suppressWarnings(as.numeric(nb$edad_padn)))
  ) |>
    filter(!is.na(age)) |>
    count(parent, age, name = "records")
  write_parquet(age_dist, file.path(out_dir, "raw_parental_age_2019.parquet"))
  say("raw parental ages 2019:", nrow(nb), "birth records;",
      "max coded maternal age", max(age_dist$age[age_dist$parent == "mother" & age_dist$age < 90]))
} else {
  say("SKIPPED raw parental ages:", raw_births, "not extracted (run ch1_011)")
}

# --- 3. Municipality universes -----------------------------------------------
# Three different sources define "the municipalities of Mexico" and they do not
# agree. Export each list so the notebook can quantify the overlap rather than
# assume one of them is authoritative.
universes <- bind_rows(
  data.frame(source = "geo_info (CONAPO IMM 2020)",
             mun = as.character(readRDS(file.path(proc, "geo_info.RDS"))$mun)),
  data.frame(source = "population (CONAPO projections)",
             mun = as.character(unique(readRDS(file.path(proc, "population.RDS"))$mun))),
  data.frame(source = "GADM 4.1 polygons",
             mun = as.character(sf::st_drop_geometry(readRDS(file.path(proc, "mex_muni.RDS")))$mun))
)
write_parquet(universes, file.path(out_dir, "mun_universes.parquet"))
say("municipality universes:",
    paste(sprintf("%s=%d", unique(universes$source),
                  tapply(universes$mun, universes$source, length)[unique(universes$source)]),
          collapse = " | "))

# --- 4. Marginalization index -------------------------------------------------
write_parquet(readRDS(file.path(proc, "marg_index.RDS")),
              file.path(out_dir, "marg_index.parquet"))
write_parquet(readRDS(file.path(proc, "geo_info.RDS")),
              file.path(out_dir, "geo_info.parquet"))

# The deprivation indicators the IMN orientation can be checked against live in
# the raw CONAPO workbook, not in any processed file.
imm <- readxl::read_excel("input-data-raw/marginalization/IMM_2020.xls", skip = 5)
imm <- imm[-1, ]
write_parquet(as.data.frame(lapply(imm, as.character)),
              file.path(out_dir, "imm_2020_raw.parquet"))
say("IMM 2020 raw:", nrow(imm), "rows,", ncol(imm), "columns")

say("Wrote Chapter-1 exports to", normalizePath(out_dir))
