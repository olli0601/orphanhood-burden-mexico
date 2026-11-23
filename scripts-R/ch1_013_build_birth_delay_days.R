# =============================================================================
# ch1_013_build_birth_delay_days.R  ·  Chapter 1 — Birth registration delay in days
# The per-year panels (ch1_012) keep only the year of birth and of registration.
# This step returns to the raw births microdata and counts births by municipality
# of residence x year and month of birth x days from birth to registration.
#
# Day of birth and day of registration exist from registration year 1998 on
# (NACIM85-97 carry month only), so birth cohorts 1998+ are fully covered.
# Records are filtered as in preprocess_fertility()'s mother rows (residence
# known, mother aged 15-50 or unknown), so each file's total by year of birth
# equals the female rows of fert_<YYYY>.RDS; this is asserted.
# delay_days is NA where a date is invalid or unknown, or registration precedes
# birth; those births are kept so the totals still match.
#
# Reads : input-data-raw/births/NACIM<YY>.dbf (1998-2016),
#         input-data-raw/births/natalidad_<YYYY>/conjunto_de_datos/*.csv (2017-2024),
#         input-data-processed/fertility datasets/fert_<YYYY>.RDS (check only)
# Writes: input-data-processed/birth delay days/delay_<YYYY>.RDS
#         (mun, year, month, delay_days, births; one file per registration year;
#         month is NA where the month of birth is unknown)
#         input-data-processed/.manifest/ch1_013.csv
# Run after: ch1_012; before ch3_051.
#
# A year whose file matches its manifest (rows, births) and has every output
# column is skipped;
# ORPHAN_FORCE_DELAY_DAYS=1 rebuilds all.
# =============================================================================
suppressMessages({
  library(data.table)
  source("R/manifest.R")
})

raw      <- "input-data-raw/births"
proc     <- "input-data-processed"
out_dir  <- file.path(proc, "birth delay days")
manifest <- file.path(proc, ".manifest", "ch1_013.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
force <- env_flag("ORPHAN_FORCE_DELAY_DAYS")
reg_years <- 1998:2024

births_file <- function(y) {
  cand <- if (y <= 2016) {
    file.path(raw, sprintf("NACIM%02d.%s", y %% 100, c("dbf", "DBF")))
  } else {
    d <- file.path(raw, sprintf("natalidad_%d", y), "conjunto_de_datos")
    file.path(d, c(sprintf("conjunto_de_datos_natalidad_%d.csv", y),
                   sprintf("conjunto_de_datos_enr%d.csv", y)))
  }
  hit <- cand[file.exists(cand)]
  if (!length(hit)) stop("no births microdata for registration year ", y)
  hit[[1]]
}

read_births <- function(f) {
  d <- if (tolower(tools::file_ext(f)) == "dbf") {
    as.data.table(foreign::read.dbf(f, as.is = TRUE))
  } else {
    fread(f, colClasses = "character", showProgress = FALSE)   # 2017 is ';', detected
  }
  setnames(d, tolower(names(d)))
  setnames(d, "mes_nacim", "mes_nac", skip_absent = TRUE)       # 2011, 2012, 2015, 2017
  need <- c("ent_resid", "mun_resid", "edad_madn",
            "dia_nac", "mes_nac", "ano_nac", "dia_reg", "mes_reg", "ano_reg")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop(basename(f), ": missing ", paste(miss, collapse = ", "))
  d <- d[, ..need]
  d[, (need) := lapply(.SD, \(x) suppressWarnings(as.integer(x))), .SDcols = need]
  d
}

build_year <- function(y) {
  id  <- sprintf("delay_%d", y)
  out <- file.path(out_dir, paste0(id, ".RDS"))
  if (!force) {
    chk <- panel_manifest_check(out, id, "births", manifest,
                                cols = c("mun", "year", "month", "delay_days", "births"))
    if (chk$ok) { message(sprintf("  [skip] %s verified", id)); return(invisible()) }
  }
  f <- births_file(y)
  d <- read_births(f)
  # Same rows as preprocess_fertility()'s mother rows.
  d <- d[ent_resid != 99 & mun_resid != 999 & ano_nac != 9999 &
           (between(edad_madn, 15, 50) | edad_madn == 99)]
  stopifnot(all(d$ano_reg == y))
  birth <- as.Date(sprintf("%04d-%02d-%02d", d$ano_nac, d$mes_nac, d$dia_nac), "%Y-%m-%d")
  reg   <- as.Date(sprintf("%04d-%02d-%02d", d$ano_reg, d$mes_reg, d$dia_reg), "%Y-%m-%d")
  d[, delay_days := as.integer(reg - birth)]
  d[delay_days < 0, delay_days := NA_integer_]
  d[, mun := sprintf("%02d%03d", ent_resid, mun_resid)]
  d[!between(mes_nac, 1, 12), mes_nac := NA_integer_]
  agg <- d[, .(births = .N), by = .(mun, year = ano_nac, month = mes_nac, delay_days)]

  # Payload check: births by year of birth must equal the panel's female rows.
  panel <- as.data.table(readRDS(file.path(proc, "fertility datasets", sprintf("fert_%d.RDS", y))))[
    sex == "female", .(panel = sum(births)), by = .(year = as.integer(year))]
  cmp <- merge(agg[, .(days = sum(births)), by = year], panel, by = "year", all = TRUE)
  bad <- cmp[is.na(days) | is.na(panel) | days != panel]
  if (nrow(bad)) { print(bad); stop(id, ": totals differ from fert_", y, ".RDS") }

  saveRDS(agg, out)
  table_manifest_upsert(manifest, id, nrow(agg), sum(agg$births))
  message(sprintf("  [ok  ] %s  %8d rows  %9d births  %.3f%% no delay  <- %s",
                  id, nrow(agg), sum(agg$births),
                  100 * agg[is.na(delay_days), sum(births)] / sum(agg$births), basename(f)))
}

message("== birth registration delay in days ==")
for (y in reg_years) build_year(y)
