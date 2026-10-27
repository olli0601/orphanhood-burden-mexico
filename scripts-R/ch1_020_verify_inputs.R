# =============================================================================
# ch1_020_verify_inputs.R  ·  Chapter 1 — Verify the acquired inputs
#
# Run this INSTEAD of re-running Chapter 1. Re-checks in seconds everything
# ch1_010/011/012/015 produce, inspecting the payload (member sizes, row counts,
# event totals) rather than paths, and exits non-zero naming what is wrong.
#
# Reads : input-data-raw/.manifest/*.tsv, input-data-processed/.manifest/ch1_012.csv,
#         the per-year panels, and the ch1_015 artefacts
# Writes: nothing
# Run after: ch1_012 (or any time you want to trust the inputs without rebuilding)
# =============================================================================

suppressMessages({ library(dplyr); source("R/manifest.R"); source("R/inputs.R") })

raw  <- "input-data-raw"
proc <- "input-data-processed"
fails <- character()
note  <- function(fmt, ...) message(sprintf(fmt, ...))
fail  <- function(fmt, ...) { m <- sprintf(fmt, ...); fails <<- c(fails, m); message("  [FAIL] ", m) }

# --- 1. raw archives --------------------------------------------------------
note("== raw archives (.tar.zst) ==")
archives <- list.files(raw, pattern = "[.]tar[.]zst$", recursive = TRUE, full.names = TRUE)
shared   <- manifest_shared_paths(raw)
if (!length(archives)) {
  fail("no .tar.zst archives found under %s/ — the committed raw inputs are absent", raw)
} else {
  bad <- 0L
  for (a in archives) {
    key <- sub("^input-data-raw/", "", a)
    chk <- manifest_check(key, raw, ignore = shared)
    if (!chk$ok) { fail("archive %s: %s", basename(a), chk$reason); bad <- bad + 1L }
  }
  note("  [ok  ] %d/%d archives verified (%d shared catalogue paths excluded)",
       length(archives) - bad, length(archives), length(shared))
}

# --- 2. downloaded + unzipped inputs ---------------------------------------
note("== downloaded inputs ==")
for (lbl in names(REQUIRED_RAW_FILES)) {
  f <- REQUIRED_RAW_FILES[[lbl]]
  if (!file.exists(f)) fail("%s missing: %s", lbl, f)
  else if (file.size(f) < MIN_RAW_BYTES) fail("%s is only %d bytes — likely an HTML stub, not data",
                                               lbl, file.size(f))
  else note("  [ok  ] %-32s %6.1f MB", lbl, file.size(f) / 1024^2)
}

# --- 3. per-year panels vs their manifest ----------------------------------
note("== per-year panels ==")
mf <- file.path(proc, ".manifest", "ch1_012.csv")
birth_years <- 1985:2024
death_years <- 1990:2024
check_panels <- function(dir, prefix, years, value_col) {
  bad <- 0L; tot <- 0
  for (y in years) {
    id  <- sprintf("%s_%d", prefix, y)
    chk <- panel_manifest_check(file.path(proc, dir, paste0(id, ".RDS")), id, value_col, mf)
    if (!chk$ok) { fail("panel %s: %s", id, chk$reason); bad <- bad + 1L; next }
    tot <- tot + chk$total
  }
  note("  [%s] %d/%d %s panels verified, %s %s total",
       if (bad) "FAIL" else "ok  ", length(years) - bad, length(years), prefix,
       format(tot, big.mark = ","), value_col)
}
check_panels("fertility datasets", "fert", birth_years, "births")
check_panels("mortality datasets", "mort", death_years, "deaths")

# --- 4. ch1_015 artefacts ---------------------------------------------------
note("== ch1_015 artefacts ==")
# Required columns of each (municipal) artefact.
expect <- list(
  births.RDS               = c("year", "sex", "age", "mun", "births", "year_reg"),
  deaths.RDS               = c("year", "sex", "age", "mun", "deaths", "year_reg"),
  population_mun_adult.RDS = c("mun", "sex", "year", "age", "population"),
  geo_info_mun.RDS         = c("state", "state_name", "mun", "mun_name", "capital"),
  marg_index_mun.RDS       = c("mun", "IMN", "GM"))
for (nm in names(expect)) {
  f <- file.path(proc, nm)
  if (!file.exists(f)) { fail("%s missing (run ch1_015)", nm); next }
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d)) { fail("%s is unreadable", nm); next }
  miss <- setdiff(expect[[nm]], names(d))
  if (length(miss)) { fail("%s lacks column(s) %s", nm, paste(miss, collapse = ", ")); next }
  if ("group_id" %in% names(d)) {
    fail("%s carries a group_id column — a grouped artefact has been written over a municipal name", nm)
    next
  }
  note("  [ok  ] %-24s %9s rows", nm, format(nrow(d), big.mark = ","))
}

pop <- tryCatch(readRDS(file.path(proc, "population_mun_adult.RDS")), error = function(e) NULL)
if (!is.null(pop) && "population" %in% names(pop)) {
  p2020 <- sum(pop$population[pop$year == 2020], na.rm = TRUE)
  # Adult bands only, hence well below the ~127.8M all-ages total.
  note("  [info] population_mun_adult 2020 total (ages 15-79): %s", format(round(p2020), big.mark = ","))
}

# --- 5. the two distinct `99` sentinels ------------------------------------
# Unrelated: 99 as unknown occurrence YEAR (2-digit-year files, dropped by
# load_year_panels()) and 99 as unknown parental AGE (age = NA in the panels,
# redistributed by load_birth_panels()).
note("== sentinels and ranges ==")
for (nm in c("births.RDS", "deaths.RDS")) {
  f <- file.path(proc, nm)
  if (!file.exists(f)) next
  d <- readRDS(f)
  note("  [info] %-11s year %d-%d, year_reg %d-%d, %d rows with year == 99",
       nm, min(d$year, na.rm = TRUE), max(d$year, na.rm = TRUE),
       min(d$year_reg, na.rm = TRUE), max(d$year_reg, na.rm = TRUE),
       sum(d$year == 99, na.rm = TRUE))
}
fert_files <- list.files(file.path(proc, "fertility datasets"), pattern = "[.]RDS$", full.names = TRUE)
if (length(fert_files)) {
  na_age <- dplyr::bind_rows(lapply(fert_files, function(f) {
    d <- readRDS(f)
    data.frame(sex = d$sex, unknown = ifelse(is.na(d$age), d$births, 0), births = d$births)
  })) |> dplyr::group_by(sex) |>
    dplyr::summarise(unknown = sum(unknown), births = sum(births), .groups = "drop")
  for (i in seq_len(nrow(na_age)))
    note("  [info] fertility panels: %s births (%.2f%%) with unknown %s age (INEGI 99), kept as age = NA",
         format(na_age$unknown[i], big.mark = ","), 100 * na_age$unknown[i] / na_age$births[i],
         ifelse(na_age$sex[i] == "female", "mother's", "father's"))
  note("         and redistributed across age bands by load_birth_panels().")
}

# --- verdict ---------------------------------------------------------------
if (length(fails)) {
  message("")
  stop(sprintf("ch1_020: %d check(s) failed:\n  - %s\n\nRe-run the relevant step; ORPHAN_FORCE_EXTRACT=1 / ORPHAN_FORCE_PANELS=1 force a rebuild.",
               length(fails), paste(fails, collapse = "\n  - ")), call. = FALSE)
}
message("\nch1_020: all checks passed. Chapter 1 inputs are intact — no need to re-run ch1_010/011/012.")
