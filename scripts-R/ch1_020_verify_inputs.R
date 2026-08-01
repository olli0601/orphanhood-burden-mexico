# =============================================================================
# ch1_020_verify_inputs.R  ·  Chapter 1 — Verify the acquired inputs
#
# Run this INSTEAD of re-running Chapter 1. It re-checks, in a few seconds,
# everything ch1_010/011/012/015 were supposed to produce, and exits non-zero
# with a specific message if anything is missing, truncated or inconsistent.
#
# It exists because the alternative — "the files are there, so it must be
# fine" — is precisely the reasoning that has failed repeatedly in this project:
# natalidad_2017..2023 reported "[skip] already extracted" while holding only
# catalogue files, and ch1_012 skipped panels on filename alone. Every check
# below inspects the payload (row counts, event totals, recorded member sizes),
# never just a path.
#
# Reads : input-data-raw/.manifest/*.tsv, input-data-processed/.manifest/ch1_012.csv,
#         the per-year panels, and the ch1_015 artefacts
# Writes: nothing
# Run after: ch1_012 (or any time you want to trust the inputs without rebuilding)
# =============================================================================

suppressMessages({ library(dplyr); source("R/manifest.R") })

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
required_files <- c(
  "CONAPO quinquennial population" =
    file.path(raw, "population", "00_Republica_mexicana", "1_Grupo_Quinq_00_RM.xlsx"),
  "IMM 2020" = file.path(raw, "marginalization", "IMM_2020.xls"),
  "IMM 2015" = file.path(raw, "marginalization", "IMM_DP2_2015.xlsx"),
  "IMM 2010" = file.path(raw, "marginalization", "IMM_DP2_2010.xlsx"),
  "GADM ADM2 geometries" = file.path(raw, "geometries", "gadm41_MEX_2.json"))
for (lbl in names(required_files)) {
  f <- required_files[[lbl]]
  if (!file.exists(f)) fail("%s missing: %s", lbl, f)
  else if (file.info(f)$size < 10000) fail("%s is only %d bytes — likely an HTML stub, not data",
                                           lbl, file.info(f)$size)
  else note("  [ok  ] %-32s %6.1f MB", lbl, file.info(f)$size / 1024^2)
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
    out <- file.path(proc, dir, paste0(id, ".RDS"))
    rec <- table_manifest_get(mf, id)
    if (!file.exists(out)) { fail("panel %s missing", id); bad <- bad + 1L; next }
    if (is.null(rec))      { fail("panel %s has no manifest entry — provenance unknown", id)
                             bad <- bad + 1L; next }
    d <- tryCatch(readRDS(out), error = function(e) NULL)
    if (is.null(d)) { fail("panel %s is unreadable", id); bad <- bad + 1L; next }
    n <- sum(d[[value_col]], na.rm = TRUE)
    if (nrow(d) != rec$rows || !isTRUE(all.equal(n, rec$total))) {
      fail("panel %s: manifest says %s rows / %s %s, found %s / %s",
           id, format(rec$rows, big.mark = ","), format(rec$total, big.mark = ","), value_col,
           format(nrow(d), big.mark = ","), format(n, big.mark = ","))
      bad <- bad + 1L; next
    }
    tot <- tot + n
  }
  note("  [%s] %d/%d %s panels verified, %s %s total",
       if (bad) "FAIL" else "ok  ", length(years) - bad, length(years), prefix,
       format(tot, big.mark = ","), value_col)
}
check_panels("fertility datasets", "fert", birth_years, "births")
check_panels("mortality datasets", "mort", death_years, "deaths")

# --- 4. ch1_015 artefacts ---------------------------------------------------
# These names are written by ch1_015 AND later overwritten by ch2_060 with
# different content, so the vintage on disk depends on how far the pipeline has
# run. Check the schema and plausibility, and report which vintage this looks
# like rather than pretending the name is unambiguous.
note("== ch1_015 artefacts ==")
# These names now have exactly ONE writer each, so a single schema per name is
# correct. Previously population.RDS / geo_info.RDS / marg_index.RDS were each
# written by both ch1_015 and ch2_060 with different content, and this table had
# to accept either — marg_index.RDS in particular was a municipal IMM table after
# ch1_015 and a group-level interpolated index after ch2_060. ch2_060's versions
# are now population_mun.RDS, geo_info_mun_grouped.RDS and index_grouped_mun.RDS.
expect <- list(
  births.RDS               = list(`ch1_015` = c("year", "sex", "age", "mun", "births", "year_reg")),
  deaths.RDS               = list(`ch1_015` = c("year", "sex", "age", "mun", "deaths", "year_reg")),
  population_mun_adult.RDS = list(`ch1_015 municipal, 15-79` = c("mun", "sex", "year", "age", "population")),
  geo_info_mun.RDS         = list(`ch1_015 municipal`        = c("state", "state_name", "mun", "mun_name", "capital")),
  marg_index_mun.RDS       = list(`ch1_015 municipal IMM`    = c("mun", "IMN", "GM")))
for (nm in names(expect)) {
  f <- file.path(proc, nm)
  if (!file.exists(f)) { fail("%s missing (run ch1_015)", nm); next }
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d)) { fail("%s is unreadable", nm); next }
  hits <- vapply(expect[[nm]], function(cols) all(cols %in% names(d)), logical(1))
  if (!any(hits)) {
    fail("%s matches no known schema. Has: %s. Expected: %s",
         nm, paste(names(d), collapse = ", "),
         paste(sprintf("%s (%s)", names(expect[[nm]]),
                       vapply(expect[[nm]], paste, character(1), collapse = "+")), collapse = " | "))
    next
  }
  # One writer per name now, so an unexpected group_id column means something
  # upstream has started overwriting a municipal artefact again.
  if ("group_id" %in% names(d)) {
    fail("%s carries a group_id column — a grouped artefact has been written over a municipal name", nm)
    next
  }
  note("  [ok  ] %-24s %9s rows  [%s]", nm, format(nrow(d), big.mark = ","),
       names(expect[[nm]])[which(hits)[1]])
}

pop <- tryCatch(readRDS(file.path(proc, "population_mun_adult.RDS")), error = function(e) NULL)
if (!is.null(pop) && "population" %in% names(pop)) {
  p2020 <- sum(pop$population[pop$year == 2020], na.rm = TRUE)
  # Adult bands only (15-79), so this is well below the ~127.8M all-ages total
  # that ch2_010 asserts against the CONAPO workbook. Expected, not wrong.
  note("  [info] population_mun_adult 2020 total (ages 15-79): %s", format(round(p2020), big.mark = ","))
}

# --- 5. the two distinct `99` sentinels ------------------------------------
# Keep both visible: one is an unknown occurrence YEAR in the two-digit-year
# files (dropped by load_year_panels); the other is unknown parental AGE in the
# INEGI microdata, which falls out of preprocess_fertility()'s 15-64/15-84
# filter without ever being counted. They are unrelated and easily conflated.
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
note("  [info] unknown parental age (INEGI code 99) is dropped upstream by")
note("         preprocess_fertility()'s age filter and is NOT counted anywhere;")
note("         see docs/audit/thesis_vs_code_backlog.md.")

# --- verdict ---------------------------------------------------------------
if (length(fails)) {
  message("")
  stop(sprintf("ch1_020: %d check(s) failed:\n  - %s\n\nRe-run the relevant step; ORPHAN_FORCE_EXTRACT=1 / ORPHAN_FORCE_PANELS=1 force a rebuild.",
               length(fails), paste(fails, collapse = "\n  - ")), call. = FALSE)
}
message("\nch1_020: all checks passed. Chapter 1 inputs are intact — no need to re-run ch1_010/011/012.")
