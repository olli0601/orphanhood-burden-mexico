# =============================================================================
# ch1_011_extract_raw_archives.R  ·  Chapter 1 — Extract manual raw archives
# The request-gated manual inputs (pre-2017 registered births + all registered
# deaths) are stored compressed as .tar.zst under input-data-raw/{births,deaths}/
# to keep the repository small. This script extracts every .tar.zst in place
# (next to the archive): zstd (CLI, from pixi) decompresses to .tar, then R's
# built-in untar() unpacks it. Works on Linux/macOS/Windows.
#
# Reads : input-data-raw/{births,deaths}/**/*.tar.zst
# Writes: the extracted folder tree beside each archive
# Run    : only if you need the raw files (e.g. to regenerate the per-year
#          panels from scratch). The default pipeline runs from the already-
#          processed datasets and does NOT require this step.
#
# Windows note: the archives are standard .tar.zst, so they also open directly
# in 7-Zip (>= 21.01) without R. This script just makes it reproducible via pixi.
# =============================================================================

zstd_bin <- Sys.which("zstd")
if (!nzchar(zstd_bin)) {
  stop("`zstd` not on PATH. Run inside the pixi environment (pixi run Rscript ...) ",
       "or install zstd / 7-Zip and extract manually.")
}

raw_dir  <- "input-data-raw"
archives <- list.files(raw_dir, pattern = "[.]tar[.]zst$", recursive = TRUE,
                       full.names = TRUE)

if (length(archives) == 0) {
  message("ch1_011: no .tar.zst archives found under ", raw_dir, "/ — nothing to do.")
} else {
  message(sprintf("ch1_011: extracting %d archive(s)...", length(archives)))
  for (a in archives) {
    dest <- dirname(a)                       # extract beside the archive
    tarf <- sub("[.]zst$", "", a)            # foo.tar.zst -> foo.tar
    ok <- tryCatch({
      # zstd -d -f -k : decompress, overwrite, keep the .zst archive
      st <- system2(zstd_bin, c("-d", "-f", "-k", shQuote(a)))
      if (st != 0) stop("zstd exit ", st)
      untar(tarf, exdir = dest)
      file.remove(tarf)                      # drop the intermediate .tar
      TRUE
    }, error = function(e) {
      message("  [FAIL] ", basename(a), " : ", conditionMessage(e)); FALSE
    })
    if (ok) message("  [ok  ] ", basename(a))
  }
  message("ch1_011: done. (Archives are kept; delete them manually if not needed.)")
}

# --- INEGI births 2017-2024 (.zip, downloaded by ch1_010) --------------------
# ch1_010 downloads these and unzips them itself, but its skip condition is the
# existence of the target DIRECTORY. A partial extraction therefore looks
# "already done" forever, and re-running ch1_010 reports
# "[skip] natalidad_2018 already extracted" while the microdata is absent.
# (Observed: natalidad_2017..2023 held only catalogos/ files.)
#
# So repair them here, keyed on the actual MICRODATA FILE rather than the
# directory. Only conjunto_de_datos/ is extracted — the catalogos/,
# diccionario_de_datos/ and metadatos/ members are ~180 files the pipeline
# never reads.
births_zips <- list.files(file.path(raw_dir, "births"), pattern = "^natalidad_[0-9]{4}[.]zip$",
                          full.names = TRUE)
if (length(births_zips) > 0) {
  message(sprintf("ch1_011: checking %d births .zip archive(s)...", length(births_zips)))
  for (z in births_zips) {
    yr   <- sub("^natalidad_([0-9]{4})[.]zip$", "\\1", basename(z))
    dest <- file.path(raw_dir, "births", paste0("natalidad_", yr))
    ok <- tryCatch({
      entries <- utils::unzip(z, list = TRUE)$Name
      # The microdata is the sole csv under conjunto_de_datos/; naming drifts
      # between years (natalidad_<YYYY> up to 2022, enr<YYYY> from 2023).
      want <- grep("conjunto_de_datos/.*[.]csv$", entries, ignore.case = TRUE, value = TRUE)
      if (length(want) == 0) stop("no conjunto_de_datos/*.csv member found")
      have <- file.path(dest, want)
      if (all(file.exists(have)) && all(file.info(have)$size > 1e6)) {
        message("  [skip] ", basename(z), " microdata already extracted")
        TRUE
      } else {
        utils::unzip(z, files = want, exdir = dest)
        if (!all(file.exists(file.path(dest, want)))) stop("extraction produced no file")
        message(sprintf("  [ok  ] %s -> %s", basename(z), paste(basename(want), collapse = ", ")))
        TRUE
      }
    }, error = function(e) {
      message("  [FAIL] ", basename(z), " : ", conditionMessage(e)); FALSE
    })
  }
}
