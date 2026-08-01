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
#         input-data-raw/.manifest/*.tsv  (what each archive produced)
# Run    : only if you need the raw files (e.g. to regenerate the per-year
#          panels from scratch). The default pipeline runs from the already-
#          processed datasets and does NOT require this step.
#
# Re-running is cheap. Each archive records the members it produced and their
# sizes; a later run verifies that payload and skips the archive if it is
# intact. Before this, the loop below had NO skip condition and used `zstd -f`,
# so every invocation decompressed ~600 MB of archives and untarred ~12.9 GB
# over the existing tree - by far the largest avoidable I/O in the project.
# Set ORPHAN_FORCE_EXTRACT=1 to re-extract regardless.
#
# Windows note: the archives are standard .tar.zst, so they also open directly
# in 7-Zip (>= 21.01) without R. This script just makes it reproducible via pixi.
# =============================================================================

source("R/manifest.R")

zstd_bin <- Sys.which("zstd")
if (!nzchar(zstd_bin)) {
  stop("`zstd` not on PATH. Run inside the pixi environment (pixi run Rscript ...) ",
       "or install zstd / 7-Zip and extract manually.")
}

raw_dir  <- "input-data-raw"
force    <- env_flag("ORPHAN_FORCE_EXTRACT")
archives <- list.files(raw_dir, pattern = "[.]tar[.]zst$", recursive = TRUE,
                       full.names = TRUE)

if (length(archives) == 0) {
  message("ch1_011: no .tar.zst archives found under ", raw_dir, "/ — nothing to do.")
} else {
  message(sprintf("ch1_011: %d archive(s)%s", length(archives),
                  if (force) " (ORPHAN_FORCE_EXTRACT set — ignoring manifests)" else ""))

  # Members shipped by more than one archive are last-writer-wins and so cannot
  # verify any single archive; exclude them from the skip decision.
  shared <- manifest_shared_paths(raw_dir)
  if (length(shared)) {
    message(sprintf("ch1_011: %d member path(s) are shipped by more than one archive ",
                    length(shared)),
            "(catalogue/dictionary files); excluded from verification. ",
            "Their on-disk content depends on extraction order — no pipeline step reads them.")
  }

  n_skip <- 0L; n_ext <- 0L; n_fail <- 0L; bytes_avoided <- 0
  for (a in archives) {
    key  <- sub("^input-data-raw/", "", a)   # manifest key, relative to raw_dir
    dest <- dirname(a)                       # extract beside the archive

    chk <- if (force) list(ok = FALSE, reason = "forced", n = 0L)
           else manifest_check(key, raw_dir, ignore = shared)
    if (chk$ok) {
      n_skip <- n_skip + 1L
      bytes_avoided <- bytes_avoided + file.info(a)$size
      message(sprintf("  [skip] %s (%d members verified)", basename(a), chk$n))
      next
    }
    if (!force && chk$reason != "no manifest") {
      message(sprintf("  [redo] %s : %s", basename(a), chk$reason))
    }

    tarf <- sub("[.]zst$", "", a)            # foo.tar.zst -> foo.tar
    ok <- tryCatch({
      # zstd -d -f -k : decompress, overwrite, keep the .zst archive
      st <- system2(zstd_bin, c("-d", "-f", "-k", shQuote(a)))
      if (st != 0) stop("zstd exit ", st)
      members <- untar(tarf, list = TRUE)
      untar(tarf, exdir = dest)
      file.remove(tarf)                      # drop the intermediate .tar

      # Record what landed on disk, relative to raw_dir, so the next run can
      # verify it. manifest_write() refuses to certify an empty extraction.
      rel <- file.path(sub("^input-data-raw/?", "", dest), members)
      rel <- sub("^/+", "", rel)
      manifest_write(key, rel, raw_dir)
      TRUE
    }, error = function(e) {
      message("  [FAIL] ", basename(a), " : ", conditionMessage(e)); FALSE
    })
    if (ok) { n_ext <- n_ext + 1L; message("  [ok  ] ", basename(a)) } else n_fail <- n_fail + 1L
  }
  message(sprintf("ch1_011: %d extracted, %d skipped (%s of archive reads avoided), %d failed.",
                  n_ext, n_skip, fmt_bytes(bytes_avoided), n_fail))
  if (n_fail > 0) {
    stop("ch1_011: ", n_fail, " archive(s) failed to extract - see [FAIL] lines above. ",
         "Downstream steps would read an incomplete raw tree.", call. = FALSE)
  }
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
  n_zip_fail <- 0L
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
    # `ok` used to be assigned and discarded, so a failed .zip repair exited 0.
    if (!isTRUE(ok)) n_zip_fail <- n_zip_fail + 1L
  }
  if (n_zip_fail > 0) {
    stop("ch1_011: ", n_zip_fail, " births .zip archive(s) failed - see [FAIL] lines above. ",
         "ch1_012 would silently build fewer years.", call. = FALSE)
  }
}
