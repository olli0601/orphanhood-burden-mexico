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
