# manifest.R
# Payload manifests for the Chapter 1 acquisition steps.
#
# The problem this exists to solve: ch1_010 and ch1_012 decided "already done"
# from the existence of a directory or a file. That has already produced a false
# "done" in this project - natalidad_2017..2023 reported "[skip] already
# extracted" while holding only catalogos/ files - and it is exactly the failure
# mode CLAUDE.md warns about ("be suspicious of checks that don't inspect what
# they claim to verify"). ch1_011's .tar.zst loop had the opposite problem: no
# check at all, so it re-extracted ~13 GB on every run.
#
# A manifest records what a step actually produced - every member path and its
# byte size - so a later run can verify the payload instead of guessing from a
# directory listing. Manifests are cheap to write, cheap to check, and make a
# truncated or partially-deleted extraction detectable.
#
# Manifests live in <root>/.manifest/ and are plain TSV so they can be read with
# any tool. They are derived data: deleting one forces the work to be redone,
# which is the safe direction to fail.
# =============================================================================

# Sanitise an arbitrary key (usually a path) into a single filename.
.manifest_key <- function(key) {
  k <- gsub("[^A-Za-z0-9._-]+", "_", key)
  substr(k, max(1, nchar(k) - 180), nchar(k))   # keep the tail; it is the distinctive part
}

manifest_path <- function(key, root) {
  file.path(root, ".manifest", paste0(.manifest_key(key), ".tsv"))
}

# Record the files a step produced. `files` are paths relative to `root`.
manifest_write <- function(key, files, root) {
  files <- files[!grepl("/$", files)]                     # drop directory entries
  abs   <- file.path(root, files)
  keep  <- file.exists(abs)
  if (!any(keep)) {
    stop("manifest_write(", key, "): none of the ", length(files),
         " recorded members exist on disk - refusing to write a manifest that ",
         "would certify nothing.")
  }
  mp <- manifest_path(key, root)
  dir.create(dirname(mp), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    data.frame(path = files[keep], size = file.info(abs[keep])$size),
    mp, sep = "\t", row.names = FALSE, quote = FALSE)
  invisible(mp)
}

# Paths claimed by more than one manifest. Several INEGI deaths archives ship
# the same catalogue and dictionary files (catalogos/*.csv, ENTMUN.dbf,
# Thumbs.db), so whichever archive extracts last wins and the others' recorded
# sizes no longer match. Those members cannot certify anything about a given
# archive and must be excluded from its check. 91 of 455 member paths collide
# here, 60 of them with differing sizes - but no microdata file is among them,
# which is why the pipeline is unaffected (ch1_012 resolves microdata by
# explicit path precisely to avoid these catalogue decoys).
manifest_shared_paths <- function(root) {
  mans <- list.files(file.path(root, ".manifest"), pattern = "[.]tsv$", full.names = TRUE)
  if (!length(mans)) return(character())
  paths <- unlist(lapply(mans, function(f) {
    m <- try(utils::read.table(f, sep = "\t", header = TRUE, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(m, "try-error") || !nrow(m)) character() else unique(m$path)
  }), use.names = FALSE)
  tab <- table(paths)
  names(tab)[tab > 1]
}

# Verify a manifest against what is on disk.
# Returns list(ok, reason, n, n_missing, n_resized). ok = FALSE whenever the
# manifest is absent, so a missing manifest means "redo the work", never "fine".
# `ignore` drops members from the check (see manifest_shared_paths).
manifest_check <- function(key, root, ignore = character()) {
  mp <- manifest_path(key, root)
  if (!file.exists(mp)) {
    return(list(ok = FALSE, reason = "no manifest", n = 0L, n_missing = 0L, n_resized = 0L))
  }
  m <- utils::read.table(mp, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                         colClasses = c("character", "numeric"))
  if (length(ignore)) m <- m[!m$path %in% ignore, , drop = FALSE]
  if (!nrow(m)) {
    # Everything this archive provides is shared with another archive, so there
    # is no exclusive payload to verify. Report rather than silently pass.
    return(list(ok = FALSE, reason = "no exclusive members to verify",
                n = 0L, n_missing = 0L, n_resized = 0L))
  }
  abs     <- file.path(root, m$path)
  present <- file.exists(abs)
  sizes   <- rep(NA_real_, length(abs))
  sizes[present] <- file.info(abs[present])$size
  resized <- present & !is.na(sizes) & sizes != m$size

  n_missing <- sum(!present)
  n_resized <- sum(resized)
  ok <- n_missing == 0L && n_resized == 0L
  reason <- if (ok) "verified" else
    paste0(if (n_missing) sprintf("%d missing", n_missing) else NULL,
           if (n_missing && n_resized) ", " else NULL,
           if (n_resized) sprintf("%d wrong size", n_resized) else NULL)
  list(ok = ok, reason = reason, n = nrow(m),
       n_missing = n_missing, n_resized = n_resized)
}

# Tabular manifest (one row per built artefact), used by ch1_012 for the per-year
# panels where the meaningful payload is a row count and an event total, not a
# file size. Keyed on the `id` column.
table_manifest_read <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(id = character(), rows = numeric(), total = numeric(),
                      stringsAsFactors = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

table_manifest_upsert <- function(path, id, rows, total) {
  m <- table_manifest_read(path)
  m <- m[m$id != id, , drop = FALSE]
  m <- rbind(m, data.frame(id = id, rows = rows, total = total, stringsAsFactors = FALSE))
  m <- m[order(m$id), , drop = FALSE]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(m, path, row.names = FALSE)
  invisible(path)
}

table_manifest_get <- function(path, id) {
  m <- table_manifest_read(path)
  r <- m[m$id == id, , drop = FALSE]
  if (!nrow(r)) NULL else as.list(r[1, ])
}

# Env-var flag, so a forced rebuild does not require editing a script.
env_flag <- function(name) {
  v <- tolower(Sys.getenv(name, ""))
  nzchar(v) && !v %in% c("0", "false", "no")
}

# Human-readable byte size for the "avoided N GB" reporting.
fmt_bytes <- function(b) {
  if (is.na(b) || b <= 0) return("0 B")
  u <- c("B", "KB", "MB", "GB", "TB")
  i <- min(length(u), floor(log(b, 1024)) + 1)
  sprintf("%.1f %s", b / 1024^(i - 1), u[i])
}
