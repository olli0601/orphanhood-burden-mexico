# =============================================================================
# ch1_010_get_input_data_raw.R  ·  Chapter 1 — Data sources
# Download all raw input data into input-data-raw/. Auto-pulls the sources with
# stable URLs (CONAPO population & marginalization, GADM geometries) and unzips
# them; INEGI births/deaths must be added manually (no scriptable URL).
#
# Reads : remote URLs (CONAPO, GADM)
# Writes: input-data-raw/{births,deaths,population,marginalization,geometries}/
# Run    : first — everything else depends on these raw files.
# =============================================================================

# get_input_data_raw.R
# ---------------------------------------------------------------------------
# Download the raw input data for the Mexico orphanhood-burden analysis into
# `input-data-raw/`. Sources are described in README.md (# Data section):
#
#   1. INEGI  - Estadisticas de Nacimientos Registrados   (fertility / births)
#   2. INEGI  - Estadisticas de Defunciones Registradas   (mortality / deaths)
#   3. CONAPO - Indice de Marginacion Municipal (IMM)      (marginalization)
#   4. gob.mx - municipal population statistics            (population)
#   5. GADM   - municipal geometries v4.1                  (spatial units)
#
# Run from the repository root:   Rscript R/get_input_data_raw.R
#
# NOTE on INEGI: the INEGI open-data portal (https://en.www.inegi.org.mx/
# datosabiertos/) serves the per-year birth/death files through a JavaScript
# download widget, so there is no stable, guessable direct file URL. You must
# copy the per-year ZIP links from the portal into the `inegi_births_urls` /
# `inegi_deaths_urls` vectors below (or use the Dropbox / INSP mirrors noted
# there). Everything else downloads automatically.
# ---------------------------------------------------------------------------

suppressMessages(library(tools))
source("R/manifest.R")

# --- configuration ---------------------------------------------------------

raw_dir <- "input-data-raw"

dirs <- c(
  births       = file.path(raw_dir, "births"),
  deaths       = file.path(raw_dir, "deaths"),
  population   = file.path(raw_dir, "population"),
  marginalization = file.path(raw_dir, "marginalization"),
  geometries   = file.path(raw_dir, "geometries")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Bump the timeout: some of these files are tens of MB on slow mirrors.
options(timeout = max(1200, getOption("timeout")))

# --- TLS: complete the CONAPO certificate chain ------------------------------
# conapo.segob.gob.mx (population + marginalization) serves ONLY its leaf
# certificate (CN=*.segob.gob.mx) and omits the intermediate
# "Go Daddy Secure Certificate Authority - G2". macOS' system curl hides this
# (Keychain has the intermediate and does AIA fetching), but the conda/pixi CA
# bundle is root-only, so R's download.file() aborts with
#   "SSL peer certificate or SSH remote key was not OK"
# and every CONAPO file silently ends up as [FAIL] while the script exits 0.
#
# Fix: append the published intermediate to a session-local copy of the CA
# bundle and point libcurl at it. Certificate verification stays FULLY enabled —
# we are supplying a missing link in the chain, not disabling any check. The
# intermediate is fetched from certs.godaddy.com, which has a complete chain of
# its own (so this download works before the fix is in place).
#
# Built in tempdir() rather than committed: it is a derived artefact, and the
# intermediate expires 2031-05-03, which would rot silently in version control.
.ensure_conapo_ca <- function() {
  base <- Sys.getenv("CURL_CA_BUNDLE")
  if (!nzchar(base) || !file.exists(base)) {
    cand <- c(file.path(Sys.getenv("CONDA_PREFIX"), "ssl", "cacert.pem"),
              file.path(R.home(), "..", "..", "ssl", "cacert.pem"),
              ".pixi/envs/default/ssl/cacert.pem")
    cand <- cand[nzchar(cand) & file.exists(cand)]
    base <- if (length(cand)) cand[[1]] else ""
  }
  if (!nzchar(base)) {
    message("  [WARN] no CA bundle found - leaving TLS config untouched; ",
            "CONAPO downloads may fail.")
    return(invisible(FALSE))
  }
  out <- file.path(tempdir(), "cacert-conapo.pem")
  if (file.exists(out) && identical(Sys.getenv("CURL_CA_BUNDLE"), out)) {
    return(invisible(TRUE))  # already bootstrapped this session
  }
  ok <- tryCatch({
    itm <- file.path(tempdir(), "gdig2.crt.pem")
    download.file("https://certs.godaddy.com/repository/gdig2.crt.pem",
                  itm, mode = "wb", quiet = TRUE)
    # A login/error page instead of a certificate would be far larger, and must
    # not be concatenated into the trust bundle.
    txt <- readLines(itm, warn = FALSE)
    if (!any(grepl("^-----BEGIN CERTIFICATE-----", txt))) {
      stop("fetched intermediate is not PEM (got ", length(txt), " lines)")
    }
    writeLines(c(readLines(base, warn = FALSE), txt), out)
    TRUE
  }, error = function(e) {
    message("  [WARN] CA bootstrap failed (", conditionMessage(e), ") - ",
            "CONAPO downloads may fail.")
    FALSE
  })
  if (ok) {
    Sys.setenv(CURL_CA_BUNDLE = out)
    message("  [ca  ] CONAPO chain completed via ", basename(out))
  }
  invisible(ok)
}

message("== TLS setup (CONAPO chain) ==")
.ensure_conapo_ca()

# --- helper ----------------------------------------------------------------

# Download `url` to `dest`. Skips if already present, warns (does not abort) on
# failure, and flags suspiciously small files (INEGI returns a ~2 KB HTML stub
# for URLs that do not resolve, so a tiny "success" usually means a dead link).
#
# If `unzip = TRUE`, the downloaded archive is extracted into `exdir` (default:
# the directory holding `dest`) and the .zip is deleted afterwards.
#
# Re-run skipping is decided from a PAYLOAD MANIFEST (R/manifest.R), not from
# `marker`. `marker` was usually a directory, and a directory left behind by a
# half-finished unzip satisfied file.exists() forever: that is how
# natalidad_2017..2023 came to report "[skip] already extracted" while holding
# only catalogos/ files, which ch1_011 then had to repair (see its comment at
# the births .zip loop). `marker` is retained only as the manifest key.
# `only` (a regex) extracts just the matching members. The INEGI vital-stats
# zips ship ~180 catalogue files per year that no pipeline step reads, and one
# of them is named `catalogos/año_nacimiento.csv`: utils::unzip() aborts on that
# non-UTF8 filename with "Illegal byte sequence", leaving a partial extraction.
# The old directory-existence skip then declared the year done forever. Passing
# `only` sidesteps the bad member entirely - the same approach ch1_011's repair
# loop already takes.
fetch <- function(url, dest, min_bytes = 10000, unzip = FALSE,
                  exdir = NULL, marker = NULL, only = NULL) {
  force_dl <- env_flag("ORPHAN_FORCE_DOWNLOAD")
  if (unzip && !is.null(marker) && !force_dl) {
    chk <- manifest_check(marker, raw_dir)
    if (chk$ok) {
      message(sprintf("  [skip] %s (%d members verified)", basename(marker), chk$n))
      return(invisible(TRUE))
    }
    if (chk$reason != "no manifest") {
      message(sprintf("  [redo] %s : %s", basename(marker), chk$reason))
    }
  }
  if (!unzip && file.exists(dest) && file.info(dest)$size > min_bytes && !force_dl) {
    message(sprintf("  [skip] %s already present", basename(dest)))
    return(invisible(TRUE))
  }
  message(sprintf("  [get ] %s", basename(dest)))
  ok <- tryCatch(
    {
      utils::download.file(url, dest, mode = "wb", quiet = TRUE)
      TRUE
    },
    error = function(e) {
      message(sprintf("  [FAIL] %s : %s", basename(dest), conditionMessage(e)))
      FALSE
    }
  )
  if (ok && file.exists(dest) && file.info(dest)$size < min_bytes) {
    message(sprintf(
      "  [WARN] %s is only %d bytes - likely a dead link / HTML stub, not data.",
      basename(dest), file.info(dest)$size
    ))
    return(invisible(FALSE))  # do not try to unzip an HTML stub
  }
  if (ok && unzip) {
    if (is.null(exdir)) exdir <- dirname(dest)
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    members <- tryCatch(
      {
        want <- NULL
        if (!is.null(only)) {
          entries <- utils::unzip(dest, list = TRUE)$Name
          want <- grep(only, entries, ignore.case = TRUE, value = TRUE)
          if (!length(want)) stop("no member matched /", only, "/")
        }
        if (is.null(want)) utils::unzip(dest, exdir = exdir)
        else               utils::unzip(dest, files = want, exdir = exdir)
      },
      error = function(e) {
        message(sprintf("  [FAIL] unzip %s : %s", basename(dest), conditionMessage(e)))
        character()
      }
    )
    if (length(members)) {
      # Record what actually landed, so the next run verifies the payload
      # instead of trusting that a directory exists.
      if (!is.null(marker)) {
        rel <- sub(paste0("^", normalizePath(raw_dir, mustWork = FALSE), "/?"), "",
                   normalizePath(members, mustWork = FALSE))
        rel <- sub(paste0("^", raw_dir, "/?"), "", rel)
        tryCatch(manifest_write(marker, rel, raw_dir),
                 error = function(e) message("  [WARN] manifest: ", conditionMessage(e)))
      }
      file.remove(dest)
      message(sprintf("  [unzip] -> %s/ (%d members, removed %s)",
                      basename(exdir), length(members), basename(dest)))
    }
  }
  invisible(ok)
}

# ===========================================================================
# 1-2. INEGI births & deaths
# ===========================================================================
# Paste the per-year ZIP/CSV URLs copied from the INEGI open-data portal here.
# Portal path: Datos Abiertos -> Administrative Records-Statistics -> Vital ->
# "Registered Births Statistics" / "Registered Deaths Statistics".
# Public portal covers 2017-2023; pre-2017 (from 1990) were obtained courtesy
# of Jose Manuel Aburto (Dropbox mirrors below).
#
# Leave a vector empty to skip that source.

# INEGI registered-births open data — stable direct URLs (verified 2026-06).
# Only 2017-2024 are published at the open-data path; 1985-2016 are request-gated
# (NADA microdata form) and must be added manually. Naming: 2017-2022 use
# "natalidad", 2023-2024 use "enr". CSV through preprocess_fertility() reproduces
# the fert_YYYY tables exactly (validated for 2020).
.inegi_nat <- "https://www.inegi.org.mx/contenidos/programas/natalidad/datosabiertos"
inegi_births_urls <- c(
  "2017" = file.path(.inegi_nat, "2017/conjunto_de_datos_natalidad_2017_csv.zip"),
  "2018" = file.path(.inegi_nat, "2018/conjunto_de_datos_natalidad_2018_csv.zip"),
  "2019" = file.path(.inegi_nat, "2019/conjunto_de_datos_natalidad_2019_csv.zip"),
  "2020" = file.path(.inegi_nat, "2020/conjunto_de_datos_natalidad_2020_csv.zip"),
  "2021" = file.path(.inegi_nat, "2021/conjunto_de_datos_natalidad_2021_csv.zip"),
  "2022" = file.path(.inegi_nat, "2022/conjunto_de_datos_natalidad_2022_csv.zip"),
  "2023" = file.path(.inegi_nat, "2023/conjunto_de_datos_enr2023_csv.zip"),
  "2024" = file.path(.inegi_nat, "2024/conjunto_de_datos_enr2024_csv.zip")
)

# INEGI registered-deaths open data — the mortalidad open-data file-naming was
# not confirmed by direct probe; add per-year URLs from the portal as needed.
inegi_deaths_urls <- c(
)

# Dropbox mirrors shared by Jose Manuel Aburto (pre-processed by registration
# year). NOTE: these are Dropbox *Transfer* links - browser-only (the download
# is CSRF/cookie-gated, returns 403 to scripts) and time-limited, so they
# CANNOT be auto-pulled here. Open in a browser and drop the files into
# input-data-raw/births and input-data-raw/deaths manually:
#   fertility : https://www.dropbox.com/t/IKzEQFCeuFFFKi5h
#   mortality : https://www.dropbox.com/t/ZIbYNxir2hkWTx29
# Consolidated INEGI vital-stats mirror (births 1985-2024), INSP RIISP:
#   https://riisp.insp.mx/nada/index.php/catalog/study/MEX-INSP-EVNAC-1985-2024

message("== INEGI births ==")
if (length(inegi_births_urls) == 0) {
  message("  [skip] no URLs set in `inegi_births_urls` - see comments above.")
} else {
  for (yr in names(inegi_births_urls)) {
    u <- inegi_births_urls[[yr]]
    out <- file.path(dirs["births"], sprintf("natalidad_%s", yr))
    fetch(u, paste0(out, ".zip"), unzip = TRUE, exdir = out, marker = out,
          only = "conjunto_de_datos/.*[.]csv$")
  }
}

message("== INEGI deaths ==")
if (length(inegi_deaths_urls) == 0) {
  message("  [skip] no URLs set in `inegi_deaths_urls` - see comments above.")
} else {
  for (yr in names(inegi_deaths_urls)) {
    u <- inegi_deaths_urls[[yr]]
    out <- file.path(dirs["deaths"], sprintf("defunciones_%s", yr))
    fetch(u, paste0(out, ".zip"), unzip = TRUE, exdir = out, marker = out,
          only = "conjunto_de_datos/.*[.]csv$")
  }
}

# --- manual INEGI files (cannot be auto-downloaded) ------------------------
# Births 1985-2016 and ALL deaths years are request-gated (JS widget / NADA form
# / shared mirrors) and must be dropped into input-data-raw/{births,deaths}/ by
# hand. The lists below are the exact filenames used in this analysis (courtesy
# J.M. Aburto + INEGI open data). They are checked for presence on each run so a
# missing file is reported instead of silently producing an incomplete panel.
births_manual <- c(
  "Natalidad 1985 Database.zip", "Natalidad Database 1986.zip",
  "Natalidad 1987 Database.zip", "Natalidad Database 1988.zip",
  "Natalidad 1989 Database.zip", "Natalidad Database 1990.zip",
  "Natalidad 1991 Database.zip", "Natalidad 1992 Database.zip",
  "Natalidad Database 1993.zip", "natalidad_base_datos_1994_dbf.zip",
  "Birth Data 1995.zip", "Natalidad 1996 Database.zip",
  "Natalidad 1997 Database.zip", "Natalidad 1998 Database.zip",
  "Natalidad 1999 Database.zip", "Natalidad Base Datos 2000.zip",
  "Natalidad Base Datos 2001.zip", "natalidad_base_datos_2002_dbf.zip",
  "natalidad_base_datos_2003_dbf.zip", "Natalidad Base Datos 2004.zip",
  "Birth Data 2005.zip", "Natalidad 2006 Database.zip",
  "natalidad_base_datos_2007_dbf.zip", "Natalidad 2008 Database.zip",
  "Natalidad 2009 Database.zip", "natalidad_base_datos_2010_dbf.zip",
  "Natalidad Base Datos 2011.zip", "Natalidad Base Datos 2012.zip",
  "natalidad_base_datos_2013_dbf.zip", "Natalidad Base Datos 2014.zip",
  "Natalidad 2015 Database.zip", "Natalidad Base Datos 2016.zip"
)
deaths_manual <- c(
  "General Deaths Database 1990-1994.zip",
  "defunciones_generales_base_datos_1995_1999_dbf.zip",
  "defunciones_generales_base_datos_2000_2004_dbf.zip",
  "defunciones_generales_base_datos_2005_2009_dbf.zip",
  "General Deaths Database 2010-2014.zip",
  "defunciones_base_datos_2012_csv.zip", "defunciones_base_datos_2013_csv.zip",
  "defunciones_base_datos_2014_csv.zip", "Defunciones Base Datos 2015.zip",
  "Defunciones Base Datos 2016.zip", "General Deaths Dataset 2017.zip",
  "conjunto_de_datos_defunciones_registradas_2018_csv.zip",
  "Registered Deaths 2019 Data.zip",
  "conjunto_de_datos_defunciones_registradas_2020_csv.zip",
  "conjunto_de_datos_defunciones_registradas_2021_csv.zip",
  "conjunto_de_datos_defunciones_registradas_2022_csv.zip",
  "Registered Deaths Data 2023.zip",
  "conjunto_de_datos_defunciones_registradas_2024_csv.zip"
)

message("== manual files check (births 1985-2016 + all deaths) ==")
# A manual file counts as present whether it is the original .zip or the
# committed .tar.zst archive (see scripts-R/ch1_011_extract_raw_archives.R).
.check_manual <- function(files, subdir) {
  present <- function(f) {
    cand <- c(f, paste0(sub("[.]zip$", "", f), ".tar.zst"))
    any(file.exists(file.path(dirs[subdir], cand)))
  }
  miss <- files[!vapply(files, present, logical(1))]
  if (length(miss) == 0) {
    message(sprintf("  [ok  ] all %d %s files present", length(files), subdir))
  } else {
    message(sprintf("  [MISS] %d/%d %s files absent - add to %s/ :",
                    length(miss), length(files), subdir, dirs[subdir]))
    for (m in miss) message("         - ", m)
  }
}
.check_manual(births_manual, "births")
.check_manual(deaths_manual, "deaths")

# ===========================================================================
# 3. CONAPO - Indice de Marginacion Municipal (IMM)
# ===========================================================================
# Direct datos-abiertos municipal workbooks (verified 2026-06). 2010 & 2015 use
# the DP2 (penalised-distance) method and the .xlsx extension; 2020 is .xls.
# All under the same CONAPO open-data path; listed on the gob.mx 2020 release:
# https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372
message("== CONAPO marginalization (IMM) ==")
.imm <- "https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/Municipio"
fetch(file.path(.imm, "IMM_2020.xls"),      file.path(dirs["marginalization"], "IMM_2020.xls"),      min_bytes = 5000)
fetch(file.path(.imm, "IMM_DP2_2015.xlsx"), file.path(dirs["marginalization"], "IMM_DP2_2015.xlsx"), min_bytes = 5000)
fetch(file.path(.imm, "IMM_DP2_2010.xlsx"), file.path(dirs["marginalization"], "IMM_DP2_2010.xlsx"), min_bytes = 5000)

# ===========================================================================
# 4. CONAPO - municipal population (BD municipales, "pry23" reconstruction)
# ===========================================================================
# CONAPO "Reconstruccion y proyecciones de la poblacion de los municipios de
# Mexico": population by municipality, sex, and (single / five-year) age, 1950-
# 2070. `00_Republica_mexicana.zip` is the national file (all 32 states); the
# per-state files live under the same DBMun/ path if you only need a subset.
# These direct URLs are the ones embedded in the gob.mx "interactive PDF"
# (BD_municipales_portada_regiones_FINAL.pdf), also fetched below for reference.
message("== CONAPO municipal population ==")
fetch(
  "https://conapo.segob.gob.mx/work/models/CONAPO/pry23/DBMun/00_Republica_mexicana.zip",
  file.path(dirs["population"], "00_Republica_mexicana.zip"),
  unzip = TRUE,
  marker = file.path(dirs["population"], "00_Republica_mexicana")
)
fetch(
  "https://www.gob.mx/cms/uploads/attachment/file/918028/BD_municipales_portada_regiones_FINAL.pdf",
  file.path(dirs["population"], "BD_municipales_portada_regiones_FINAL.pdf"),
  min_bytes = 5000
)

# ===========================================================================
# 5. GADM - municipal geometries (v4.1, ADM level 2)
# ===========================================================================
# Used to aggregate the 2,457 municipalities into the analytical units.
message("== GADM geometries ==")
fetch(
  "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MEX_2.json.zip",
  file.path(dirs["geometries"], "gadm41_MEX_2.json.zip"),
  unzip = TRUE,
  marker = file.path(dirs["geometries"], "gadm41_MEX_2.json")
)

# ===========================================================================
# 6. Verify the auto-downloaded files later chapters actually require
# ===========================================================================
# fetch() reports failures with message() and returns FALSE rather than
# aborting, so a dead link or TLS problem would otherwise leave this script
# exiting 0 with missing data - only surfacing much later as a confusing error
# inside ch1_015 / ch2_010. Fail loudly here instead.
message("== required auto-downloaded files ==")
required <- c(
  "population (CONAPO quinquennial)" =
    file.path(dirs["population"], "00_Republica_mexicana", "1_Grupo_Quinq_00_RM.xlsx"),
  "marginalization IMM 2020" = file.path(dirs["marginalization"], "IMM_2020.xls"),
  "marginalization IMM 2015" = file.path(dirs["marginalization"], "IMM_DP2_2015.xlsx"),
  "marginalization IMM 2010" = file.path(dirs["marginalization"], "IMM_DP2_2010.xlsx"),
  "GADM geometries"          = file.path(dirs["geometries"], "gadm41_MEX_2.json")
)
missing_required <- character(0)
for (lbl in names(required)) {
  f <- required[[lbl]]
  # Size guard as well as existence: a dead link or captive portal yields a
  # small HTML stub, which would pass a bare file.exists() check.
  if (file.exists(f) && file.info(f)$size > 10000) {
    message(sprintf("  [ok  ] %s (%.1f MB)", lbl, file.info(f)$size / 1024^2))
  } else {
    message(sprintf("  [MISS] %s -> %s", lbl, f))
    missing_required <- c(missing_required, lbl)
  }
}

message("\nDone. Files written under '", raw_dir, "/'.")
message("If any INEGI section was skipped, set the URL vectors near the top and re-run.")

if (length(missing_required) > 0) {
  stop(sprintf(
    "%d required auto-downloaded file(s) missing: %s.\nThe pipeline cannot start without these (ch1_015 needs the population workbook and IMM_2020.xls). Check the [FAIL]/[WARN] lines above and re-run.",
    length(missing_required), paste(missing_required, collapse = "; ")
  ), call. = FALSE)
}
