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

# --- helper ----------------------------------------------------------------

# Download `url` to `dest`. Skips if already present, warns (does not abort) on
# failure, and flags suspiciously small files (INEGI returns a ~2 KB HTML stub
# for URLs that do not resolve, so a tiny "success" usually means a dead link).
#
# If `unzip = TRUE`, the downloaded archive is extracted into `exdir` (default:
# the directory holding `dest`) and the .zip is deleted afterwards. Pass
# `marker` (a file or directory that exists only once extraction succeeded) so
# re-runs skip the whole download+unzip step instead of re-fetching the zip.
fetch <- function(url, dest, min_bytes = 10000, unzip = FALSE,
                  exdir = NULL, marker = NULL) {
  if (!is.null(marker) && file.exists(marker)) {
    message(sprintf("  [skip] %s already extracted", basename(marker)))
    return(invisible(TRUE))
  }
  if (!unzip && file.exists(dest) && file.info(dest)$size > min_bytes) {
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
    done <- tryCatch(
      {
        utils::unzip(dest, exdir = exdir)
        TRUE
      },
      error = function(e) {
        message(sprintf("  [FAIL] unzip %s : %s", basename(dest), conditionMessage(e)))
        FALSE
      }
    )
    if (done) {
      file.remove(dest)
      message(sprintf("  [unzip] -> %s/ (removed %s)", basename(exdir), basename(dest)))
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

inegi_births_urls <- c(
  # 2023 = "https://www.inegi.org.mx/.../conjunto_de_datos_natalidad_2023_csv.zip",
  # 2022 = "...",
)

inegi_deaths_urls <- c(
  # 2023 = "https://www.inegi.org.mx/.../conjunto_de_datos_defunciones_2023_csv.zip",
  # 2022 = "...",
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
    fetch(u, paste0(out, ".zip"), unzip = TRUE, exdir = out, marker = out)
  }
}

message("== INEGI deaths ==")
if (length(inegi_deaths_urls) == 0) {
  message("  [skip] no URLs set in `inegi_deaths_urls` - see comments above.")
} else {
  for (yr in names(inegi_deaths_urls)) {
    u <- inegi_deaths_urls[[yr]]
    out <- file.path(dirs["deaths"], sprintf("defunciones_%s", yr))
    fetch(u, paste0(out, ".zip"), unzip = TRUE, exdir = out, marker = out)
  }
}

# ===========================================================================
# 3. CONAPO - Indice de Marginacion Municipal (IMM)
# ===========================================================================
# Direct datos-abiertos workbooks. Only the 2020 release is served under the
# stable CONAPO open-data path; 2010 & 2015 ship inside the 2020 historical
# bundle on the gob.mx documents page (see README), so fetch those manually if
# needed:  https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372
message("== CONAPO marginalization (IMM) ==")
fetch(
  "https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/Municipio/IMM_2020.xls",
  file.path(dirs["marginalization"], "IMM_2020.xls")
)

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
# Used to aggregate the 2,457 municipalities into the 519 analytical units.
message("== GADM geometries ==")
fetch(
  "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MEX_2.json.zip",
  file.path(dirs["geometries"], "gadm41_MEX_2.json.zip"),
  unzip = TRUE,
  marker = file.path(dirs["geometries"], "gadm41_MEX_2.json")
)

message("\nDone. Files written under '", raw_dir, "/'.")
message("If any INEGI section was skipped, set the URL vectors near the top and re-run.")
