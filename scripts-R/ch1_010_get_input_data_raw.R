# =============================================================================
# ch1_010_get_input_data_raw.R  ·  Chapter 1 — Data sources
# Download all raw input data into input-data-raw/. Auto-pulls the sources with
# stable URLs (CONAPO population & marginalization, GADM geometries) and unzips
# them; INEGI births/deaths must be added manually (no scriptable URL).
#
# Reads : remote URLs (CONAPO, GADM)
# Writes: input-data-raw/{births,deaths,population,marginalization,geometries,census}/
# Run    : first — everything else depends on these raw files.
# Sources (README.md, "Data"):
#   INEGI  registered births and deaths   INEGI  census locality tables (ITER)
#   CONAPO municipal marginalization      CONAPO municipal population
#   GADM   municipal geometries v4.1
# INEGI births 2017-2024 download automatically; earlier births and all deaths
# are request-gated and must be placed by hand (checked below).
# =============================================================================

suppressMessages(library(tools))
source("R/manifest.R")
source("R/inputs.R")

# --- configuration ---------------------------------------------------------

raw_dir <- "input-data-raw"

dirs <- c(
  births       = file.path(raw_dir, "births"),
  deaths       = file.path(raw_dir, "deaths"),
  population   = file.path(raw_dir, "population"),
  marginalization = file.path(raw_dir, "marginalization"),
  geometries   = file.path(raw_dir, "geometries"),
  census       = file.path(raw_dir, "census")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Bump the timeout: some of these files are tens of MB on slow mirrors.
options(timeout = max(1200, getOption("timeout")))

# --- TLS: complete the CONAPO certificate chain ------------------------------
# conapo.segob.gob.mx serves only its leaf certificate, omitting the
# intermediate "Go Daddy Secure Certificate Authority - G2". The pixi CA bundle
# is root-only, so download.file() fails with "SSL peer certificate ... not OK".
# Append the published intermediate to a session-local copy of the bundle;
# verification stays fully enabled. Built in tempdir() because the intermediate
# expires (2031-05-03).
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
    # Never append a non-certificate (e.g. an error page) to the trust bundle.
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
# Download `url` to `dest`, retrying, and flag files under `min_bytes` (INEGI
# answers dead links with a ~2 KB HTML page). Failures are reported, not raised;
# the required-files check at the end raises.
#   unzip  : extract into `exdir` (default: dirname(dest)) and delete the .zip
#   marker : manifest key; an unzipped download is skipped when its manifest
#            verifies (R/manifest.R)
#   only   : regex of members to extract. INEGI zips ship ~180 catalogue files,
#            one with a non-UTF8 name that makes utils::unzip() abort.
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
  # Retry: CONAPO intermittently redirects a valid path to a 404.
  for (attempt in 1:3) {
    ok <- tryCatch(
      {
        utils::download.file(url, dest, mode = "wb", quiet = TRUE)
        TRUE
      },
      error = function(e) {
        message(sprintf("  [%s] %s : %s", if (attempt < 3) "retry" else "FAIL",
                        basename(dest), conditionMessage(e)))
        FALSE
      },
      warning = function(w) {
        message(sprintf("  [%s] %s : %s", if (attempt < 3) "retry" else "FAIL",
                        basename(dest), conditionMessage(w)))
        FALSE
      }
    )
    if (ok) break
    Sys.sleep(2)
  }
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
# Births 2017-2024 have stable open-data URLs ("natalidad" to 2022, "enr" from
# 2023). Deaths have none confirmed; add per-year URLs to `inegi_urls$deaths`.
# Pre-2017 births and all deaths come from J.M. Aburto's Dropbox transfers
# (browser-only, time-limited) or the INSP RIISP mirror:
#   fertility : https://www.dropbox.com/t/IKzEQFCeuFFFKi5h
#   mortality : https://www.dropbox.com/t/ZIbYNxir2hkWTx29
#   https://riisp.insp.mx/nada/index.php/catalog/study/MEX-INSP-EVNAC-1985-2024
.inegi_nat <- "https://www.inegi.org.mx/contenidos/programas/natalidad/datosabiertos"
inegi_urls <- list(
  births = stats::setNames(
    file.path(.inegi_nat, 2017:2024,
              c(sprintf("conjunto_de_datos_natalidad_%d_csv.zip", 2017:2022),
                sprintf("conjunto_de_datos_enr%d_csv.zip", 2023:2024))),
    2017:2024),
  deaths = character())
inegi_prefix <- c(births = "natalidad", deaths = "defunciones")

for (src in names(inegi_urls)) {
  message("== INEGI ", src, " ==")
  urls <- inegi_urls[[src]]
  if (!length(urls)) message("  [skip] no URLs set in `inegi_urls$", src, "`")
  for (yr in names(urls)) {
    out <- file.path(dirs[src], sprintf("%s_%s", inegi_prefix[[src]], yr))
    fetch(urls[[yr]], paste0(out, ".zip"), unzip = TRUE, exdir = out, marker = out,
          only = "conjunto_de_datos/.*[.]csv$")
  }
}

# --- manual INEGI files ------------------------------------------------------
# Exact filenames used in this analysis, checked so a missing year is reported.
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
# Present as the original .zip or as the committed .tar.zst (see ch1_011).
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
# Municipal workbooks; 2010 and 2015 are the DP2 (penalised-distance) re-runs.
# Listed on the gob.mx 2020 release:
# https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372
message("== CONAPO marginalization (IMM) ==")
.imm <- "https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/Municipio"
fetch(file.path(.imm, "IMM_2020.xls"),      file.path(dirs["marginalization"], "IMM_2020.xls"),      min_bytes = 5000)
fetch(file.path(.imm, "IMM_DP2_2015.xlsx"), file.path(dirs["marginalization"], "IMM_DP2_2015.xlsx"), min_bytes = 5000)
fetch(file.path(.imm, "IMM_DP2_2010.xlsx"), file.path(dirs["marginalization"], "IMM_DP2_2010.xlsx"), min_bytes = 5000)

# 1990-2015 stacked series (PCA-era): indicator percentages per municipality-
# year, for ch2_055's fixed-weight index. Listed on:
# https://www.gob.mx/conapo/documentos/indices-de-marginacion-1990-2010
fetch(file.path(.imm, "Base_Indice_de_marginacion_municipal_90-15.csv"),
      file.path(dirs["marginalization"], "Base_Indice_de_marginacion_municipal_90-15.csv"),
      min_bytes = 100000)

# ===========================================================================
# 4. CONAPO - municipal population (BD municipales, "pry23" reconstruction)
# ===========================================================================
# Population by municipality, sex and age, 1950-2070 (national file, all
# states). URLs from the gob.mx "interactive PDF", also fetched for reference.
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
# 6. INEGI - census locality tables (ITER), 2020 and 2010
# ===========================================================================
# ch2_055 rebuilds "15+ without complete primary" for 2020 from ITER (CONAPO
# 2020 publishes a different education variable); 2010 validates the rebuild.
message("== INEGI census ITER ==")
.iter <- "https://www.inegi.org.mx/contenidos/programas/ccpv"
fetch(file.path(.iter, "2020/datosabiertos/iter/iter_00_cpv2020_csv.zip"),
      file.path(dirs["census"], "iter_00_cpv2020_csv.zip"),
      unzip = TRUE, only = "conjunto_de_datos/.*[.]csv$",
      marker = file.path(dirs["census"], "iter_00_cpv2020"))
fetch(file.path(.iter, "2010/datosabiertos/iter_nal_2010_csv.zip"),
      file.path(dirs["census"], "iter_nal_2010_csv.zip"),
      unzip = TRUE, only = "conjunto_de_datos/.*[.]csv$",
      marker = file.path(dirs["census"], "iter_00_cpv2010"))

# ===========================================================================
# 7. Fail if a required auto-downloaded file is missing or a stub
# ===========================================================================
message("== required auto-downloaded files ==")
size <- stats::setNames(file.size(REQUIRED_RAW_FILES), names(REQUIRED_RAW_FILES))
ok <- !is.na(size) & size > MIN_RAW_BYTES
for (lbl in names(REQUIRED_RAW_FILES)) {
  if (ok[[lbl]]) message(sprintf("  [ok  ] %s (%.1f MB)", lbl, size[[lbl]] / 1024^2))
  else message(sprintf("  [MISS] %s -> %s", lbl, REQUIRED_RAW_FILES[[lbl]]))
}
message("\nDone. Files written under '", raw_dir, "/'.")
if (!all(ok)) {
  stop(sprintf("%d required auto-downloaded file(s) missing: %s.\nCheck the [FAIL]/[WARN] lines above and re-run.",
               sum(!ok), paste(names(REQUIRED_RAW_FILES)[!ok], collapse = "; ")), call. = FALSE)
}
