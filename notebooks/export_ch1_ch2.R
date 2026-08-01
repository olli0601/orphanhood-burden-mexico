# =============================================================================
# export_ch1_ch2.R  ·  Data export for notebooks/ch1_ch2_review.ipynb
#
# Read-only with respect to the pipeline: writes ONLY into
# input-data-processed/py-export/ (gitignored). Nothing here re-implements a
# pipeline computation — it reads the artefacts the pipeline produced and
# reshapes them to parquet so the notebook can read them without R.
#
# Run: pixi run Rscript notebooks/export_ch1_ch2.R
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(sf)
  source("R/municipality_grouping.R")
})

proc <- "input-data-processed"
out  <- file.path(proc, "py-export")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
# Clear stale exports from the retired notebooks so nothing half-current lingers.
invisible(file.remove(list.files(out, pattern = "[.]parquet$|[.]gpkg$", full.names = TRUE)))

put <- function(df, name) {
  write_parquet(as.data.frame(df), file.path(out, paste0(name, ".parquet")))
  message(sprintf("  %-28s %7d rows", name, nrow(df)))
}
message("export_ch1_ch2: writing to ", out)

# --- Chapter 1: per-year panels and their manifest ---------------------------
man <- read.csv(file.path(proc, ".manifest", "ch1_012.csv"), stringsAsFactors = FALSE) |>
  mutate(prefix = sub("_[0-9]{4}$", "", id),
         year   = as.integer(sub("^.*_", "", id))) |>
  select(prefix, year, rows, total)
put(man, "ch1_panels")

# --- Chapter 1: events by occurrence and registration year -------------------
ev <- bind_rows(
  readRDS(file.path(proc, "births.RDS")) |>
    group_by(year, year_reg) |> summarise(events = sum(births), .groups = "drop") |>
    mutate(source = "births"),
  readRDS(file.path(proc, "deaths.RDS")) |>
    group_by(year, year_reg) |> summarise(events = sum(deaths), .groups = "drop") |>
    mutate(source = "deaths")
) |> mutate(delay = year_reg - year)
put(ev, "ch1_events_by_year")

# --- Chapter 2: municipalities and the units they were merged into -----------
mex <- st_drop_geometry(readRDS(file.path(proc, "mex_muni.RDS"))) |>
  select(mun, mun_name, state_name, total_pop)
grp <- st_drop_geometry(readRDS(file.path(proc, "grouped_municipality_50000.RDS"))) |>
  select(mun, group_id)
muni <- left_join(mex, grp, by = "mun")
put(muni, "ch2_municipalities")

units <- st_drop_geometry(readRDS(file.path(proc, "aggregated_muni_50000.RDS"))) |>
  select(group_id, total_pop) |>
  left_join(count(muni, group_id, name = "n_muns"), by = "group_id") |>
  mutate(kind = if_else(n_muns > 1, "merged", "singleton"))
put(units, "ch2_units")

# --- Chapter 2: the adjacency evidence ---------------------------------------
# Recomputed here because it is the core claim of the grouping fix and should be
# reproducible from the notebook rather than quoted from prose.
g  <- st_make_valid(st_geometry(readRDS(file.path(proc, "mex_muni.RDS"))))
gp <- st_transform(g, 6372)
stat <- function(nb, label) {
  nb <- lapply(seq_along(nb), function(i) setdiff(nb[[i]], i))
  d <- lengths(nb)
  data.frame(method = label, edges = sum(d) / 2, mean_neighbours = mean(d),
             polygons_with_none = sum(d == 0))
}
adj <- bind_rows(
  stat(st_touches(g), "st_touches (previous)"),
  stat(st_intersects(st_buffer(gp, 10)), "st_intersects + 10 m snap, EPSG:6372 (current)")
)
put(adj, "ch2_adjacency")

# --- Chapter 2: the threshold sweep ------------------------------------------
sweep_csv <- "output/ch2/ch2_020_grouping_diagnostics.csv"
if (file.exists(sweep_csv)) {
  put(read.csv(sweep_csv, stringsAsFactors = FALSE), "ch2_threshold_sweep")
} else {
  message("  [skip] ch2_threshold_sweep - run ch2_020_grouping_diagnostics.R first")
}

# --- Chapter 1: unknown parental age (INEGI code 99) -------------------------
# The headline number in docs/memo_unknown_parental_age.md. Computed from the raw
# CSVs, so it is slow and only covers the years shipped as CSV (2018+); skipped
# silently when those are absent.
uk_path <- file.path(out, "ch1_unknown_parent_age.parquet")
rows <- list()
for (y in 2018:2024) {
  f <- list.files(file.path("input-data-raw", "births", sprintf("natalidad_%d", y),
                            "conjunto_de_datos"), pattern = "[.]csv$", full.names = TRUE)
  if (!length(f)) next
  d <- suppressWarnings(readr::read_csv(f[1], show_col_types = FALSE, progress = FALSE))
  # Column case drifts between years: 2019 ships EDAD_MADN, the others edad_madn.
  # Matching case-sensitively silently skipped 2019 and left a gap in the series.
  names(d) <- tolower(names(d))
  if (!"edad_madn" %in% names(d)) next
  m <- suppressWarnings(as.numeric(d$edad_madn)); p <- suppressWarnings(as.numeric(d$edad_padn))
  rows[[length(rows) + 1]] <- data.frame(
    year_reg = y, records = length(m),
    unknown_mother = sum(m == 99, na.rm = TRUE),
    unknown_father = sum(p == 99, na.rm = TRUE),
    max_real_mother_age = suppressWarnings(max(m[m < 99], na.rm = TRUE)))
  message(sprintf("  [scan] natalidad_%d", y))
}
if (length(rows)) {
  put(bind_rows(rows) |>
        mutate(pct_unknown_mother = 100 * unknown_mother / records,
               pct_unknown_father = 100 * unknown_father / records),
      "ch1_unknown_parent_age")
} else {
  message("  [skip] ch1_unknown_parent_age - raw births CSVs not present")
}

message("export_ch1_ch2: done.")
