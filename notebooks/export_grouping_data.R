# =============================================================================
# notebooks/export_grouping_data.R
# Dump the Chapter-2 municipality-aggregation objects into open formats so the
# Python notebook (notebooks/ch2_grouping_vs_thesis.ipynb) can read them without
# an R bridge. Read-only w.r.t. the pipeline: writes ONLY into
# input-data-processed/py-export/ and never touches pipeline inputs.
#
# Run from the repo root:  pixi run Rscript notebooks/export_grouping_data.R
# =============================================================================

suppressMessages({
  library(sf)
  library(dplyr)
  library(arrow)
})
source("R/load_population.R")

out_dir <- "input-data-processed/py-export"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

say <- function(...) cat(..., "\n")

# --- 1. mex_muni: the exact input to the grouping algorithm --------------------
# geometry + the total_pop that ch2_010 actually fed to group_until_threshold().
mex_muni <- readRDS("input-data-processed/mex_muni.RDS")
st_write(mex_muni, file.path(out_dir, "mex_muni.gpkg"),
         layer = "mex_muni", delete_dsn = TRUE, quiet = TRUE)
write_parquet(st_drop_geometry(mex_muni), file.path(out_dir, "mex_muni.parquet"))
say("mex_muni:", nrow(mex_muni), "polygons")

# --- 1b. Polygon adjacency, exactly as the grouping helper computes it --------
# group_until_threshold() calls st_touches() on the st_make_valid()'d polygons.
# Export that adjacency as a 0-based edge list so Python reproduces the same
# neighbour structure instead of re-deriving it with a different predicate.
mm_valid <- st_make_valid(mex_muni)
nb <- sf::st_touches(mm_valid)
edges <- data.frame(
  i = rep(seq_along(nb) - 1L, lengths(nb)),
  j = unlist(nb) - 1L
)
write_parquet(edges, file.path(out_dir, "adjacency_touches.parquet"))
say("adjacency (st_touches):", nrow(edges), "directed edges;",
    sum(lengths(nb) == 0), "polygons with no touching neighbour")

# Second adjacency: the "infinitesimal buffer to capture near touches" step the
# thesis describes (Section 2.3) but group_until_threshold() does not implement.
nb_buf <- sf::st_intersects(st_buffer(st_geometry(mm_valid), 1e-5))
nb_buf <- lapply(seq_along(nb_buf), function(i) setdiff(nb_buf[[i]], i))
edges_buf <- data.frame(
  i = rep(seq_along(nb_buf) - 1L, lengths(nb_buf)),
  j = unlist(nb_buf) - 1L
)
write_parquet(edges_buf, file.path(out_dir, "adjacency_buffer.parquet"))
say("adjacency (1e-5 buffer):", nrow(edges_buf), "directed edges;",
    sum(lengths(nb_buf) == 0), "polygons with no neighbour")

# --- 2. Grouping results at both thresholds ----------------------------------
for (thr in c("50000", "30000")) {
  agg <- readRDS(sprintf("input-data-processed/aggregated_muni_%s.RDS", thr))
  grp <- readRDS(sprintf("input-data-processed/grouped_municipality_%s.RDS", thr))
  st_write(agg, file.path(out_dir, sprintf("aggregated_muni_%s.gpkg", thr)),
           layer = "aggregated", delete_dsn = TRUE, quiet = TRUE)
  write_parquet(st_drop_geometry(agg),
                file.path(out_dir, sprintf("aggregated_muni_%s.parquet", thr)))
  write_parquet(st_drop_geometry(grp),
                file.path(out_dir, sprintf("grouped_municipality_%s.parquet", thr)))
  say(sprintf("threshold %s: %d units from %d municipalities", thr, nrow(agg), nrow(grp)))
}

# --- 3. Population, as it stands on disk NOW ----------------------------------
# NOTE: population.RDS is rewritten by ch1_015 -> ch2_040 -> ch2_060 with
# different content, so this is not necessarily the table ch2_010 consumed.
# That is precisely what the notebook checks.
population <- readRDS("input-data-processed/population.RDS")
write_parquet(population, file.path(out_dir, "population.parquet"))
say("population:", nrow(population), "rows,",
    length(unique(population$mun)), "municipalities,",
    paste(range(population$year), collapse = "-"))

# Total population, all age bands — the denominator ch2_010 evaluates the threshold
# against. Exported separately so the notebook can verify the grouping input without
# reaching for the CONAPO workbook itself.
population_full <- load_population(keep_child = TRUE, keep_old = TRUE)
write_parquet(population_full, file.path(out_dir, "population_full.parquet"))
say("population_full:", nrow(population_full), "rows,",
    length(levels(droplevels(population_full$age))), "age bands")

geo_info <- readRDS("input-data-processed/geo_info.RDS")
write_parquet(geo_info, file.path(out_dir, "geo_info.parquet"))
say("geo_info:", nrow(geo_info), "rows")

say("Wrote exports to", normalizePath(out_dir))
