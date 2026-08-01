# =============================================================================
# notebooks/export_grouping_variants.R
# Re-run the Chapter-2 aggregation under candidate population definitions and
# record every statistic the thesis reports (Section 2.5): number of units,
# median population before and after merging, and the minimum unit population.
#
# Purpose: find out whether ANY plausible reading of "the 2020 population counts"
# reproduces the thesis's 519 units / median 23,800 -> 66,300 / minimum 50,013.
# The merging itself is the pipeline's own group_until_threshold() — unchanged,
# so the only thing varying is the population fed to it.
#
# Run from the repo root:  pixi run Rscript notebooks/export_grouping_variants.R
# Runtime: ~1 min per variant (st_touches dominates).
# =============================================================================

suppressMessages({
  library(sf); library(dplyr); library(arrow); library(readxl); library(stringr)
})
source("R/municipality_grouping.R")

out_dir <- "input-data-processed/py-export"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

mex_muni <- readRDS("input-data-processed/mex_muni.RDS") |> select(-total_pop)
wb <- read_excel("input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx") |>
  mutate(mun = str_pad(CLAVE, 5, pad = "0"))

BANDS_CHILD <- c("POB_00_04", "POB_05_09")
BANDS_OLD   <- c("POB_80_84", "POB_85_mm")
BANDS_ALL   <- grep("^POB_", names(wb), value = TRUE) |> setdiff("POB_TOTAL")

# Candidate readings of "the population". The last two are not sensible choices —
# they reproduce the classic pivot_longer(starts_with("POB_")) mistake, where the
# POB_TOTAL column is treated as another age band and every municipality is
# counted twice. Included because the thesis's pre-merge median is ~1.7x ours and
# double counting is the obvious way a population gets inflated.
variants <- list(
  "all ages (POB_TOTAL)"                = list(cols = BANDS_ALL,                              total = FALSE),
  "excl. ages 0-9"                      = list(cols = setdiff(BANDS_ALL, BANDS_CHILD),        total = FALSE),
  "excl. 80+"                           = list(cols = setdiff(BANDS_ALL, BANDS_OLD),          total = FALSE),
  "excl. 0-9 and 80+ (the old ch2_010)" = list(cols = setdiff(BANDS_ALL, c(BANDS_CHILD, BANDS_OLD)), total = FALSE),
  "all ages + TOTAL counted twice"      = list(cols = BANDS_ALL,                              total = TRUE),
  "excl. 0-9 and 80+, + TOTAL twice"    = list(cols = setdiff(BANDS_ALL, c(BANDS_CHILD, BANDS_OLD)), total = TRUE),
  # The population definition in the archived original loader
  # (old-R/ch1_020_population_data_preprocessing.R): ages 0-14 and 85+ dropped,
  # then females 65+ dropped as well, leaving males 15-84 + females 15-64.
  "archived old-R (M 15-84, F 15-64)"   = list(cols = setdiff(BANDS_ALL, c(BANDS_CHILD, "POB_10_14", "POB_85_mm")),
                                               total = FALSE, drop_female_65 = TRUE)
)

rows <- list()
for (yr in c(2020, 2023)) {
  wy <- filter(wb, `AÑO` == yr)
  for (vn in names(variants)) {
    v <- variants[[vn]]
    cols <- if (v$total) c(v$cols, "POB_TOTAL") else v$cols
    wv <- wy
    if (isTRUE(v$drop_female_65)) {
      # females contribute only the bands below 65
      f_cols <- setdiff(cols, c("POB_65_69", "POB_70_74", "POB_75_79", "POB_80_84"))
      wv <- wv |> mutate(across(all_of(setdiff(cols, f_cols)),
                                ~ ifelse(SEXO == "MUJERES", 0, .x)))
    }
    pop <- wv |>
      mutate(p = rowSums(across(all_of(cols)), na.rm = TRUE)) |>
      group_by(mun) |> summarise(total_pop = sum(p), .groups = "drop")

    mm <- left_join(mex_muni, pop, by = "mun")
    res <- group_until_threshold(mm, threshold = 50000, max_iter = 50)
    agg <- st_drop_geometry(res$aggregated)
    grp <- st_drop_geometry(res$muni_ref_with_groups)

    rows[[length(rows) + 1]] <- data.frame(
      year               = yr,
      population         = vn,
      national_total     = sum(mm$total_pop, na.rm = TRUE),
      median_before      = median(mm$total_pop, na.rm = TRUE),
      units              = nrow(agg),
      median_after       = median(agg$total_pop),
      min_unit           = min(agg$total_pop),
      max_muni_per_unit  = max(table(grp$group_id)),
      n_above_66300      = sum(mm$total_pop > 66300, na.rm = TRUE)
    )
    cat(sprintf("%d | %-36s -> %4d units, median %8.0f -> %8.0f, min %6.0f\n",
                yr, vn, nrow(agg), median(mm$total_pop, na.rm = TRUE),
                median(agg$total_pop), min(agg$total_pop)))
  }
}

variants_df <- bind_rows(rows)
write_parquet(variants_df, file.path(out_dir, "grouping_variants.parquet"))

# --- Threshold sweep ----------------------------------------------------------
# Section 2.4 says thresholds between 10,000 and 100,000 were explored. Hold the
# population at the thesis's stated vintage (2020, all ages) and vary only the
# threshold, so the reported 519 units / median 66,300 / minimum 50,013 can be
# checked against the whole family of runs rather than one point.
pop2020 <- wb |>
  filter(`AÑO` == 2020) |>
  group_by(mun) |> summarise(total_pop = sum(POB_TOTAL), .groups = "drop")
mm2020 <- left_join(mex_muni, pop2020, by = "mun")

sweep <- bind_rows(lapply(c(10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000, 100000), function(th) {
  res <- group_until_threshold(mm2020, threshold = th, max_iter = 50)
  agg <- st_drop_geometry(res$aggregated)
  cat(sprintf("threshold %6d -> %4d units, median %8.0f, min %7.0f\n",
              th, nrow(agg), median(agg$total_pop), min(agg$total_pop)))
  data.frame(threshold = th, units = nrow(agg),
             median_after = median(agg$total_pop), min_unit = min(agg$total_pop))
}))
write_parquet(sweep, file.path(out_dir, "grouping_threshold_sweep.parquet"))

cat("\nThesis (Section 2.5): 519 units, median 23,800 -> 66,300, minimum 50,013\n")
cat("Wrote grouping_variants.parquet + grouping_threshold_sweep.parquet\n")
