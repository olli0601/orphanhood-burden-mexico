# Moving municipality-level tables onto the grouped analytical units built in
# ch2_010 (group_id). The grouping file is an sf object; geometry is dropped
# here so results stay non-spatial.
# =============================================================================

GROUPING_FILE <- "input-data-processed/grouped_municipality_50000.RDS"

# mun -> group_id lookup (plus any extra `cols`), without geometry.
read_group_lookup <- function(path = GROUPING_FILE, cols = c("mun", "group_id")) {
  readRDS(path) |> sf::st_drop_geometry() |> dplyr::select(dplyr::all_of(cols))
}

# Attach group_id (and any other `cols` of the grouping) to a mun-level table.
# `grouping` is a path or an already-loaded grouping table (sf or plain).
# Municipality codes absent from the grouping (21 created by INEGI after GADM
# v4.1; 0.05% of deaths, 0.03% of births) would otherwise collapse into one
# spurious group_id = NA unit downstream, so they are dropped by default.
# Their count is reported either way.
attach_group_id <- function(df, cols = c("mun", "group_id"), grouping = GROUPING_FILE,
                            drop_unmatched = TRUE, what = "attach_group_id") {
  gm <- if (is.character(grouping)) readRDS(grouping) else grouping
  gm <- sf::st_drop_geometry(gm) |> dplyr::select(dplyr::all_of(cols))
  out <- dplyr::left_join(df, gm, by = "mun")
  bad <- is.na(out$group_id)
  if (any(bad)) {
    message(sprintf("%s: %d of %d rows (%d municipality codes) have no grouped unit%s.",
                    what, sum(bad), nrow(out), dplyr::n_distinct(out$mun[bad]),
                    if (drop_unmatched) " - dropped" else " - KEPT as group_id = NA"))
    if (drop_unmatched) out <- out[!bad, , drop = FALSE]
  }
  out
}

# Attach group_id and sum `value_col` per group_id x `by` (x year_reg when
# keep_year_reg = TRUE), into `out_col`.
aggregate_to_grouped_mun <- function(df, value_col, out_col = value_col,
                                     by = c("year", "sex", "age"), keep_year_reg = TRUE,
                                     grouping = GROUPING_FILE, drop_unmatched = TRUE) {
  keys <- c("group_id", by, if (keep_year_reg) "year_reg")
  attach_group_id(df, grouping = grouping, drop_unmatched = drop_unmatched) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise("{out_col}" := sum(.data[[value_col]]), .groups = "drop")
}
