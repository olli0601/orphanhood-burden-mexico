# grouped_mun.R
# Helpers for moving original-municipality tables onto the aggregated ("grouped")
# municipality units built in ch2_010. The grouping file
# (grouped_municipality_50000.RDS) is an sf object, so its geometry is dropped
# here to keep the result non-spatial (sf summarise() would otherwise misbehave).
# Replaces the join + group_by/summarise blocks duplicated across
# ch2_040 / ch3_010 / ch3_020 / ch3_050.
# =============================================================================

# Attach the grouped-municipality id (group_id) to a mun-level table by joining
# the ch2 grouping on `mun`. `cols` is what to carry over from the grouping
# (defaults to just mun + group_id; pass more, e.g. state_name/mun_name).
# `grouping` is normally a path to the ch2_010 output, but an already-loaded
# grouping table (sf or plain) is accepted too, so callers that hold several
# groupings in memory - ch2_020's threshold sweep - need not round-trip each one
# through disk.
#
# `drop_unmatched`: municipality codes absent from the grouping otherwise carry
# group_id = NA into every downstream group_by, where they collapse into one
# spurious extra unit indistinguishable from a real one in tables and figures.
# INEGI has created 21 municipalities since GADM v4.1 and they appear in the
# event panels, accounting for 0.05% of deaths and 0.03% of births. Dropping is
# the default because keeping them silently is never what a caller wants; pass
# FALSE to keep them, and the count is reported either way.
attach_group_id <- function(df, cols = c("mun", "group_id"),
                            grouping = "input-data-processed/grouped_municipality_50000.RDS",
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

# Attach group_id and sum `value_col` to grouped-municipality level. Grouping
# keys are group_id + `by` (+ year_reg when keep_year_reg = TRUE). The summed
# column is named `out_col` (defaults to value_col).
aggregate_to_grouped_mun <- function(df, value_col, out_col = value_col,
                                     by = c("year", "sex", "age"),
                                     keep_year_reg = TRUE,
                                     grouping = "input-data-processed/grouped_municipality_50000.RDS") {
  keys <- c("group_id", by, if (keep_year_reg) "year_reg")
  attach_group_id(df, grouping = grouping) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise("{out_col}" := sum(.data[[value_col]]), .groups = "drop")
}
