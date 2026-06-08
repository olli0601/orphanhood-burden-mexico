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
attach_group_id <- function(df, cols = c("mun", "group_id"),
                            grouping = "input-data-processed/grouped_municipality_50000.RDS") {
  gm <- sf::st_drop_geometry(readRDS(grouping)) |> dplyr::select(dplyr::all_of(cols))
  dplyr::left_join(df, gm, by = "mun")
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
