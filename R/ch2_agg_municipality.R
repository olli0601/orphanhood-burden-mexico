#' Aggregate neighbouring polygons until a population threshold is met
#'
#' @param sf_obj    An `sf` multipolygon object.
#' @param id_col    Column name that uniquely identifies each unit.
#' @param pop_col   Column giving population counts.
#' @param threshold Minimum population for a group.
#' @param max_iter  Safeguard against infinite loops.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{aggregated}{`sf` object of merged polygons.}
#'   \item{unit_map}{Original `sf` with a `group_id` column.}
#' }
#' @details
#' The function repeatedly unions small units with the *least-populated*
#' adjacent neighbour (touching or nearest) until every group meets
#' `threshold`.
#'
#' @examples
#' \dontrun{
#' mx   <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#' agg  <- group_until_threshold(mx, id_col = "NAME", pop_col = "BIR79",
#'                               threshold = 30000)
#' plot(agg$aggregated["total_pop"])
#' }
#' @export
group_until_threshold <- function(
    sf_obj,
    id_col,
    pop_col,
    threshold = 30000,
    max_iter  = 50
) {
  # ---- preparations ----
  stopifnot(id_col %in% names(sf_obj), pop_col %in% names(sf_obj))
  library(sf)
  library(dplyr)
  library(units)

  sf_obj <- sf_obj |>
    mutate(
      total_pop = if_else(is.na(.data[[pop_col]]), 0, .data[[pop_col]]),
      group_id  = as.character(.data[[id_col]])
    ) |>
    st_make_valid()

  # ---- iterative merging (unchanged core logic) ----
  for (iter in seq_len(max_iter)) {
    sf_obj <- sf_obj |>
      filter(
        st_is_valid(geometry),
        st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"),
        st_area(geometry) > set_units(0, "m^2")
      )

    aggregated <- sf_obj |>
      group_by(group_id) |>
      summarise(
        geometry   = suppressWarnings(st_union(geometry)),
        total_pop  = sum(total_pop, na.rm = TRUE),
        .groups    = "drop"
      ) |>
      mutate(geometry = st_make_valid(geometry)) |>
      filter(
        st_is_valid(geometry),
        st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"),
        st_area(geometry) > set_units(0, "m^2")
      )

    candidates <- aggregated |> filter(total_pop < threshold)
    if (nrow(candidates) == 0) break

    for (i in seq_len(nrow(candidates))) {
      cand_id   <- candidates$group_id[i]
      cand_idx  <- which(aggregated$group_id == cand_id)
      cand_geom <- aggregated[cand_idx, ]

      neigh_idx <- sf::st_touches(
        sf::st_buffer(cand_geom, 1e-8),
        aggregated
      )[[1]]
      neigh_idx <- neigh_idx[neigh_idx != cand_idx]

      if (length(neigh_idx) == 0) {                   # fallback: nearest
        d <- sf::st_distance(cand_geom, aggregated)
        d[cand_idx] <- Inf
        neigh_idx <- which.min(d)
      }

      target_idx <- neigh_idx[which.min(aggregated$total_pop[neigh_idx])]
      target_id  <- aggregated$group_id[target_idx]

      sf_obj <- sf_obj |>
        mutate(group_id = if_else(group_id == cand_id, target_id, group_id))
    }
  }

  aggregated_final <- sf_obj |>
    group_by(group_id) |>
    summarise(
      geometry   = suppressWarnings(st_union(geometry)),
      total_pop  = sum(total_pop, na.rm = TRUE),
      .groups    = "drop"
    ) |>
    mutate(geometry = st_make_valid(geometry)) |>
    filter(
      st_is_valid(geometry),
      st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"),
      st_area(geometry) > set_units(0, "m^2")
    )

  list(aggregated = aggregated_final, unit_map = sf_obj)
}
