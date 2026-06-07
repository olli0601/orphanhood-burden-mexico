# Municipality aggregation/grouping helpers
# Helper functions split from the former R/utils.R
# =============================================================================

group_small_municipalities <- function(mex_muni, threshold = 30000) {
  library(sf)
  library(dplyr)
  
  muni_ref <- mex_muni %>%
    mutate(
      total_pop = ifelse(is.na(total_pop), 0, total_pop),
      small = total_pop < threshold
    ) %>%
    st_make_valid()
  
  # Get neighbors using spatial touches
  neighbors <- sf::st_touches(muni_ref)
  
  # Initialize group ID with municipality code
  muni_ref$group_id <- muni_ref$mun
  
  # Loop over small municipalities
  small_indices <- which(muni_ref$small == TRUE)
  
  for (i in small_indices) {
    neighbors_i <- neighbors[[i]]
    
    if (length(neighbors_i) == 0) next
    
    pops <- muni_ref$total_pop[neighbors_i]
    
    if (all(is.na(pops))) next
    
    chosen <- neighbors_i[which.max(pops)]
    
    muni_ref$group_id[i] <- muni_ref$group_id[chosen]
  }
  
  # Aggregate geometries and population by group_id
  aggregated_muni <- muni_ref %>%
    group_by(group_id) %>%
    summarise(
      geometry = st_union(geometry),
      total_pop = sum(total_pop, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(aggregated_muni)
}


#-------------------------------------------------------------------------------
# Optimised reimplementation. Same merge rule as before — repeatedly fold each
# below-threshold unit into its lowest-population adjacent neighbour — but the
# expensive sf geometry work is done ONCE: build the polygon adjacency a single
# time, resolve all merges on a union-find graph using only population sums
# (pure-R, fast), then union geometries one final time per resulting group.
# Avoids the per-iteration st_union and per-candidate st_buffer/st_distance that
# made the original O(iterations x candidates) in geometry ops.
group_until_threshold <- function(mex_muni, threshold = 30000, max_iter = 50) {
  library(sf)
  library(dplyr)

  muni <- st_make_valid(mex_muni)
  muni$total_pop <- ifelse(is.na(muni$total_pop), 0, muni$total_pop)
  n <- nrow(muni)

  nb   <- sf::st_touches(muni)                  # adjacency: computed ONCE
  base_pop <- muni$total_pop
  mun_lab  <- as.character(mex_muni$mun)        # group label = root unit's mun
  cent <- NULL                                  # lazy centroids (islands only)

  # --- union-find ---
  parent <- seq_len(n)
  find <- function(i) { r <- i; while (parent[r] != r) r <- parent[r]
                        while (parent[i] != r) { nx <- parent[i]; parent[i] <<- r; i <- nx }; r }
  unite <- function(a, b) { ra <- find(a); rb <- find(b); if (ra != rb) parent[ra] <<- rb }

  for (iter in seq_len(max_iter)) {
    roots <- vapply(seq_len(n), find, integer(1))
    gpop  <- tapply(base_pop, roots, sum)
    small <- as.integer(names(gpop)[gpop < threshold])
    if (length(small) == 0) { message("All groups meet the threshold (iter ", iter - 1L, ").") ; break }
    small <- small[order(gpop[as.character(small)])]   # smallest first
    changed <- FALSE
    for (r in small) {
      if (find(r) != r) next                            # already merged this round
      members <- which(roots == r)
      nbr <- unique(vapply(unlist(nb[members]), find, integer(1)))
      nbr <- setdiff(nbr, r)
      if (length(nbr) == 0) {                            # island: nearest centroid
        if (is.null(cent)) cent <- suppressWarnings(st_centroid(st_geometry(muni)))
        d <- as.numeric(st_distance(cent[members[1]], cent)); d[members] <- Inf
        unite(r, find(which.min(d))); changed <- TRUE; next
      }
      tgt <- nbr[which.min(gpop[as.character(nbr)])]     # lowest-pop neighbour
      unite(r, tgt); changed <- TRUE
    }
    if (!changed) break
  }

  roots <- vapply(seq_len(n), find, integer(1))
  muni$group_id <- mun_lab[roots]

  aggregated_final <- muni |>
    group_by(group_id) |>
    summarise(geometry  = suppressWarnings(st_union(geometry)),
              total_pop = sum(total_pop, na.rm = TRUE), .groups = "drop") |>
    mutate(geometry = st_make_valid(geometry))

  list(aggregated = aggregated_final, muni_ref_with_groups = muni)
}

#-------------------------------------------------------------------------------
