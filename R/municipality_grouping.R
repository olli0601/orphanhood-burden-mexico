# Municipality aggregation/grouping helpers
# Helper functions split from the former R/utils.R
# =============================================================================

#-------------------------------------------------------------------------------
# Polygon contiguity graph.
#
# Why NOT st_touches(): GADM ADM2 polygons do not share exact boundaries — they
# overlap by numerical slivers. Every boundary-based construction therefore
# collapses to the same broken graph on this data:
#
#   st_touches(muni)                          4,086 edges  mean 1.66  655 orphans
#   st_relate(pattern = "F***T****") (queen)  4,086 edges  mean 1.66  655 orphans
#   st_touches with sf_use_s2(FALSE)          4,086 edges  mean 1.66  655 orphans
#   spdep::poly2nb()                          same family, same failure
#   st_intersects(st_buffer(., 10 m))        14,346 edges  mean 5.84    0 orphans
#
# A planar contiguity graph averages ~5-6 neighbours (Euler bounds it below 6),
# so 1.66 is not a quirk — it is a broken graph, and it silently sent 655 of
# 2,457 municipalities down the nearest-centroid fallback in the merge loop.
#
# `snap` is in METRES and the geometry is projected to `crs` first. A buffer in
# degrees is not a meaningful quantity here: 1e-7 and 1e-3 degrees produce the
# identical edge count, so its value cannot be reasoned about or reviewed.
build_contiguity <- function(muni, crs = 6372, snap = 10,
                             min_mean_nb = 3, max_zero_nb = 25, verbose = TRUE) {
  g  <- sf::st_geometry(sf::st_make_valid(muni))
  gp <- sf::st_transform(g, crs)

  nb <- sf::st_intersects(sf::st_buffer(gp, snap))
  nb <- lapply(seq_along(nb), function(i) setdiff(nb[[i]], i))   # drop self

  deg     <- lengths(nb)
  mean_nb <- mean(deg)
  n_zero  <- sum(deg == 0)

  # Connected components, iterative BFS (igraph is not a project dependency).
  n <- length(nb)
  comp <- integer(n)
  k <- 0L
  for (s in seq_len(n)) {
    if (comp[s] != 0L) next
    k <- k + 1L
    stack <- s
    comp[s] <- k
    while (length(stack)) {
      v <- stack[length(stack)]; stack <- stack[-length(stack)]
      for (w in nb[[v]]) if (comp[w] == 0L) { comp[w] <- k; stack <- c(stack, w) }
    }
  }

  # Assert on the payload, not on the call having returned something. This is
  # exactly the guard that would have caught the st_touches defect.
  if (mean_nb < min_mean_nb) {
    stop(sprintf(paste0("build_contiguity: mean %.2f neighbours per polygon (expected ~5-6 for a ",
                        "planar contiguity graph). The adjacency graph is broken; any grouping ",
                        "built on it would be spatially meaningless."), mean_nb))
  }
  if (n_zero > max_zero_nb) {
    stop(sprintf(paste0("build_contiguity: %d of %d polygons have no neighbour (at most %d ",
                        "expected, for genuine islands). Adjacency is broken."),
                 n_zero, n, max_zero_nb))
  }

  if (verbose) {
    sizes <- sort(table(comp), decreasing = TRUE)
    message(sprintf("build_contiguity: %d polygons, %d edges, mean %.2f neighbours, %d with none.",
                    n, sum(deg) / 2, mean_nb, n_zero))
    message(sprintf("build_contiguity: %d connected component(s); largest %d polygons (snap = %g m, EPSG:%s).",
                    k, sizes[[1]], snap, crs))
  }

  list(nb = nb, mean_nb = mean_nb, n_zero = n_zero, n_components = k,
       comp = comp, deg = deg, crs = crs, snap = snap)
}

#-------------------------------------------------------------------------------
# Iteratively merge below-threshold municipalities into a neighbour until every
# analytical unit clears `threshold` inhabitants.
#
# Merge rule: take the globally smallest unit still below threshold and fold it
# into its LOWEST-POPULATION adjacent neighbour. The unit population is updated
# immediately, so a unit stops being a merge candidate the moment it crosses the
# threshold.
#
# That immediacy is the whole point. The previous implementation froze group
# populations at the start of each round and then merged every below-threshold
# root in that round, so units kept absorbing long after they had cleared the
# threshold: at a 50,000 threshold the median unit came out at 136,566, two
# thirds of merged units exceeded 100,000, and one unit swallowed 153
# municipalities. Refreshing the population after each merge removes that
# entirely, at no meaningful cost (each merge retires exactly one root, so the
# loop runs at most n-1 times and no sf work happens inside it).
#
# Alternative merge target considered and NOT adopted: fold into whichever
# neighbour lands closest to the threshold. It minimises overshoot, but it
# systematically welds small rural municipalities onto the nearest city, which
# destroys precisely the deprivation and rural/urban heterogeneity the analysis
# rests on. Changing this is a statistical decision for the researchers; see
# scripts-R/ch2_020_grouping_diagnostics.R for the numbers that would inform it.
#
# `within` (optional): a vector, one entry per row of `mex_muni`, constraining
# merges to stay inside a partition — e.g. substr(mun, 1, 2) to stop units
# crossing state lines. Default NULL = unconstrained. The number of units that
# do cross a state line is reported in $diagnostics either way.
group_until_threshold <- function(mex_muni, threshold = 30000, nb = NULL,
                                  snap = 10, crs = 6372, within = NULL,
                                  max_iter = NULL, verbose = TRUE) {

  # Order by `mun` so tie-breaking in which.min() does not depend on the row
  # order of the source GeoJSON. Without this, re-downloading GADM could
  # silently change every group_id.
  ord      <- order(as.character(mex_muni$mun))
  muni     <- sf::st_make_valid(mex_muni[ord, , drop = FALSE])
  within   <- if (is.null(within)) NULL else as.character(within)[ord]
  muni$total_pop <- ifelse(is.na(muni$total_pop), 0, muni$total_pop)
  n <- nrow(muni)
  if (is.null(max_iter) || max_iter < n) max_iter <- n

  adj <- if (is.null(nb)) build_contiguity(muni, crs = crs, snap = snap, verbose = verbose)
         else nb
  # A precomputed graph was built on the caller's row order; reindex to ours.
  nbs <- if (is.null(nb)) adj$nb else lapply(adj$nb[ord], function(v) match(v, ord))

  mun_lab <- as.character(muni$mun)

  # --- union-find with population maintained at the root ---
  parent   <- seq_len(n)
  pop_root <- as.numeric(muni$total_pop)
  active   <- rep(TRUE, n)
  find <- function(i) { while (parent[i] != i) i <- parent[i]; i }

  cent <- NULL
  n_merges <- 0L
  n_island <- 0L
  n_within_relaxed <- 0L

  repeat {
    small <- which(active & pop_root < threshold)
    if (!length(small)) break
    r <- small[which.min(pop_root[small])]          # globally smallest unit

    cand <- setdiff(unique(vapply(nbs[[r]], find, integer(1))), r)
    cand <- cand[active[cand]]

    if (!is.null(within) && length(cand)) {
      same <- cand[within[cand] == within[r]]
      if (length(same)) cand <- same else n_within_relaxed <- n_within_relaxed + 1L
    }

    if (!length(cand)) {                             # genuine island
      if (is.null(cent)) {
        cent <- suppressWarnings(sf::st_centroid(sf::st_transform(sf::st_geometry(muni), crs)))
      }
      d <- as.numeric(sf::st_distance(cent[r], cent))
      d[!active] <- Inf; d[r] <- Inf
      tgt <- which.min(d)
      n_island <- n_island + 1L
    } else {
      tgt <- cand[which.min(pop_root[cand])]         # lowest-population neighbour
    }

    parent[r]     <- tgt
    pop_root[tgt] <- pop_root[tgt] + pop_root[r]     # refreshed NOW, not next round
    nbs[[tgt]]    <- setdiff(unique(c(nbs[[tgt]], nbs[[r]])), c(r, tgt))
    active[r]     <- FALSE
    nbs[[r]]      <- integer(0)
    pop_root[r]   <- Inf                             # retired roots never win a which.min

    n_merges <- n_merges + 1L
    if (n_merges > max_iter) {
      stop("group_until_threshold: exceeded the merge cap; adjacency or population is inconsistent.")
    }
  }

  roots <- vapply(seq_len(n), find, integer(1))
  muni$group_id <- mun_lab[roots]

  aggregated_final <- muni |>
    dplyr::group_by(group_id) |>
    dplyr::summarise(geometry  = suppressWarnings(sf::st_union(geometry)),
                     total_pop = sum(total_pop, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(geometry = sf::st_make_valid(geometry))

  # The assertion the function exists to guarantee. Cheap, and the previous
  # implementation asserted nothing at all.
  stopifnot(nrow(muni) == nrow(mex_muni),
            !anyNA(muni$group_id),
            nrow(aggregated_final) < nrow(mex_muni),
            all(aggregated_final$total_pop >= threshold),
            isTRUE(all.equal(sum(aggregated_final$total_pop), sum(muni$total_pop))))

  n_per_unit <- table(muni$group_id)
  n_states   <- tapply(substr(mun_lab, 1, 2), muni$group_id, function(x) length(unique(x)))

  diagnostics <- list(
    threshold          = threshold,
    n_units            = nrow(aggregated_final),
    n_merges           = n_merges,
    n_units_merged     = sum(n_per_unit > 1),
    n_units_singleton  = sum(n_per_unit == 1),
    max_muns_per_unit  = max(n_per_unit),
    n_island           = n_island,
    n_within_relaxed   = n_within_relaxed,
    n_crossing_state   = sum(n_states > 1),
    mean_nb            = adj$mean_nb,
    n_zero_nb          = adj$n_zero,
    n_components       = adj$n_components,
    snap = snap, crs = crs)

  if (verbose) {
    message(sprintf(paste0("group_until_threshold(%s): %d units from %d municipalities ",
                           "(%d merged, %d already above threshold), %d merges, max %d muns/unit."),
                    format(threshold, big.mark = ","), diagnostics$n_units, n,
                    diagnostics$n_units_merged, diagnostics$n_units_singleton,
                    n_merges, diagnostics$max_muns_per_unit))
    message(sprintf("group_until_threshold(%s): median unit population %s; %d units cross a state line; %d centroid fallbacks.",
                    format(threshold, big.mark = ","),
                    format(round(median(aggregated_final$total_pop)), big.mark = ","),
                    diagnostics$n_crossing_state, n_island))
    if (n_island > 0) {
      message("group_until_threshold: WARNING - centroid fallback fired. With a correct ",
              "contiguity graph the country is one connected component and it should not.")
    }
  }

  list(aggregated = aggregated_final, muni_ref_with_groups = muni,
       diagnostics = diagnostics)
}

#-------------------------------------------------------------------------------
# group_small_municipalities() lived here. It was dead code (its only call site
# in ch2_010 was commented out) and wrong in three independent ways: it used the
# broken st_touches adjacency, it merged into the HIGHEST-population neighbour
# (the opposite of the documented rule), and being a single non-iterative pass
# its output satisfied no threshold at all. Moved to old-R/ for the record.
