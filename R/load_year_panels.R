# Loading the per-year event panels (input-data-processed/{fertility,mortality} datasets/).
# =============================================================================

# All per-year panels in `subdir` as one long frame. Adds `year_reg` from the
# filename when absent, and converts the 2-digit occurrence year of the
# `correct_years` files (19xx), dropping their 99 "unknown year" sentinel.
load_year_panels <- function(subdir, dir = "input-data-processed",
                             correct_years = 1985:1997) {
  files <- list.files(file.path(dir, subdir), pattern = "[.]RDS$", full.names = TRUE)
  stopifnot(length(files) > 0)
  dplyr::bind_rows(lapply(files, function(f) {
    yr <- stringr::str_extract(basename(f), "[0-9]{4}")
    d  <- readRDS(f)
    if (!"year_reg" %in% names(d)) d$year_reg <- yr
    if (as.integer(yr) %in% correct_years) {
      d <- d[d$year != 99, , drop = FALSE]
      d$year <- as.numeric(paste0("19", stringr::str_pad(as.character(d$year), 2, "left", "0")))
    }
    d
  }))
}

# Unknown parental age (INEGI code 99) is kept as age = NA by
# preprocess_fertility(). This spreads each such count over the age bands in
# proportion to the known-age births of the same parent sex in the same panel
# cell (municipality x occurrence year x registration year), falling back to
# the state, then the national profile, when the cell has no known-age births.
# Applied to mothers and fathers alike. Assumes unknown-age records share the
# known-age profile of their cell, which they need not
# (docs/memo_unknown_parental_age.md). Counts become fractional; total births
# are conserved exactly.
#
# Known issue: with year_reg in the cell key, late registrations fall through to
# the state profile and can land in zero-population bands (Inf rates in
# municipal fert.RDS; grouped panels unaffected). See the memo's "Open issue".
redistribute_unknown_age <- function(births, value_col = "births") {
  keys  <- c("sex", "year", "year_reg")
  cells <- c("mun", keys)
  d <- dplyr::mutate(births, state = substr(mun, 1, 2))
  total_in <- sum(d[[value_col]])

  known   <- dplyr::filter(d, !is.na(age))
  pending <- d |>
    dplyr::filter(is.na(age)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(cells, "state")))) |>
    dplyr::summarise(u = sum(.data[[value_col]]), .groups = "drop")
  u_total <- sum(pending$u)

  profile <- function(by) known |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, keys, "age")))) |>
    dplyr::summarise(n = sum(.data[[value_col]]), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, keys)))) |>
    dplyr::mutate(p = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::select(-n)

  levels <- list(cell = "mun", state = "state", national = character(0))
  placed <- list()
  placed_by_level <- c()
  for (lv in names(levels)) {
    m <- dplyr::inner_join(pending, profile(levels[[lv]]), by = c(levels[[lv]], keys),
                           relationship = "many-to-many")
    hit <- dplyr::distinct(m, dplyr::across(dplyr::all_of(cells)))
    placed_by_level[lv] <- sum(dplyr::semi_join(pending, hit, by = cells)$u)
    placed[[lv]] <- dplyr::transmute(m, year, sex, age, mun, year_reg, value = u * p)
    pending <- dplyr::anti_join(pending, hit, by = cells)
  }
  dropped <- sum(pending$u)   # no known-age birth of that sex anywhere in those years

  out <- dplyr::bind_rows(
    dplyr::transmute(known, year, sex, age, mun, year_reg, value = .data[[value_col]]),
    dplyr::bind_rows(placed)
  ) |>
    dplyr::group_by(year, sex, age, mun, year_reg) |>
    dplyr::summarise("{value_col}" := sum(value), .groups = "drop")

  stopifnot(!anyNA(out$age),
            abs(sum(out[[value_col]]) - (total_in - dropped)) < 1e-6 * total_in)
  message(sprintf(paste0(
    "redistribute_unknown_age: %s of %s births (%.2f%%) had unknown parental age; ",
    "placed by cell %s, state %s, national %s; dropped %s (no known-age profile)."),
    format(u_total, big.mark = ","), format(total_in, big.mark = ","), 100 * u_total / total_in,
    format(placed_by_level[["cell"]], big.mark = ","), format(placed_by_level[["state"]], big.mark = ","),
    format(placed_by_level[["national"]], big.mark = ","), format(dropped, big.mark = ",")))
  out
}

# Birth panels with unknown parental age redistributed. Read births through
# this, never load_year_panels() directly.
load_birth_panels <- function(dir = "input-data-processed", ...) {
  redistribute_unknown_age(load_year_panels("fertility datasets", dir = dir, ...))
}

# One row per cell with events summed over registration years and population
# kept once. Events frames carry a row per year_reg with the cell's population
# repeated, so summing population before this counts it once per row. Stops if
# population is not constant within a cell.
collapse_year_reg <- function(df, count_col,
                              keys = intersect(c("group_id", "mun", "year", "sex", "age"),
                                               names(df))) {
  out <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(n_pop = dplyr::n_distinct(population),
                     "{count_col}" := sum(.data[[count_col]], na.rm = TRUE),
                     population = dplyr::first(population), .groups = "drop")
  if (any(out$n_pop > 1))
    stop("collapse_year_reg: population varies within ", sum(out$n_pop > 1),
         " cells of ", paste(keys, collapse = " x "), ".")
  dplyr::select(out, -n_pop)
}
