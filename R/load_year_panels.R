# load_year_panels.R
# Load all per-year processed panels from a subdirectory of input-data-processed/
# (e.g. "mortality datasets" / "fertility datasets") into one long data frame,
# adding `year_reg` (from the filename) and correcting the 2-digit occurrence
# year stored in the early files (1985-1997: 90 -> 1990, drops the 99 sentinel,
# via correct_year()). Replaces the duplicated load loop + mget()/correct_year()
# pattern in ch2_040 / ch2_050 / ch2_060.
# =============================================================================
load_year_panels <- function(subdir, dir = "input-data-processed",
                             correct_years = 1985:1997) {
  files <- list.files(file.path(dir, subdir), pattern = "[.]RDS$", full.names = TRUE)
  stopifnot(length(files) > 0)
  dplyr::bind_rows(lapply(files, function(f) {
    yr <- stringr::str_extract(basename(f), "[0-9]{4}")
    d  <- readRDS(f)
    if (!"year_reg" %in% names(d)) d$year_reg <- yr
    if (as.integer(yr) %in% correct_years) {
      # 2-digit occurrence year -> 19xx; drop the 99 sentinel (cf. correct_year())
      d <- d[d$year != 99, , drop = FALSE]
      d$year <- as.numeric(paste0("19", stringr::str_pad(as.character(d$year), 2, "left", "0")))
    }
    d
  }))
}

# Unknown parental age (INEGI code 99) -----------------------------------------
# preprocess_fertility() keeps births whose parent's age is coded 99 as
# age = NA. This spreads each such count over the age bands in proportion to the
# known-age births of the same parent sex in the same panel cell: municipality x
# occurrence year x registration year. A cell with no known-age births borrows
# the profile of its state, then of the country, for the same sex and years.
#
# Decided with Oliver (Oct 2026): plain proportional redistribution, applied to
# mothers and fathers alike. Assumes unknown-age records share the age profile of
# the known-age ones in their cell. They are not missing at random
# (docs/memo_unknown_parental_age.md), and most father-99 records probably have
# no father on them at all, so this raises paternal counts and hence paternal
# orphanhood. Counts become fractional; total births are conserved exactly.
#
# KNOWN ISSUE (deferred): year_reg in the cell key makes late registrations fall
# through to the state profile, which can place births in zero-population bands
# (Inf rates in municipal fert.RDS; grouped panels unaffected). See "Open issue"
# in docs/memo_unknown_parental_age.md.
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

# The births panels with unknown parental age redistributed. Every consumer of
# births goes through this rather than load_year_panels() directly.
load_birth_panels <- function(dir = "input-data-processed", ...) {
  redistribute_unknown_age(load_year_panels("fertility datasets", dir = dir, ...))
}

# Registration delay (occurrence -> registration), with an optional bucketed
# factor capping at `max_delay`+. bucket = FALSE returns the raw numeric delay.
add_delay <- function(df, max_delay = 5, bucket = TRUE) {
  df <- dplyr::mutate(df, delay = as.numeric(year_reg) - as.numeric(year))
  if (bucket) {
    df <- dplyr::mutate(df,
      delay = dplyr::if_else(delay > max_delay, paste0(max_delay, "+"), as.character(delay)),
      delay = factor(delay, levels = c(as.character(0:max_delay), paste0(max_delay, "+"))))
  }
  df
}
