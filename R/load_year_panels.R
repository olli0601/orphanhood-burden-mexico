# load_year_panels.R
# Load all per-year processed panels from a subdirectory of input-data-processed/
# (e.g. "mortality datasets" / "fertility datasets") into one long data frame,
# adding `year_reg` (from the filename) and correcting the 2-digit occurrence
# year stored in the early files (1985-1997: 90 -> 1990, drops the 99 sentinel,
# via correct_year()). Replaces the duplicated load loop + mget()/correct_year()
# pattern in ch1_040 / ch1_050 / ch1_060.
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
