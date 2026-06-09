# =============================================================================
# ch5_010_build_long_inputs.R  ·  Chapter 5 — Orphanhood estimation (data prep)
# Build the single-year-age long inputs the orphanhood engine expects, from the
# Chapter-3 grouped-municipality panels (group_id level). 5-year age bands are
# split uniformly to single years (same convention the engine uses for
# population). Outputs are at GROUP level (group_id), so the engine no longer
# needs the original-municipality join.
#
#   births_long     group_id × year × sex × single-age (15-79) × births
#   deaths_df_long  group_id × year × sex × single-age (15-84) × deaths  (adult)
#   mort_df         group_id × year × sex × single-age (0-17)  × deaths  (child;
#                   ages 0-14 absent from the parent-focused panels -> deaths = 0
#                   => child survival ≈ 1. A documented approximation; extracting
#                   all-age child deaths from the raw DEFUN files would refine it,
#                   analogous to the optional nowcasting refinement on births.)
#
# Reads : input-data-processed/{births_grouped_mun,deaths_grouped_mun,population_grouped_mun}.RDS
# Writes: input-data-processed/{births_long,deaths_df_long,mort_df}.RDS
# Run before: ch5_020_orphanhood_estimation
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(stringr) })
g <- "input-data-processed/"

# Uniformly split 5-year age bands ("15-19" ...) into single years, dividing the
# value column by the band width.
split_to_single <- function(df, value_col) {
  df |>
    mutate(age_start = as.integer(str_extract(age, "^\\d+")),
           age_end   = as.integer(str_extract(age, "\\d+$"))) |>
    filter(!is.na(age_start), !is.na(age_end)) |>
    rowwise() |>
    mutate(.ages = list(seq(age_start, age_end))) |>
    ungroup() |>
    mutate(!!value_col := .data[[value_col]] / (age_end - age_start + 1)) |>
    unnest(.ages) |>
    mutate(age = .ages) |>
    select(group_id, year, sex, age, dplyr::all_of(value_col))
}

# --- births: parents 15-79 ---------------------------------------------------
births_long <- readRDS(paste0(g, "births_grouped_mun.RDS")) |>
  group_by(group_id, year, sex, age) |>
  summarise(births = sum(births), .groups = "drop") |>
  split_to_single("births") |>
  filter(age >= 15, age <= 79)
saveRDS(births_long, paste0(g, "births_long.RDS"))

# --- adult deaths (single-year) ----------------------------------------------
deaths_single <- readRDS(paste0(g, "deaths_grouped_mun.RDS")) |>
  group_by(group_id, year, sex, age) |>
  summarise(deaths = sum(tot_deaths), .groups = "drop") |>
  split_to_single("deaths")
saveRDS(deaths_single, paste0(g, "deaths_df_long.RDS"))

# --- child deaths 0-17 for survival (0-14 zero-filled; 15-17 from adult split) -
universe   <- readRDS(paste0(g, "population_grouped_mun.RDS")) |>
  distinct(group_id, year, sex)
child_zero <- tidyr::crossing(universe, age = 0:14) |> mutate(deaths = 0)
mort_df <- bind_rows(child_zero, deaths_single |> filter(age <= 17)) |>
  group_by(group_id, year, sex, age) |>
  summarise(deaths = sum(deaths), .groups = "drop")
saveRDS(mort_df, paste0(g, "mort_df.RDS"))

cat(sprintf("ch5_010: births_long %d rows, deaths_df_long %d rows, mort_df %d rows\n",
            nrow(births_long), nrow(deaths_single), nrow(mort_df)))
