# =============================================================================
# ch2_061_marginalization_fixed_weight.R  ·  Chapter 2 — Fixed-weight variant
# Group-level marginalization from ch2_055's fixed-weight index, written as
# "_fw" twins of ch2_060's MPI outputs so the two can be compared downstream:
#
#   ch2_060 (CONAPO IMN)        ch2_061 (fixed weight)
#   mpi_mun.RDS            ->   mpi_mun_fw.RDS
#   mpi_classified.RDS     ->   mpi_classified_fw.RDS
#   mpi_imputed.RDS        ->   mpi_imputed_fw.RDS
#   index_grouped_mun.RDS  ->   index_grouped_mun_fw.RDS
#   index_marg.RDS         ->   index_marg_fw.RDS
#
# Consumers pick a variant with ORPHAN_MARG=fw (see marg_file() in
# R/marginalization.R); figures then go to output/ch3_fw, output/ch5_fw.
# ch2_060 is unchanged.
#
# Same steps as ch2_060's MPI section, with three differences:
#   1. Scale. Consumers assume CONAPO's orientation (higher IMN = LESS
#      marginalized, roughly 0.3-1: ntile(IMN, 5) == 1 is "most marginalized",
#      the ch5 map is centred at 0.85). So IMN here = 1 - marg_index / 100,
#      where marg_index is ch2_055's 0-100 index (higher = MORE marginalized).
#   2. Years. Rounds 1990, 2000, 2005, 2010, 2015, 2020, interpolated linearly
#      over 1990-2020 (ch2_060: 2010-2020).
#   3. Grades. Same rule as ch2_060 — each CONAPO grade's [min, max] IMN over
#      the 2010/2015/2020 municipalities, using CONAPO's DP2 grades — but on
#      the fixed-weight scale, so the cut-points are fixed and a unit's grade
#      can now move over time. Cut-points are printed below.
#
# Reads : input-data-processed/{marg_fixed_weight_mun, grouped_municipality_50000}.RDS,
#         input-data-raw/marginalization/IMM_{2020.xls,DP2_2015.xlsx,DP2_2010.xlsx}
# Writes: input-data-processed/{mpi_mun, mpi_classified, mpi_imputed,
#         index_grouped_mun, index_marg}_fw.RDS
# Run after: ch2_055, ch2_060 (checks its unit set against index_grouped_mun.RDS)
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr) })
source("R/marginalization.R")

proc <- "input-data-processed"

fw <- readRDS(file.path(proc, "marg_fixed_weight_mun.RDS")) |>
  mutate(IMN = 1 - marg_index / 100)

# CONAPO DP2 grades, as used by ch2_060 for its cut-points.
imm <- file.path("input-data-raw", "marginalization")
gm_of <- function(x, y) x |> filter(!is.na(mun)) |> transmute(mun, year = y, GM)
dp2_gm <- bind_rows(
  gm_of(load_imm(file.path(imm, "IMM_2020.xls"), 2020, skip = 5), 2020),
  gm_of(load_imm(file.path(imm, "IMM_DP2_2015.xlsx"), 2015, sheet = "IMM_2015"), 2015),
  gm_of(load_imm(file.path(imm, "IMM_DP2_2010.xlsx"), 2010, sheet = "IMM_2010"), 2010))

combined_index <- fw |>
  left_join(dp2_gm, by = c("mun", "year")) |>
  select(GM, IMN, mun, year, population)
saveRDS(combined_index, file.path(proc, "mpi_mun_fw.RDS"))

grouped_municipality_50000 <- readRDS(file.path(proc, "grouped_municipality_50000.RDS")) |>
  sf::st_drop_geometry() |>
  select(mun, group_id)

combined_index_grouped_mun <- combined_index |>
  left_join(grouped_municipality_50000, by = "mun") |>
  filter(!is.na(group_id))
n_unmatched <- n_distinct(combined_index$mun) - n_distinct(combined_index_grouped_mun$mun)
message(sprintf("ch2_061: %d municipalities have no grouped unit and are dropped.", n_unmatched))

mpi_grouped_mun <- combined_index_grouped_mun |>
  group_by(group_id, year) |>
  summarise(mpi = weighted.mean(IMN, w = population), .groups = "drop")

thresholds <- combined_index_grouped_mun |>
  filter(year %in% c(2010, 2015, 2020), !is.na(GM)) |>
  group_by(GM) |>
  summarise(min_value = min(IMN), max_value = max(IMN)) |>
  arrange(min_value)
message("== grade cut-points on the fixed-weight IMN scale (overlaps resolve to the first match, Muy alto first) ==")
print(thresholds)

classify_gm <- function(x) {
  t <- function(g, side) thresholds[[side]][thresholds$GM == g]
  case_when(
    # Open-ended at the extremes: 1990-2005 units can be more (or less)
    # marginalized than any 2010-2020 municipality the cut-points came from.
    x <  t("Muy alto", "min_value") ~ "Very High",
    x >  t("Muy bajo", "max_value") ~ "Very Low",
    x >= t("Muy alto", "min_value") & x <= t("Muy alto", "max_value") ~ "Very High",
    x >= t("Alto",     "min_value") & x <= t("Alto",     "max_value") ~ "High",
    x >= t("Medio",    "min_value") & x <= t("Medio",    "max_value") ~ "Medium",
    x >= t("Bajo",     "min_value") & x <= t("Bajo",     "max_value") ~ "Low",
    x >= t("Muy bajo", "min_value") & x <= t("Muy bajo", "max_value") ~ "Very Low",
    TRUE ~ NA_character_
  )
}

classified_data <- combined_index_grouped_mun |>
  group_by(group_id, year) |>
  summarise(IMN = mean(IMN, na.rm = TRUE), .groups = "drop") |>
  mutate(GM = classify_gm(IMN))
saveRDS(classified_data, file.path(proc, "mpi_classified_fw.RDS"))

# Linear interpolation between rounds, 1990-2020 (ch2_060: 2010-2020).
index <- mpi_grouped_mun |>
  complete(group_id, year = 1990:2020) |>
  group_by(group_id) |>
  arrange(year, .by_group = TRUE) |>
  group_modify(~ {
    obs <- filter(.x, !is.na(mpi))
    .x$mpi <- if (nrow(obs) >= 2) approx(obs$year, obs$mpi, xout = .x$year, rule = 2)$y
              else if (nrow(obs) == 1) rep(obs$mpi, nrow(.x)) else NA_real_
    .x
  }) |>
  ungroup()
saveRDS(index, file.path(proc, "mpi_imputed_fw.RDS"))

index <- index |>
  mutate(GM = classify_gm(mpi)) |>
  rename(IMN = mpi)
saveRDS(index, file.path(proc, "index_grouped_mun_fw.RDS"))

index_marg <- combined_index_grouped_mun |>
  group_by(group_id) |>
  summarise(marg_index_weighted = weighted.mean(IMN, population, na.rm = TRUE), .groups = "drop")
saveRDS(index_marg, file.path(proc, "index_marg_fw.RDS"))

# Payload checks: same unit set as the CONAPO variant, no NA unit, full years.
conapo_units <- unique(readRDS(file.path(proc, "index_grouped_mun.RDS"))$group_id)
stopifnot(!anyNA(index$group_id),
          setequal(unique(index$group_id), conapo_units),
          all(table(index$group_id) == length(1990:2020)),
          !anyNA(index$IMN))
message("== grade distribution of grouped units by year (fixed cut-points) ==")
print(with(filter(index, year %in% c(1990, 2000, 2005, 2010, 2015, 2020)),
           table(year, factor(GM, levels = c("Very High", "High", "Medium", "Low", "Very Low")),
                 useNA = "ifany")))
message(sprintf("ch2_061: %d grouped units x %d years written (_fw).",
                length(conapo_units), length(1990:2020)))
