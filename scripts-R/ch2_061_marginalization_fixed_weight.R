# =============================================================================
# ch2_061_marginalization_fixed_weight.R  ·  Chapter 2 — Fixed-weight variant
# Group-level marginalization from ch2_055's fixed-weight index, written as
# "_fw" twins of ch2_060's files (mpi_mun, mpi_classified, mpi_imputed,
# index_grouped_mun, index_marg). Consumers read them with ORPHAN_MARG=fw.
#
# Same steps as ch2_060's marginalization section, except:
#   - Scale: IMN = 1 - marg_index / 100, so higher = LESS marginalized, as in
#     CONAPO's IMN that consumers expect (ch2_055's marg_index runs the other way).
#   - Years: rounds 1990-2020, interpolated linearly over 1990-2020.
#   - Grades: CONAPO's 2010/2015/2020 grade ranges on this fixed scale, so
#     cut-points are fixed over time and open-ended at the extremes.
#
# Reads : input-data-processed/{marg_fixed_weight_mun, grouped_municipality_50000,
#         index_grouped_mun}.RDS, input-data-raw/marginalization/IMM_{2020.xls,DP2_2015.xlsx,DP2_2010.xlsx}
# Writes: input-data-processed/{mpi_mun, mpi_classified, mpi_imputed, index_grouped_mun, index_marg}_fw.RDS
# Run after: ch2_055, ch2_060
# =============================================================================

source("R/setup.R")
proc <- "input-data-processed"

combined_index <- readRDS(file.path(proc, "marg_fixed_weight_mun.RDS")) |>
  mutate(IMN = 1 - marg_index / 100) |>
  left_join(load_imm_rounds(c("mun", "year", "GM")), by = c("mun", "year")) |>
  select(GM, IMN, mun, year, population)
saveRDS(combined_index, file.path(proc, "mpi_mun_fw.RDS"))

marg <- group_marg_index(combined_index, years = 1990:2020, rule = 2,
                         threshold_years = c(2010, 2015, 2020), open_ended = TRUE, what = "ch2_061")
message("== grade cut-points on the fixed-weight IMN scale (overlaps resolve to the first match, Muy alto first) ==")
print(marg$thresholds)
saveRDS(marg$classified, file.path(proc, "mpi_classified_fw.RDS"))
saveRDS(marg$imputed, file.path(proc, "mpi_imputed_fw.RDS"))
saveRDS(marg$index, file.path(proc, "index_grouped_mun_fw.RDS"))

index_marg <- marg$grouped |>
  group_by(group_id) |>
  summarise(marg_index_weighted = weighted.mean(IMN, population, na.rm = TRUE), .groups = "drop")
saveRDS(index_marg, file.path(proc, "index_marg_fw.RDS"))

# Same unit set as the CONAPO variant, no NA unit, every year present.
index <- marg$index
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
