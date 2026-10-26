# =============================================================================
# ch2_055_marginalization_index.R  ·  Chapter 2 — Fixed-weight marginalization
# Municipal marginalization index comparable over time, 1990-2020: weights are
# estimated ONCE from a PCA pooled over all municipality-years, instead of being
# re-fitted each census as in CONAPO's published IM/IMN. Method after Pelaez
# Herreros (2023, EconoQuantum 20(1), 59-82; "IMAC"), who publishes it for the
# 32 states only; here it is re-estimated at municipal level.
#
# Indicators: CONAPO's nine, MINUS income (PO2SM, "<= 2 minimum wages") —
# dropped by decision with Oliver (benchmarked to a policy variable that Mexico
# raised steeply from 2019). Eight remain, all % of population / dwellings:
#   illiterate, no complete primary (15+), no drainage, no electricity,
#   no piped water, overcrowding, dirt floors, localities < 5,000.
#
# Years 1990, 2000, 2005, 2010, 2015, 2020. Sources and breaks:
#   - 1990-2015: CONAPO's stacked 90-15 file (PCA-era definitions).
#     1995 is dropped: it was a population count, only 3 of the 9 indicators
#     are published (Pelaez drops it too, for inconsistencies).
#     2000 publishes drainage as OVSDSE ("sin drenaje ni servicio sanitario
#     exclusivo") instead of OVSDE; used as that year's drainage.
#   - 2020: CONAPO IMM_2020 for seven indicators. Education is rebuilt from the
#     2020 census (ITER, municipal totals) as (no schooling + incomplete primary)
#     / population 15+, because CONAPO 2020 publishes only "without basic
#     education" (SBASC), a different variable. Validated below by applying the
#     same formula to ITER 2010 and comparing with CONAPO's 2010 SPRIM.
#     2020 drainage keeps CONAPO's new definition (river/lake/sea outlets now
#     count as no drainage); its DP2 re-runs of 2010/2015 show the shift is small
#     — printed below. ITER cannot reproduce the old definition.
#
# Index: PC1 of the pooled, standardised indicators gives loadings a_i; on the
# raw percentages the coefficient is a_i / sd_i (pooled sd). Normalised to sum
# to one, the index is a weighted average of the eight percentages: 0-100,
# HIGHER = MORE MARGINALIZED (the opposite of CONAPO's IMN; see CLAUDE.md).
# Unweighted across municipalities, as CONAPO does.
#
# Standalone for now: nothing downstream reads its output yet (ch2_060 still
# interpolates CONAPO's IMN 2010/2015/2020).
#
# Reads : input-data-raw/marginalization/{Base_Indice_de_marginacion_municipal_90-15.csv,
#         IMM_2020.xls, IMM_DP2_2010.xlsx, IMM_DP2_2015.xlsx},
#         input-data-raw/census/iter_00_cpv{2020,2010}/conjunto_de_datos/*.csv
# Writes: input-data-processed/marg_fixed_weight_mun.RDS
#         output/ch2/ch2_055_{weights,validation,national_means}.csv
# Run after: ch1_010 (raw inputs only; independent of the rest of chapter 2)
# =============================================================================

suppressMessages(library(dplyr))
source("R/marginalization.R")

raw     <- "input-data-raw"
out_dir <- "output/ch2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

indicators <- c("illiterate_pct", "no_primary_pct", "no_drainage_pct",
                "no_electricity_pct", "no_piped_water_pct", "overcrowding_pct",
                "dirt_floors_pct", "small_towns_pct")
years <- c(1990, 2000, 2005, 2010, 2015, 2020)

# --- 1. CONAPO 1990-2015 series ---------------------------------------------
s9015 <- read.csv(file.path(raw, "marginalization", "Base_Indice_de_marginacion_municipal_90-15.csv"),
                  fileEncoding = "latin1", check.names = FALSE,
                  na.strings = c("-", "N. D."))
# Last column is "AÑO"; match by position, but check it really is the year.
stopifnot(grepl("^A.O$", names(s9015)[ncol(s9015)]))
names(s9015)[ncol(s9015)] <- "year"

mun_9015 <- s9015 |>
  filter(year %in% years) |>
  transmute(
    mun = sprintf("%05d", CVE_MUN), year, population = POB_TOT,
    illiterate_pct     = ANALF,
    no_primary_pct     = SPRIM,
    no_drainage_pct    = if_else(year == 2000, OVSDSE, OVSDE),
    no_electricity_pct = OVSEE,
    no_piped_water_pct = OVSAE,
    overcrowding_pct   = VHAC,
    dirt_floors_pct    = OVPT,
    small_towns_pct    = `PL<5000`,
    conapo_IM = IM, conapo_GM = GM)

# --- 2. Census ITER: rebuild "15+ without complete primary" -----------------
# Municipal totals are the LOC == 0000 rows (MUN == 000 is the state total).
# 2010 headers are lowercase, 2020 uppercase.
read_iter_primary <- function(path) {
  x <- data.table::fread(path, colClasses = "character", encoding = "UTF-8")
  names(x) <- toupper(names(x))
  x <- x[x$LOC == "0000" & x$MUN != "000", ]
  num <- function(v) as.numeric(v)  # municipal totals are never suppressed ("*")
  out <- tibble(mun = paste0(x$ENTIDAD, x$MUN),
                no_primary_pct = 100 * (num(x$P15YM_SE) + num(x$P15PRI_IN)) / num(x$P_15YMAS))
  stopifnot(!anyNA(out$no_primary_pct), !anyDuplicated(out$mun))
  out
}
iter_dir <- file.path(raw, "census")
edu_2020 <- read_iter_primary(file.path(iter_dir, "iter_00_cpv2020", "conjunto_de_datos",
                                        "conjunto_de_datos_iter_00CSV20.csv"))
edu_2010 <- read_iter_primary(file.path(iter_dir, "iter_00_cpv2010", "conjunto_de_datos",
                                        "iter_00_cpv2010.csv"))

# Validation: the same formula on 2010 vs CONAPO's published 2010 SPRIM.
val_edu <- inner_join(edu_2010, filter(mun_9015, year == 2010), by = "mun",
                      suffix = c("_iter", "_conapo")) |>
  mutate(d = no_primary_pct_iter - no_primary_pct_conapo)
message(sprintf(paste0("== education rebuild, 2010 check (%d municipalities) ==\n",
                       "  cor %.5f | mean diff %+.3f pt | max |diff| %.2f pt | %.0f%% within 0.1 pt"),
                nrow(val_edu), cor(val_edu$no_primary_pct_iter, val_edu$no_primary_pct_conapo),
                mean(val_edu$d), max(abs(val_edu$d)), 100 * mean(abs(val_edu$d) < 0.1)))

# --- 3. 2020: CONAPO IMM_2020 + rebuilt education ---------------------------
imm_2020 <- load_imm(file.path(raw, "marginalization", "IMM_2020.xls"), 2020, skip = 5) |>
  filter(!is.na(mun))  # drops the "Nacional" row
mun_2020 <- imm_2020 |>
  transmute(mun, year = 2020, population,
            illiterate_pct, no_drainage_pct, no_electricity_pct, no_piped_water_pct,
            overcrowding_pct, dirt_floors_pct, small_towns_pct,
            conapo_IM = as.numeric(IM), conapo_GM = GM) |>
  left_join(edu_2020, by = "mun")
n_miss <- sum(is.na(mun_2020$no_primary_pct))
if (n_miss > 0) stop(n_miss, " municipalities in IMM_2020 have no ITER 2020 match")

# Drainage definition shift: CONAPO re-ran 2010/2015 under the 2020 definition
# (DP2 files); compare with the old-definition values used here.
for (y in c(2010, 2015)) {
  dp2 <- load_imm(file.path(raw, "marginalization", sprintf("IMM_DP2_%d.xlsx", y)), y,
                  sheet = sprintf("IMM_%d", y))
  d <- inner_join(filter(mun_9015, year == y), dp2, by = "mun")
  diff <- d$no_drainage_pct.y - d$no_drainage_pct.x
  message(sprintf("  drainage %d, new minus old definition: mean %+.3f pt, max |diff| %.2f pt",
                  y, mean(diff, na.rm = TRUE), max(abs(diff), na.rm = TRUE)))
}

# --- 4. Pooled panel, PCA, fixed weights ------------------------------------
panel <- bind_rows(mun_9015, mun_2020) |> arrange(mun, year)
complete <- stats::complete.cases(panel[, indicators])
message(sprintf("== panel: %d municipality-years, %d dropped for missing indicators ==",
                nrow(panel), sum(!complete)))
print(panel[!complete, c("mun", "year", indicators)])
panel <- panel[complete, ]

X   <- as.matrix(panel[, indicators])
pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
a   <- pca$rotation[, 1]
if (sum(a) < 0) a <- -a  # orient so higher = more deprived
if (any(a <= 0)) warning("PC1 loading(s) not all positive: ",
                         paste(names(a)[a <= 0], collapse = ", "))
w <- (a / pca$scale) / sum(a / pca$scale)

weights <- tibble(indicator = indicators, pc1_loading = a, pooled_sd = pca$scale, weight = w)
message(sprintf("== PC1 explains %.1f%% of pooled variance ==",
                100 * summary(pca)$importance[2, 1]))
print(weights, n = Inf)

panel$marg_index <- as.vector(X %*% w)

# --- 5. Checks against CONAPO, and national trend ---------------------------
# CONAPO's IM is PCA-era for 1990-2015 (higher = more marginalized) and DP2 for
# 2020 (higher = LESS marginalized), so the expected sign flips in 2020.
validation <- panel |>
  group_by(year) |>
  summarise(n_mun = n(),
            spearman_vs_conapo_IM = cor(marg_index, conapo_IM, method = "spearman",
                                        use = "complete.obs"),
            .groups = "drop")
national <- panel |>
  group_by(year) |>
  summarise(index_mean = mean(marg_index),
            index_popweighted = weighted.mean(marg_index, population),
            across(all_of(indicators), mean), .groups = "drop")
message("== within-year agreement with CONAPO (2020 sign is expected negative) ==")
print(validation)
message("== national means (unweighted over municipalities unless stated) ==")
print(national, width = Inf)

# --- 6. Write -----------------------------------------------------------------
saveRDS(panel |> select(mun, year, population, all_of(indicators), marg_index,
                        conapo_IM, conapo_GM),
        "input-data-processed/marg_fixed_weight_mun.RDS")
write.csv(weights,    file.path(out_dir, "ch2_055_weights.csv"),        row.names = FALSE)
write.csv(validation, file.path(out_dir, "ch2_055_validation.csv"),     row.names = FALSE)
write.csv(national,   file.path(out_dir, "ch2_055_national_means.csv"), row.names = FALSE)
message("Wrote input-data-processed/marg_fixed_weight_mun.RDS (", nrow(panel), " rows)")
