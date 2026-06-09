# =============================================================================
# ch5_011_build_child_mortality.R  ·  Chapter 5 — Child mortality (ages 0-17)
# Replaces the "child survival ≈ 1" placeholder with EMPIRICAL child mortality
# extracted from the raw INEGI EDR microdata (DEFUN*.dbf / defunciones*.csv),
# which carry every single-year age (preprocess_mortality drops ages 0-14).
#
# Child mortality at the grouped-municipality level is sparse and noisy, so this
# script also DIAGNOSES that noise (a pop-size-invariant variability measure vs
# population), applies a simple smoother, and — per the investigation below —
# wires a POOLED (national) single-year child-survival curve into the engine
# (stable; the standard choice for small-area orphanhood, cf. life-table inputs).
#
# Reads : input-data-raw/deaths/*.tar.zst  (extracted on the fly),
#         input-data-processed/{grouped_municipality_50000,population_grouped_mun}.RDS
# Writes: input-data-processed/child_deaths_group.RDS   (group×year×sex×age deaths)
#         input-data-processed/child_survival.RDS       (year×sex×age survival, pooled)
#         output/ch5/ch5_011_child_mortality_*.pdf
# Run after: ch2_010 ; before ch5_020.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(data.table)
  library(foreign); library(ggplot2); library(sf)
})

g       <- "input-data-processed/"
fig_dir <- "output/ch5"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
cache   <- paste0(g, "child_deaths_group.RDS")

# -----------------------------------------------------------------------------
# 1. Extract child deaths (0-17) from the raw EDR microdata (cached)
# -----------------------------------------------------------------------------
# EDAD coding: 1000-3999 = age in hours/days/months (i.e. < 1 yr -> age 0);
#              4000-4017  = age in completed years 0-17; >= 4018 / 4998 = older/unknown.
decode_child_age <- function(edad) {
  edad <- suppressWarnings(as.integer(as.character(edad)))
  dplyr::case_when(edad >= 1000 & edad < 4000 ~ 0L,
                   edad >= 4000 & edad <= 4017 ~ edad - 4000L,
                   TRUE ~ NA_integer_)
}

read_child_deaths <- function(f) {
  ext <- tools::file_ext(f)
  d <- tryCatch({
    if (ext == "dbf") foreign::read.dbf(f, as.is = TRUE)
    else data.table::fread(f, showProgress = FALSE, colClasses = "character",
                           encoding = "Latin-1")   # INEGI CSVs are Latin-1/Win-1252
  }, error = function(e) NULL)
  if (is.null(d)) return(NULL)
  names(d) <- tolower(iconv(names(d), to = "ASCII//TRANSLIT", sub = ""))
  need <- c("ent_resid", "mun_resid", "sexo", "edad", "anio_ocur")
  if (!all(need %in% names(d))) return(NULL)
  d <- as.data.table(d)[, ..need]
  d[, `:=`(age  = decode_child_age(edad),
           ent  = suppressWarnings(as.integer(ent_resid)),
           mn   = suppressWarnings(as.integer(mun_resid)),
           year = suppressWarnings(as.integer(anio_ocur)),
           sex  = dplyr::case_when(as.character(sexo) == "1" ~ "male",
                                   as.character(sexo) == "2" ~ "female",
                                   TRUE ~ NA_character_))]
  d <- d[!is.na(age) & !is.na(sex) & ent != 99 & mn != 999 & year >= 1990 & year <= 2023]
  if (nrow(d) == 0) return(NULL)
  d[, mun := paste0(str_pad(ent, 2, pad = "0"), str_pad(mn, 3, pad = "0"))]
  d[, .(deaths = .N), by = .(mun, year, sex, age)]
}

if (file.exists(cache)) {
  message("ch5_011: using cached child_deaths_group.RDS")
  child_deaths_group <- readRDS(cache)
} else {
  archives <- list.files("input-data-raw/deaths", pattern = "[.]tar[.]zst$", full.names = TRUE)
  stopifnot(length(archives) > 0)
  work <- file.path(tempdir(), "ch5_011_deaths"); dir.create(work, showWarnings = FALSE)
  child_mun <- vector("list", 0)
  for (a in archives) {
    td <- file.path(work, tools::file_path_sans_ext(basename(a)))
    dir.create(td, showWarnings = FALSE)
    system2("bash", c("-c", shQuote(sprintf("zstd -dc %s | tar xf - -C %s",
                                            shQuote(a), shQuote(td)))))
    files <- list.files(td, pattern = "(?i)(DEFUN.*[.]dbf|defunciones.*[.]csv)$",
                        recursive = TRUE, full.names = TRUE)
    for (f in files) {
      cd <- read_child_deaths(f)
      if (!is.null(cd)) child_mun[[length(child_mun) + 1]] <- cd
    }
    unlink(td, recursive = TRUE)
    message("  parsed ", basename(a))
  }
  child_mun <- data.table::rbindlist(child_mun)
  # dedup: a year can appear in more than one archive -> keep max (most complete)
  child_mun <- child_mun[, .(deaths = max(deaths)), by = .(mun, year, sex, age)]

  # map original municipalities -> grouped units
  gm <- sf::st_drop_geometry(readRDS(paste0(g, "grouped_municipality_50000.RDS"))) |>
    dplyr::select(mun, group_id) |> as.data.table()
  child_deaths_group <- merge(child_mun, gm, by = "mun")[
    , .(deaths = sum(deaths)), by = .(group_id, year, sex, age)]
  saveRDS(child_deaths_group, cache)
  message("ch5_011: built child_deaths_group.RDS (", nrow(child_deaths_group), " rows)")
}

# -----------------------------------------------------------------------------
# 2. Child population (single-year 0-17) per grouped unit
# -----------------------------------------------------------------------------
child_pop <- readRDS(paste0(g, "population_grouped_mun.RDS")) |>
  mutate(a0 = as.integer(str_extract(age, "^\\d+")),
         a1 = as.integer(str_extract(age, "\\d+$"))) |>
  filter(a0 <= 17) |>
  rowwise() |> mutate(age = list(seq(a0, a1))) |> ungroup() |> unnest(age) |>
  filter(age <= 17) |>
  mutate(population = round(population / (a1 - a0 + 1))) |>
  group_by(group_id, year, age) |>                       # both sexes (child mortality ~ sex-symmetric here)
  summarise(population = sum(population), .groups = "drop")

# Empirical all-child (0-17) mortality rate per grouped unit × year
child_rate <- child_deaths_group |>
  group_by(group_id, year) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  left_join(child_pop |> group_by(group_id, year) |>
              summarise(population = sum(population), .groups = "drop"),
            by = c("group_id", "year")) |>
  filter(population > 0) |>
  mutate(rate = 1000 * deaths / population)              # child deaths per 1,000 children

# -----------------------------------------------------------------------------
# 3. DIAGNOSTIC — variability vs population size
#    y = coefficient of variation of the yearly rate within a group (scale-free,
#        i.e. invariant to the rate level / population size in expectation under a
#        constant true rate); x = mean child population. Pure sampling noise makes
#        CV blow up as N shrinks -> the small-area noise we must tame.
# -----------------------------------------------------------------------------
var_by_group <- child_rate |>
  group_by(group_id) |>
  summarise(mean_pop = mean(population), mean_rate = mean(rate),
            cv = sd(rate) / mean(rate), .groups = "drop") |>
  filter(is.finite(cv), mean_rate > 0)

p_raw <- ggplot(var_by_group, aes(mean_pop, cv)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, colour = "#b2182b") +
  scale_x_log10() +
  labs(title = "Child-mortality rate: noise vs grouped-municipality size (raw)",
       subtitle = "CV of the yearly 0-17 mortality rate within each grouped unit",
       x = "Mean child population (log scale)", y = "Coefficient of variation of rate") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch5_011_child_mortality_cv_vs_pop_raw.pdf"), p_raw, width = 8, height = 6)

# -----------------------------------------------------------------------------
# 4. SMOOTHER — simple centred running mean (k = 5 yr) of each group's rate
# -----------------------------------------------------------------------------
run_mean <- function(x, k = 5) {
  n <- length(x); out <- numeric(n); h <- (k - 1) %/% 2
  for (i in seq_len(n)) out[i] <- mean(x[max(1, i - h):min(n, i + h)], na.rm = TRUE)
  out
}
child_rate_sm <- child_rate |>
  arrange(group_id, year) |>
  group_by(group_id) |>
  mutate(rate_smooth = run_mean(rate, 5)) |>
  ungroup()

var_by_group_sm <- child_rate_sm |>
  group_by(group_id) |>
  summarise(mean_pop = mean(population), mean_rate = mean(rate_smooth),
            cv = sd(rate_smooth) / mean(rate_smooth), .groups = "drop") |>
  filter(is.finite(cv), mean_rate > 0)

p_sm <- ggplot() +
  geom_point(data = var_by_group,    aes(mean_pop, cv), alpha = 0.25, colour = "grey60") +
  geom_point(data = var_by_group_sm, aes(mean_pop, cv), alpha = 0.5,  colour = "#2166ac") +
  geom_smooth(data = var_by_group_sm, aes(mean_pop, cv), method = "loess", se = FALSE,
              colour = "#2166ac") +
  scale_x_log10() +
  labs(title = "Child-mortality rate noise after a 5-year running-mean smoother",
       subtitle = "grey = raw CV, blue = smoothed CV",
       x = "Mean child population (log scale)", y = "Coefficient of variation of rate") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch5_011_child_mortality_cv_vs_pop_smoothed.pdf"), p_sm, width = 8, height = 6)

# -----------------------------------------------------------------------------
# 5. INVESTIGATION / APPROACH  (see the two diagnostic plots + README)
#    Finding 1 — the CV of the yearly 0-17 mortality rate is ≈ flat at ~0.33
#    across the whole size range (child pop ~3e4–3e5); there is NO funnel. Because
#    every grouped unit is already ≥50k people, AGGREGATE child-mortality sampling
#    noise is largely controlled by the grouping — the ~0.33 CV mostly reflects the
#    REAL multi-decade decline in child mortality (1990-2023), not small-N noise.
#    Finding 2 — a 5-year running mean drops CV uniformly to ~0.21, i.e. it strips
#    high-frequency year-to-year jitter while leaving the underlying trend (so naive
#    per-group temporal smoothing would also attenuate genuine change).
#    Finding 3 — the real sparsity is at the SINGLE-AGE level: deaths at ages 1-17
#    are rare per group×year (the aggregate-0-17 view hides this), and survival
#    needs single-age hazards.
#    Approach — pool to a NATIONAL single-year survival schedule kept YEAR-specific
#    (preserves the real temporal decline) and broadcast to all groups. This avoids
#    single-age sparsity without flattening the national trend; per-group
#    empirical-Bayes shrinkage toward this curve is the natural refinement if
#    between-area variation is later wanted. Wired into ch5_020 / ch5_030.
# -----------------------------------------------------------------------------
nat_deaths <- child_deaths_group |>
  group_by(year, sex, age) |> summarise(deaths = sum(deaths), .groups = "drop")
nat_pop <- readRDS(paste0(g, "population_grouped_mun.RDS")) |>
  mutate(a0 = as.integer(str_extract(age, "^\\d+")),
         a1 = as.integer(str_extract(age, "\\d+$"))) |>
  filter(a0 <= 17) |>
  rowwise() |> mutate(age = list(seq(a0, a1))) |> ungroup() |> unnest(age) |>
  filter(age <= 17) |>
  mutate(population = round(population / (a1 - a0 + 1))) |>
  group_by(year, sex, age) |> summarise(population = sum(population), .groups = "drop")

child_survival <- nat_deaths |>
  right_join(nat_pop, by = c("year", "sex", "age")) |>
  mutate(deaths = coalesce(deaths, 0),
         hazard_1yr   = pmin(deaths / population, 1),
         survival_prob = 1 - hazard_1yr) |>
  select(year, sex, age, survival_prob)
saveRDS(child_survival, paste0(g, "child_survival.RDS"))

cat(sprintf("ch5_011: child_survival.RDS written (%d rows); national 0-17 survival range %.4f-%.4f\n",
            nrow(child_survival), min(child_survival$survival_prob), max(child_survival$survival_prob)))
