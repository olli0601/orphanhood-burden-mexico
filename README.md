# Orphanhood Burden in Mexico

Estimation of orphanhood incidence and prevalence in Mexico (2005–2023) from vital
statistics, with delay-adjusted nowcasting of births and standardized adjustments for
double and prior orphanhood. Based on Elsa Farinella's EPFL Master's thesis (2025),
supervised by Oliver Ratmann (external) and Victor Panaretos (EPFL).

## Repository layout

- `R/` — shared helper functions (sourced, not run directly): preprocessing, rate
  computation, plotting, municipality grouping, `load_population()`.
- `scripts-R/` — the analysis pipeline, ordered `chN_0X0_*.R` by chapter and
  dependency (run these).
- `scripts-py/` — the NowcastPNN (Python) model.
- `src/stan/` — Stan model files. `old-R/` — archived / superseded code.
- `input-data-raw/` — downloaded source data (gitignored bulk). `input-data-processed/`
  — per-year `fert_YYYY`/`mort_YYYY` + derived intermediates.
- `output/` — generated figures/tables (gitignored), in per-chapter subfolders.

## Environment (pixi)

The R toolchain is managed with [pixi](https://pixi.sh) (`pixi.toml`).

```bash
pixi install            # solve + install R, CmdStan, spatial/stats packages
pixi run setup          # install cmdstanr (R-universe) + nimble + ungroup (CRAN), verify CmdStan
```

Run any script in the environment with `pixi run Rscript <path>`. Open an R REPL with
`pixi run R` (or radian). Python (NowcastPNN) deps are in `scripts-py/requirements.txt`.

## Running the pipeline

Scripts are numbered by dependency order. Either supply the raw INEGI files and run
from scratch, or start from the per-year datasets already in `input-data-processed/`.

**0. Get raw inputs** (optional — population/marginalization/geometry + INEGI births
2017–2024 auto-download; deaths and pre-2017 births are manual, see Data below):

```bash
pixi run Rscript scripts-R/ch1_010_get_input_data_raw.R
```

**1. Chapter 1–3 (data → harmonised panels), in order:**

```bash
pixi run Rscript scripts-R/ch1_005_bootstrap_from_processed.R   # per-year -> births/deaths/population/geo_info/marg_index
pixi run Rscript scripts-R/ch1_040_clean_mort.R                 # -> mort.RDS
pixi run Rscript scripts-R/ch1_050_clean_fert.R                 # -> fert.RDS (mun level)
pixi run Rscript scripts-R/ch2_010_group_mun.R                 # municipality aggregation (519 units)
pixi run Rscript scripts-R/ch1_060_prepare_datasets.R          # new-municipality panels + marginalization
pixi run Rscript scripts-R/ch3_010_clean_mortality.R          # mort.RDS (new-mun)
pixi run Rscript scripts-R/ch3_020_clean_fertility.R          # fert.RDS (new-mun)
```

`ch1_005` must run first — it breaks the ch1↔ch2 circular dependency by building
`population`/`geo_info` before the grouping. Chapters 4 (nowcasting, `scripts-R/ch4_*`
+ `scripts-py/`) and 5 (orphanhood, `scripts-R/ch5_*`) follow.

> Note: some Chapter 3–5 scripts still require raw INEGI extraction for the
> orphans/long/monthly variants and a few EDA inputs (`type_of_mun.csv`); these are
> work in progress.

## Data

All inputs are harmonized to a common municipality–sex–age–year resolution. Ages are
grouped in five-year bands; municipality codes are standardized to a unified five-digit
state+municipality format. Where event counts are sparse, adjacent municipalities are
aggregated into 519 analytical units (each ≥50,000 inhabitants; see thesis Chapter 2).

### Fertility — Registered births

- **Source:** INEGI, *Estadísticas de Nacimientos Registrados* (Registered Births Statistics).
- **Portal:** <https://en.www.inegi.org.mx/datosabiertos/> →
  *Administrative Records – Statistics* → *Vital* → *Registered Births Statistics*.
  Each annual file holds births registered in that year (the birth may have occurred in
  an earlier year — relevant to the registration-delay analysis).
- **Coverage:** 1990–2023. The public portal exposes 2017–2023; earlier years (from 1990)
  were obtained courtesy of José Manuel Aburto.
- **Preprocessing:** retain variables needed for birth counts by municipality, parent sex,
  and age group; drop records with invalid/missing geographic identifiers; standardize
  municipality codes. Mothers kept at ages 15–64, fathers at ages 15–84.
- **Aggregation schemes:** *yearly* (year × sex × age group × municipality) for the main
  demographic analysis and Bayesian nowcast model; *monthly* (month × sex × age group ×
  municipality) for the NowcastPNN model.

#### Automatic retrieval (INEGI open data)

INEGI advertises registered-births microdata back to 1985 on the
[natalidad microdata page](https://www.inegi.org.mx/programas/natalidad/#microdatos), but
only the recent open-data years expose **stable, scriptable direct URLs**:

| Years | Direct URL (under `https://www.inegi.org.mx/contenidos/programas/natalidad/datosabiertos/<YEAR>/`) | Auto-retrievable |
|---|---|---|
| 2017–2022 | `conjunto_de_datos_natalidad_<YEAR>_csv.zip` | ✅ |
| 2023–2024 | `conjunto_de_datos_enr<YEAR>_csv.zip` | ✅ |
| 1985–2016 | not published at the open-data path | ❌ (request-gated microdata) |

So **2017–2024 can be pulled automatically**; **1985–2016 cannot** via a clean URL — they
sit behind the JS download widget / NADA microdata request form (the pre-2017 years used
here came courtesy of José Manuel Aburto). The portal landing page itself is
JavaScript-rendered, so links must be hit by the direct content URLs above, not scraped.

**Derivation is exact.** Running the raw open-data CSV through `preprocess_fertility()`
reproduces the supplied per-year fertility table to the dimensions we need
(`year × sex × age-group × municipality × births`) **bit-for-bit** — verified for 2020:
124,374 rows, 0 mismatched cells, 2,971,580 total births identical to the supplied
`fert_2020.RDS`. These URLs are wired into `scripts-R/ch1_010_get_input_data_raw.R`.

### Mortality — Registered deaths

- **Source:** INEGI, *Estadísticas de Defunciones Registradas* (Registered Deaths Statistics) —
  microdata page: <https://en.www.inegi.org.mx/programas/edr/#microdata> ("EDR" =
  *Estadísticas de Defunciones Registradas*).
- **Portal:** same INEGI open-data path → *Registered Deaths Statistics*.
- **File layout:** mixed — recent years are single-year zips
  (`conjunto_de_datos/…defunciones_registrad{os,as}_<YEAR>.csv`); older years arrive as
  **multi-year bundles** (e.g. `…1990_1994…`, `…2015_2019…`) each holding per-year
  `DEFUN<YY>.dbf` files, which must be split by year before processing.
- **Coverage and preprocessing:** same pipeline as fertility (outcome = death counts
  by municipality, sex, age group, time), giving structurally comparable fertility and
  mortality tables. Death registration delays are short (≈97–99% registered in the
  occurrence year), so mortality is used without nowcasting.

### Population

- **Source:** Government of Mexico official municipal population statistics
  (`gob.mx`, BD municipales). Census-based estimates 1990–2020 (decennial censuses and
  intercensal surveys) plus projections 2021–2040.
- **Coverage retained:** 1990–2023.
- **Resolution:** municipality × sex × five-year age group.
- **Preprocessing:** drop out-of-scope age groups and national aggregates; build five-digit
  municipality codes; merge into the new spatial units (summing constituent populations).
  Serves as the denominator for fertility and mortality rates.

### Marginalization index (socioeconomic context)

- **Source:** CONAPO, *Índice de Marginación Municipal* (IMM).
  <https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372>
- **Years:** 2010, 2015, 2020 (published every five years); linearly interpolated to an
  annual 2010–2020 series.
- **Fields:** continuous index `IMN` and categorical degree of marginalization `GM`
  (Very Low → Very High). The IMM is a PCA composite of nine deprivation indicators
  (illiteracy, incomplete basic education, dwellings without drainage/electricity/piped
  water, dirt floors, overcrowding, small-locality residence, low income).
- **Preprocessing:** standardize codes, merge into new spatial units, aggregate `IMN` as a
  population-weighted mean and reassign `GM` via official thresholds.

### Municipal geometries

- **Source:** GADM database v4.1 (`gadm41_MEX2.json`), with 2020 population counts.
- **Use:** spatial aggregation of the original 2,457 level-2 municipalities into 519
  analytical units via iterative contiguity-based merging (sf/dplyr in R 4.4.0).

### Pre-processed datasets

Pre-processed fertility and mortality datasets (death/birth counts by age group, sex, and
municipality) were produced from the raw INEGI files. Mirror copies are available via
Dropbox: [fertility](https://www.dropbox.com/t/IKzEQFCeuFFFKi5h),
[mortality](https://www.dropbox.com/t/ZIbYNxir2hkWTx29).

### Reference / external estimates

- Labour-force context (ENOE): <https://en.www.inegi.org.mx/programas/enoe/15ymas/>
- TFR / demographic reconciliation (*conciliación demográfica*): CONAPO / INEGI.
