# Orphanhood Burden in Mexico

Estimation of orphanhood incidence and prevalence in Mexico (2005–2023) from vital statistics, with delay-adjusted nowcasting of births and standardized adjustments for double and prior orphanhood. Based on Elsa Farinella's EPFL Master's thesis (2025), supervised by Oliver Ratmann (external) and Victor Panaretos (EPFL).

## Repository layout

- `R/` — shared helper functions (sourced, not run directly): preprocessing, rate
  computation, plotting, municipality grouping, `load_population()`.
- `scripts-R/` — the analysis pipeline, ordered `chN_0X0_*.R` by chapter and
  dependency (run these).
- `scripts-py/` — the NowcastPNN (Python) model.
- `src/stan/` — Stan model files. `old-R/` — archived / superseded code.
- `input-data-raw/` — source data. Auto-downloadable files are fetched by `ch1_010`;
  the request-gated manual files (pre-2017 births + all deaths) are committed here,
  compressed as `.tar.zst` (extract with `ch1_011`, or 7-Zip on Windows — see Data).
- `input-data-processed/` — directory where all processed input data will appear
  (contents gitignored; only the folder itself is tracked).
- `output/` — generated figures/tables (gitignored), in per-chapter subfolders.

## Environment (VScode and pixi)

**VS Code is the recommended editor** for further work: the repo ships `.vscode/settings.json`
(an R terminal profile wired to the pixi environment via `.vscode/orphanhood_pixi_r.sh`) and
`.vscode/tasks.json` (the pixi setup/run tasks). On first open, VS Code will offer the
recommended extensions from `.vscode/extensions.json`. The extensions we use (install by exact
identifier):

| Purpose                              | Extension           | Identifier                              |
| ------------------------------------ | ------------------- | --------------------------------------- |
| Environment                          | Pixi                | `prefix-dev.pixi-vscode`                |
| R language / REPL                    | R                   | `reditorsupport.r`                      |
| R Markdown / Quarto (`.Rmd`, `.qmd`) | Quarto              | `quarto.quarto`                         |
| Markdown editing                     | Markdown All in One | `yzhang.markdown-all-in-one`            |
| Markdown linting                     | markdownlint        | `davidanson.vscode-markdownlint`        |
| Git                                  | GitLens             | `eamodio.gitlens`                       |
| AI assistant                         | Claude Code         | `anthropic.claude-code`                 |
| Spell check                          | Code Spell Checker  | `streetsidesoftware.code-spell-checker` |

Install all at once from the integrated terminal, e.g. `code --install-extension reditorsupport.r`
(repeat per identifier), or accept the workspace-recommended prompt.

The R toolchain is managed with [pixi](https://pixi.sh) (`pixi.toml`).

```bash
pixi install            # solve + install R, CmdStan, spatial/stats packages
pixi run setup          # install cmdstanr (R-universe) + nimble + ungroup (CRAN), verify CmdStan
```

Run any script in VSCode/TERMINAL, select `R (orphanhood pixi)`. Alternatively, run scripts in a Terminal with `pixi run Rscript <path>`. Open an R REPL with `pixi run R` (or radian). 

## Running the pipeline

Scripts are numbered by dependency order, run in order from scratch.

**Where the raw inputs come from.** Each source is *either* already provided *or*
downloaded by a script (see the Data section below for the full description of each):

- **Auto-downloaded** by `scripts-R/ch1_010_get_input_data_raw.R`: CONAPO population &
  marginalization workbooks, GADM geometries, and INEGI registered-births 2017–2024
  (stable open-data URLs).
- **Provided, not downloadable** (no scriptable URL): INEGI registered-deaths and
  pre-2017 registered-births are committed under `input-data-raw/{births,deaths}/` as
  compressed `.tar.zst` archives (51 files, ~1.3 GB; no Git LFS — plain git, every file
  < 100 MB). These must be extracted (`ch1_011`) and preprocessed (`ch1_012`) — see below.

**`input-data-processed/` starts empty.** Its contents are gitignored, so a fresh clone has
no processed data at all: the per-year panels are built from the raw microdata by
`scripts-R/ch1_012_build_year_panels.R`. Run `ch1_010` → `ch1_011` → `ch1_012` before
anything else. (Historical note: these panels used to be committed and were removed in
`e175806`; they remain recoverable from git history at `fe868db` if a cross-check against
the original thesis inputs is ever needed.)

> **Network note (CONAPO).** `conapo.segob.gob.mx` serves an incomplete TLS certificate
> chain — it omits the *Go Daddy Secure Certificate Authority - G2* intermediate. macOS'
> system `curl` masks this via Keychain, but R inside pixi validates against a root-only
> CA bundle and fails with `SSL peer certificate or SSH remote key was not OK`, silently
> skipping the population and marginalization downloads. `ch1_010` handles this itself:
> it appends the published intermediate to a session-local copy of the CA bundle
> (`.ensure_conapo_ca()`, built in `tempdir()`). Certificate verification stays fully
> enabled. If you see `[WARN] CA bootstrap failed`, the CONAPO files will be missing and
> the script now stops with an explicit list rather than exiting successfully.

**1. Chapter 1–3 (Raw data → harmonised panels).** 
Run the following in this order in `VSCode` or using the `Terminal` via `pixi run Rscript xxx`: 
```bash
# run once to acquire and preprocess the raw data:
scripts-R/ch1_010_get_input_data_raw.R # download / locate raw inputs
scripts-R/ch1_011_extract_raw_archives.R # extract .tar.zst (births/deaths) + births .zip
scripts-R/ch1_012_build_year_panels.R # raw microdata -> per-year fert_YYYY/mort_YYYY panels

# pipeline to prepare harmonised data panels:
scripts-R/ch1_015_bootstrap_from_processed.R # per-year -> births/deaths/population/geo_info/marg_index
scripts-R/ch2_010_group_mun.R # municipality aggregation (519 grouped units)
scripts-R/ch2_040_clean_mort.R # -> creates mort.RDS (original municipalities)
scripts-R/ch2_050_clean_fert.R # -> creates fert.RDS (original municipalities)
scripts-R/ch2_060_prepare_datasets.R # grouped-mun panels + marginalization + rural/urban
scripts-R/ch3_010_clean_mortality_by_grouped_mun.R # -> creates mort_by_grouped_mun.RDS
scripts-R/ch3_020_clean_fertility_by_grouped_mun.R # -> creates fert_by_grouped_mun.RDS

# exploratory data analyses:
scripts-R/ch3_030_eda_mexico.R 
scripts-R/ch3_040_delay_analysis_deaths.R 
scripts-R/ch3_050_delay_analysis_births_and_deaths.R 
```

**2. Chapter 4 (nowcasting birth data).** 
Run the following in this order in `VSCode` or using the `Terminal` via `pixi run Rscript xxx`: 
```bash
# Chapter 4 (nowcasting) — delay-adjusted GAM birth nowcast (see docs/nowcasting_pipeline.md):
scripts-R/ch4_010_nowcast_fit_validate.R           # fit GAM (train 1990-05 / validate 06-10 / test 11-15)
scripts-R/ch4_020_nowcast_bias_correct.R           # bias / coverage / outlier correction
scripts-R/ch4_030_nowcast_barplots.R               # nowcast 2016-2023 -> registered+nowcasted barplots (Figs 12/13)
scripts-R/ch4_040_nowcast_evaluation_diagnostics.R # diagnostics (Fig 14) + per-municipality evaluation
scripts-R/ch4_050_assemble_nowcasted_births.R      # -> birth_data_all.RDS (input to Chapter 5 orphanhood)
```

**3. Chapter 5 (orphanhood estimation).**
Standardized orphanhood engine (fertility histories + child survival + parental deaths
→ incidence & prevalence, double/prior adjustment). Runs from the Chapter-3 grouped
panels; the Chapter-4 nowcasting adjustment is an optional refinement, not required.
```bash
scripts-R/ch5_010_build_long_inputs.R        # ch3 grouped panels -> single-year mort_df/deaths_df_long/births_long
scripts-R/ch5_011_build_child_mortality.R    # child deaths from raw EDR -> national child-survival schedule (+ noise diagnostics)
scripts-R/ch5_020_orphanhood_estimation.R    # incidence + prevalence (Figs 16,17,19,20) + orphanhood maps
scripts-R/ch5_030_orphanhood_uncertainty.R   # credible intervals (co-monotone Poisson resampling)
```
> Notes: child survival (ages 0–17) is built by `ch5_011` from the **raw EDR microdata**
> (which carry all ages; the per-year panels drop 0–14). Its diagnostics show the
> yearly-rate CV is ~flat (~0.33) vs grouped-unit size — i.e. at ≥50k units the noise is
> the real 1990–2023 decline, not small-N sampling — but single-age cells are sparse, so a
> **pooled national, year-specific** single-year survival schedule is broadcast to all
> units (per-group EB shrinkage is the natural refinement). The Fig-18 spatial four-panel
> + Moran/LISA block in `ch5_020` is skipped pending a per-group merge of
> orphan-rate / std-rate / poverty onto the geometry (flagged in-script).

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
  were obtained manually from the website.
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

| Years     | Direct URL (under `https://www.inegi.org.mx/contenidos/programas/natalidad/datosabiertos/<YEAR>/`) | Auto-retrievable            |
| --------- | -------------------------------------------------------------------------------------------------- | --------------------------- |
| 2017–2022 | `conjunto_de_datos_natalidad_<YEAR>_csv.zip`                                                       | ✅                           |
| 2023–2024 | `conjunto_de_datos_enr<YEAR>_csv.zip`                                                              | ✅                           |
| 1985–2016 | not published at the open-data path                                                                | ❌ (request-gated microdata) |

So **2017–2024 can be pulled automatically**; **1985–2016 cannot** via a clean URL and are provided as part of this repository.

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

### Manual raw archives (`.tar.zst`)

The request-gated manual inputs (pre-2017 registered births + all registered deaths) live
under `input-data-raw/{births,deaths}/` and are stored **compressed as `.tar.zst`** (zstd)
to keep the repository small — ~28 % smaller than the original `.zip`, and no file exceeds
100 MB. They are standard archives (POSIX tar + zstd), extractable on every OS:

- **Reproducible (any OS):** `pixi run Rscript scripts-R/ch1_011_extract_raw_archives.R`
  — uses the `zstd` CLI bundled by pixi + R's `untar()`, extracting each archive in place.

`ch1_011` also extracts the INEGI births 2017–2023 `.zip` files downloaded by `ch1_010`
(only their `conjunto_de_datos/` member; the ~180 `catalogos/` files are never read). It
keys its skip check on the **microdata file**, not the target directory — `ch1_010`'s own
`marker` check uses the directory, so a partially-extracted year would otherwise report
`[skip] natalidad_2018 already extracted` forever while the microdata was absent.

### Building the per-year panels (`ch1_012`)

`scripts-R/ch1_012_build_year_panels.R` turns the raw microdata into the per-year panels the
rest of the pipeline consumes, via `preprocess_fertility()` / `preprocess_mortality()`
(`R/preprocess_fertility.R`, `R/preprocess_mortality.R`):

```
input-data-processed/fertility datasets/fert_<YYYY>.RDS   1985-2024  (40 files)
input-data-processed/mortality datasets/mort_<YYYY>.RDS   1990-2024  (35 files)
```

Schema: `year, sex, age, mun, births|deaths, year_reg`. Each file holds the events
**registered** in that year, so `year_reg` is the filename year while `year` is the year of
**occurrence** and may be much earlier — that gap is the registration delay analysed in
Chapters 3–4. The 1985–1997 births and 1990–1997 deaths files store the occurrence year as
**two digits**; this is left as-is deliberately, because `load_year_panels()`
(`R/load_year_panels.R`) converts it and drops the `99` sentinel at load time.

Notes on sources:

- **Births:** `NACIM<YY>.dbf` for 1985–2016, `natalidad_<YYYY>/conjunto_de_datos/*.csv` for
  2017–2024. Paths are resolved from an explicit per-year table, not a glob: the birth
  microdata sits in the same flat folder as catalogue DBFs (`ENTMUN.dbf`, `CATEMLN*.dbf`),
  and INEGI filenames drift (`natalidad_<YYYY>` becomes `enr<YYYY>` in 2023).
- **Deaths:** `DEFUN<YY>.dbf` for 1990–2019, CSV for 2020–2024. 2012–2019 exist in *both*
  formats; the DBF is used so the whole 1990–2019 span comes from one consistent source.
  Watch the naming drift here too — 2020 is `registrados` (masculine), 2023 has a `_csv`
  infix before the extension, 2024 is `registradas24_csv.csv`. A naive glob over
  `deaths/conjunto_de_datos/` would also match an 81 MB `..._bitacora_de_cambios.csv`.
- Runtime is roughly 45–60 minutes (30+ DBFs and several 200 MB+ CSVs are read whole into
  memory). Completed years are skipped on re-run; set `force <- TRUE` in the script to
  rebuild. The script `stop()`s if any year is missing, rather than letting downstream
  scripts produce silently partial results.

**Validation against the original thesis inputs.** All 75 rebuilt panels were compared
against Elsa's originals recovered from git (`fe868db`, verified byte-unchanged since her
initial import):

- **Fertility — all 40 years reproduce exactly** (identical row counts, totals and row sets).
- **Mortality — all 35 years reproduce exactly on the age-banded rows** (zero differing rows
  in either direction).

Mortality *totals* differ by ~0.5–1.5%, confined entirely to the unknown-age `NA` bucket:
the current `preprocess_mortality()` treats INEGI `EDAD` codes 1000–3999 as a *known* age 0
(infants recorded in hours/days/months) and therefore outside the parental age range, where
the original counted them as "unknown". `NA`-age rows are dropped downstream when joining on
age band, so there is no effect on results — but note `correct_mortality_year()` carries a
`TODO: original helper was not ported`, i.e. these helpers are a reimplementation rather than
the thesis code.

### Reference / external estimates

- Labour-force context (ENOE): <https://en.www.inegi.org.mx/programas/enoe/15ymas/>
- TFR / demographic reconciliation (*conciliación demográfica*): CONAPO / INEGI.

## Tested on

The pipeline through the end of Chapter 3 (`ch1_010` → `ch3_020_clean_fertility_by_grouped_mun`)
has been run end-to-end **from the raw microdata**, on:

- **OS:** macOS 26.3.1 (Darwin 25.3.0), Apple Silicon (`arm64`).
- **R:** 4.5.3 (`aarch64-apple-darwin20`), managed via pixi (`pixi.toml`).

Chapters 4–5 are not yet verified end-to-end. The environment is pixi-managed, so other
platforms should work in principle, but only the above has been tested — note `pixi.toml`
declares `platforms = ["osx-arm64"]` only, so `pixi install` will need that list extended
before it solves on Linux or Windows.

`pixi run setup` is **not** required for the current pipeline: it installs `cmdstanr`/`nimble`
and compiles a CmdStan example, but no script in `scripts-R/` uses Stan or nimble (the GAM
nowcast replaced them; `src/stan/` is orphaned). `pixi install` alone is sufficient.
