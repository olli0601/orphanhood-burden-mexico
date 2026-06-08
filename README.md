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
  < 100 MB). The default pipeline starts from the already-processed per-year datasets, so
  `scripts-R/ch1_015_bootstrap_from_processed.R` runs out of the box once the
  auto-downloadable files are retrieved — extracting these archives is only needed to
  regenerate the per-year panels from raw (run `ch1_011`, see Data → *Manual raw archives*).

**1. Chapter 1–3 (Raw data → harmonised panels), in order.** Filename order is run
order. The grouping (`ch2_010`) precedes `ch2_040`/`ch2_050`/`ch2_060` because those
attach the grouped-municipality ids it builds (hence the mortality/fertility cleaning
sits under the `ch2_` prefix, after the aggregation). `ch1_011` is only needed when
(re)building the per-year panels from the raw archives — skip it on the default
"start from the supplied processed datasets" path.

```bash
pixi run Rscript scripts-R/ch1_010_get_input_data_raw.R               # download / locate raw inputs
pixi run Rscript scripts-R/ch1_011_extract_raw_archives.R            # extract manual .tar.zst (births/deaths) — only when (re)building panels from raw
pixi run Rscript scripts-R/ch1_015_bootstrap_from_processed.R         # per-year -> births/deaths/population/geo_info/marg_index
pixi run Rscript scripts-R/ch2_010_group_mun.R                        # municipality aggregation (519 grouped units)
pixi run Rscript scripts-R/ch2_040_clean_mort.R                       # -> mort.RDS (original municipalities)
pixi run Rscript scripts-R/ch2_050_clean_fert.R                       # -> fert.RDS (original municipalities)
pixi run Rscript scripts-R/ch2_060_prepare_datasets.R                 # grouped-mun panels + marginalization + rural/urban
pixi run Rscript scripts-R/ch3_010_clean_mortality_by_grouped_mun.R   # -> mort_by_grouped_mun.RDS
pixi run Rscript scripts-R/ch3_020_clean_fertility_by_grouped_mun.R   # -> fert_by_grouped_mun.RDS
```


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
- **Windows, without pixi:** open directly in **7-Zip ≥ 21.01** (free), or with a standalone
  `zstd.exe` (`zstd -d file.tar.zst` then untar). Windows Explorer's built-in zip handler
  does **not** read `.tar.zst`, so one of these tools is required.
- **macOS/Linux shell:** `zstd -dc file.tar.zst | tar xf -`.

The default pipeline runs from the already-processed per-year datasets, so extracting the raw
archives is only needed to regenerate from scratch.

### Pre-processed datasets

Pre-processed fertility and mortality datasets (death/birth counts by age group, sex, and
municipality) were produced from the raw INEGI files. Mirror copies are available via
Dropbox: [fertility](https://www.dropbox.com/t/IKzEQFCeuFFFKi5h),
[mortality](https://www.dropbox.com/t/ZIbYNxir2hkWTx29).

### Reference / external estimates

- Labour-force context (ENOE): <https://en.www.inegi.org.mx/programas/enoe/15ymas/>
- TFR / demographic reconciliation (*conciliación demográfica*): CONAPO / INEGI.

## Tested on

The pipeline through the end of Chapter 3 (`ch1_010` → `ch3_020_clean_fertility_by_grouped_mun`)
has been run end-to-end and verified on:

- **OS:** macOS 26.3.1 (Darwin 25.3.0), Apple Silicon (`arm64`).
- **R:** 4.5.3 (`aarch64-apple-darwin20`), managed via pixi (`pixi.toml`).

Chapters 4–5 are not yet verified end-to-end. The environment is pixi-managed, so other platforms should work in principle, but only the above has been tested.
