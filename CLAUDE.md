# CLAUDE.md

## Project

Estimation of **orphanhood incidence and prevalence in Mexico** from vital statistics:
fertility histories + child survival + parental deaths → how many children lost a parent,
where, and how that tracks deprivation. Includes delay-adjusted nowcasting of births to
correct under-registration in recent years.

Based on Elsa Farinella's EPFL Master's thesis (2025), supervised by Oliver Ratmann and
Victor Panaretos. Being developed into a **peer-reviewed publication**. The thesis PDF is in
`docs/`; it is the reference for definitions, methods and figure numbering.

## Commands

```bash
pixi run Rscript scripts-R/<script>.R    # run any pipeline script
pixi run R                               # REPL
```

- **All scripts assume cwd == repo root.** No `here::here()`, no `.Rproj`, no guard.
- `pixi install` is all the setup needed. **Do not run `pixi run setup`** — it builds
  `cmdstanr`/`nimble` and compiles CmdStan, but no script in `scripts-R/` uses Stan or nimble
  (`src/stan/` is orphaned).
- Figures go to `output/chN/` (gitignored). `input-data-processed/` is gitignored and starts
  empty on a fresh clone.

## Layout

```
R/                     shared helpers (sourced, never run directly)
scripts-R/             THE pipeline — numbered chN_0X0_* by dependency order
scripts-py/            NowcastPNN (PyTorch), an alternative birth nowcast; not wired
                       into the R data flow, reads paths outside the repo
old-R/                 archived / superseded. Nothing in scripts-R/ references it
src/stan/              orphaned; no active script uses Stan
docs/                  thesis PDF + nowcasting_pipeline.md (Chapter 4 methodology)
dev/                   historical dev artefacts, superseded by docs/
input-data-raw/        source data; committed .tar.zst archives + downloads
input-data-processed/   all generated inputs
```

## Pipeline order

Run in order — several scripts overwrite earlier outputs (notably `population.RDS` and
`geo_info.RDS`, written by `ch1_015`, then `ch2_040`, then `ch2_060` with different content).
Re-running one script in isolation can silently change downstream results.

```
ch1_010  download / locate raw inputs (network)      (manifest-cached)
ch1_011  extract .tar.zst archives + births .zip     (manifest-cached; was 13 GB/run)
ch1_012  raw microdata -> per-year panels            (manifest-cached; ~11 min cold, ~8 s warm)
ch1_020  verify everything above in seconds — RUN THIS INSTEAD of re-running ch1_010..012
ch1_015  panels -> births/deaths/population/geo_info/marg_index
ch2_010  contiguity grouping of municipalities into >=50k analytical units
ch2_040  mortality cleaning + figures
ch2_050  fertility cleaning + figures
ch2_055  fixed-weight marginalization index 1990-2020 (municipal; raw inputs only)
ch2_060  grouped-municipality panels + marginalization + rural/urban   <- real panel builder
ch2_061  ch2_055 -> group-level "_fw" twins of ch2_060's MPI files
ch3_010  mortality by grouped municipality
ch3_020  fertility by grouped municipality
ch3_030  EDA: standardised rates, urban/rural, marginalization
ch3_040  death registration-delay structure vs deprivation
ch3_050  birth and death delay vs rates and deprivation
ch4_010..050  GAM birth nowcast: fit/validate -> bias-correct -> barplots -> diagnostics
              -> assemble. Needs `mkdir -p output/ch4` first (no ch4 script creates it)
ch5_010  single-year long inputs
ch5_011  child mortality from raw microdata (ages 0-17)
ch5_020  orphanhood incidence + prevalence (the main engine)
ch5_030  credible intervals (bootstrap; heaviest step in the project)
```

**Two marginalization variants.** ch3_010..050 and ch5_020 read CONAPO IMN (ch2_060) by
default; `ORPHAN_MARG=fw` switches them to the fixed-weight files (ch2_061) and sends figures
to `output/ch3_fw`, `output/ch5_fw`. The `_fw` files keep CONAPO's orientation
(`IMN = 1 - index/100`, higher = less marginalized); ch2_055's own `marg_index` is higher =
more marginalized.

Chapter 5 does **not** depend on Chapter 4 — the nowcast is a refinement, and Chapter 5 runs
from the Chapter 3 grouped panels.

## Data semantics

The per-year panels (`input-data-processed/{fertility,mortality} datasets/`) are the
pipeline's entry point. Schema: `year, sex, age, mun, births|deaths, year_reg`.

- **`year_reg` is the registration year; `year` is the year of occurrence** and can be much
  earlier. That gap *is* the registration delay analysed in Chapters 3–4.
- **`year` is stored as two digits** in births 1985–1997 and deaths 1990–1997, with a `99`
  sentinel. This is intentional: `load_year_panels()` converts it and drops the sentinel at
  load time. Do not "fix" it upstream — that double-corrects.
- **For births, `sex` is the parent's sex, not the baby's.** Every birth appears twice, on the
  mother's row and the father's, so `sum(births)` is roughly double the true count. Mother
  rows are the meaningful figure.
- **Ages are five-year parental bands (`15-19`…`80-84`).** Ages 0–14 are absent by
  construction, which is why `ch5_011` returns to the raw microdata for child mortality.
- **`age = NA` is a real category in mortality**: deaths with INEGI `EDAD` 4998/4999 (age
  unknown), kept deliberately. They have no population to join to, so drop them before any
  age standardisation.
- **`age = NA` in the fertility panels is unknown parental age** (INEGI code `99`, 2.1% of
  mothers in 2018 → 11.7% in 2024). It is redistributed proportionally within each panel cell
  (mun × year × year_reg × parent sex) by `redistribute_unknown_age()`. Read births through
  `load_birth_panels()`, never `load_year_panels("fertility datasets")` directly. After
  that step, birth counts are fractional. Applied to fathers too, by decision with Oliver,
  even though most father-99 records probably have no father on them at all.
- **Mothers' age is top-coded at 50 by INEGI**, so the female `50-54` band means "50+".
- `mun` is a 5-digit zero-padded `state(2) + municipality(3)` string.

### Marginalization: IMN runs backwards

⚠️ In CONAPO's IMM workbooks, **higher `IM`/`IMN` means LESS marginalized**, the opposite of
what *índice de marginación* suggests. Verifiable against the deprivation indicators in the
same file: `cor(IMN, illiterate) ≈ -0.85`. So a **negative** correlation with IMN means
**more marginalized → higher** outcome. `GM` (the categorical grade) is correctly oriented and
is the safer variable. Check the sign convention before interpreting any coefficient or
axis involving IMN.

### Grouped municipalities

`group_id` identifies the merged analytical units. Some events have **`group_id = NA`**
(municipalities that never matched the grouping table); filter explicitly, or it surfaces in
figures as a spurious category. Verify the unit count from the data rather than from prose —
documentation and code have disagreed here.

## How to work in this repo

**Verify, don't assume.** Script header blocks (`Reads:`/`Writes:`/`Run after:`) are partly
stale and have contained circular ordering and wrong file formats. Confirm against the code.
The same applies to counts and claims in the README and thesis. State plainly when something
has *not* been verified.

**Be suspicious of checks that don't inspect what they claim to verify.** This codebase has
repeatedly inferred success from the wrong signal — treating a directory's existence as proof
of extraction, or reporting a failure via `message()` while exiting 0. When adding a guard,
assert on the actual payload.

**Reproduce from raw.** The pipeline should run from the committed archives plus the `ch1_010`
downloads. Processed panels recoverable from git history are for *validation only* — never
restore them into `input-data-processed/` as pipeline input.

**Report mismatches rather than tuning code to hide them.** When output diverges from the
thesis, quantify and disclose it. A documented near-match is a legitimate result; a silently
forced one is not.

**Don't make statistical decisions unilaterally.** Reference populations, sign conventions,
model choices and which numbers support a conclusion are the researchers' calls. Surface them
with the numbers attached and let them decide.

**Fix the helper, not the call site**, when the defect is the helper's — `R/` functions are
shared across chapters, so local workarounds hide bugs from other consumers.

**Watch for case-sensitivity.** The raw INEGI tree mixes `.DBF`/`.dbf` and `.CSV`/`.csv`.
macOS is case-insensitive, so this class of bug hides locally and appears on Linux.

**Beware pooled statistics across age bands.** Panels are stratified by age and sex, and age
dominates both fertility and mortality. Correlations or means computed over pooled rows are
usually meaningless; use the standardised rates in `R/rates.R`.

**Long steps should stay resumable** and skip completed work by default; several take tens of
minutes or read GB-scale microdata.

**Chapter 1 is manifest-cached — verify, don't rebuild.** `R/manifest.R` records what each
acquisition step produced (extracted member paths + sizes; per-panel row counts + event
totals). Skips are decided from that payload, never from a path existing. Run
`ch1_020_verify_inputs.R` to confirm the inputs are intact; force work with
`ORPHAN_FORCE_DOWNLOAD` / `ORPHAN_FORCE_EXTRACT` / `ORPHAN_FORCE_PANELS`. If you add another
expensive step, give it a manifest rather than a `file.exists()` guard.

**Never run two pipeline scripts concurrently.** `ch1_010` re-extracting a microdata CSV while
`ch1_012` was reading it crashed R with a bus error mid-rebuild. The stages share files by
design; run them in sequence.

**Some artefact names still have more than one writer.** `population.RDS`, `geo_info.RDS`,
`births.RDS`, `deaths.RDS` and `marg_index.RDS` are written by both ch1_015 and ch2_060 with
different content, so the file's meaning depends on how far the pipeline has run —
`marg_index.RDS` is a *municipal IMM table* after ch1_015 and a *group-level interpolated
index* after ch2_060. ch2_040's duplicate writes have been removed; the ch1_015/ch2_060 split
still needs distinct names. `ch1_020` reports which vintage is on disk.
