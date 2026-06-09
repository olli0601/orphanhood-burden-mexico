# Nowcasting pipeline (Chapter 4)

Births are under-counted in recent years because some occurred but are not yet
registered (an *occurred-but-not-yet-reported*, OBNR, problem). Deaths do **not**
need nowcasting — ≈97–99 % are registered in the occurrence year — so the nowcast is
applied to **births only**. The thesis develops **two** methods:

| § | Method | Resolution | Implementation | Role |
|---|---|---|---|---|
| 4.1 | Delay-adjusted incidence model (GAM, Poisson/NB) | yearly | R, `mgcv::bam` (`scripts-R/ch4_*`) | **produces the documented results (Figs 12–14)** |
| 4.2 | NowcastPNN (probabilistic neural net) | monthly | Python (`scripts-py/`) | alternative / comparison method |

---

## 4.1 — Delay-adjusted incidence model (the documented result)

### Method

Within each municipality, observed data are the delay triangle `Z[t,a,s,d]` (births
that occurred in year `t`, parental age band `a`, sex `s`, registered with delay `d`)
and the exposure `N[t,a,s]` (population of potential parents).

1. **Incidence–delay factorization.** `Z[t,a,s,d] ~ Poisson(π_d · Λ[t,a,s])` — the
   incidence surface `Λ` and the delay distribution `π` are separated. By the Poisson
   thinning property, the *reported-by-today* total is
   `R(today)[t,a,s] ~ Poisson(c_t · Λ[t,a,s])`, where the **completeness factor**
   `c_t = Σ_{d ≤ T_now − t} π_d` enters the regression as a fixed **log-offset**.
2. **Incidence model (borrowing of shape).** Municipality incidence rescales a stable
   *national* age–sex fertility curve `f[t,s](a)`:
   `log Λ[t,a,s] = log f[t,s](a) + β0 + β_a + β_s + log N[t,a,s]`,
   with sum-to-zero age effects. `f` is built from national rates standardized to a
   fixed 2020 reference population. Fitted with `mgcv::bam` (parametric factors, not
   smooths, for stability in sparse cells); **Poisson first, NB fallback** when the
   Pearson dispersion `φ̂ > 1.2`.
3. **Delay distribution.** A single global `π = (π_0,…,π_D)`, `D = 10` years, estimated
   by pooled empirical proportions with a weak add-α (α = 1) Dirichlet smoothing.
4. **Nowcast open years.** For `t ∈ {T_now − D,…,T_now}`, scale predicted incidence by
   `c_t` and aggregate over `(a,s)` to municipality/national totals; present as stacked
   bars = **registered (dark) + nowcasted-additional (light)**.

### Documented outputs

- **Figure 12** — registered + nowcasted births **by sex**, 1990–2023.
- **Figure 13** — **national totals**, registered + nowcasted, 1990–2023.
  (Completed years 1990–2015 = registered only; 2016–2023 stacked. Key finding: the
  2020–2023 drop is only *partly* a delay artifact — a genuine birth decline persists
  after correction.)
- **Figure 14** — diagnostics grid: residuals vs fitted, residuals by sex,
  standardized residuals, fit vs completed-year data.

### Result-producing script chain

The Ch.4 scripts were an experimental sprawl (33 files). The result pipeline has been
isolated and **split/renamed by function into 5 scripts in `scripts-R/`, numbered in
run order** (`ch4_010` → `ch4_050`); everything else (Stan, NIMBLE, prototypes,
archived variants, the empty `ch4_220` stub, intermediate fix/diagnostic iterations) is
in `old-R/`. The chain, from the harmonised panels `births_grouped_mun.RDS` +
`population_grouped_mun.RDS`:

```
ch4_010_nowcast_fit_validate          # fit GAM (train 1990-2005, validate 2006-10, test 2011-15)
        ->  municipality_models_proper_validation.RDS, {validation,test}_{predictions,metrics}.RDS
ch4_020_nowcast_bias_correct          # bias / coverage / outlier correction
        ->  {validation,test}_predictions_corrected.RDS
ch4_030_nowcast_barplots              # nowcast 2016-2023  ->  FIG 12 + FIG 13
        ->  nowcast_barplots_by_sex.png, nowcast_barplots_total.png,
            nowcast_predictions_2016_2023.RDS, stacked_births_data_2016_2023.RDS
ch4_040_nowcast_evaluation_diagnostics  # FIG 14 (diagnostics) + per-mun evaluation
ch4_050_assemble_nowcasted_births     # registered + nowcasted -> birth_data_all.RDS (ch5 input)
```

Verified end-to-end (all `rc=0`); Figs 12/13 (`nowcast_barplots_by_sex.png`,
`nowcast_barplots_total.png`) and Fig 14 (`diagnostic_*`, `fits_vs_data_*`) regenerate;
`birth_data_all.RDS` (consumed by ch5) is produced.

Reorg / fixes applied:
- Renamed `ch4_100→010`, `ch4_110→020`, `ch4_210→040`; **split** the former `ch4_230`
  into `ch4_030` (nowcast + barplots) and `ch4_050` (the `birth_data_all` assembly for
  ch5) — these were two unrelated jobs in one file.
- `ch4_010` is self-contained — it re-fits the baseline GAM (so the old `ch4_080` is not
  needed) and writes the validation/test predictions `ch4_020` consumes.
- Bugs fixed: (i) the assembly referenced `nowcast_cell_predictions` (only produced by
  the archived `ch4_080`) + a stray `colnames()` on an undefined object → repointed to
  the saved `nowcast_predictions_2016_2023.RDS` with tolerant `any_of()` selects; (ii)
  `ch4_040` `geom_smooth(method="loess", se=TRUE)` blew the loess workspace on the full
  data → `se = FALSE`.
- Dropped dead code: the former `ch4_230` had a trailing "FERTILITY LONG" block
  (single-year graduation via `ungroup::pclm`) whose output `births_1yr` was never saved
  or read — removed in the split (this is where the `pclm nlast` error lived).
- `ch4_220` was an **empty 6-line stub**; archived to `old-R/`.

## Stan models (`src/stan/`)

`src/stan/` holds two model files — `ch4_340_nowcast_nb_plugin_yearly_nowcast_only.stan`
(Negative-Binomial plug-in) and `ch4_350_nowcast_pois_plugin_yearly_nowcast_only.stan`
(Poisson plug-in). **None of the active result scripts use Stan** — the GAM pipeline above
is pure `mgcv::bam`. The Stan callers are all archived in `old-R/`:

- `old-R/ch4_020_fertility_stan.R` and `old-R/ch4_030_mortality_stan.R` call
  `src/stan/fertility_v1.stan` / `mortality_v1.stan` — **which do not exist** (only the
  `ch4_340/350` plug-in models are present), so these scripts cannot run as-is.
- `old-R/ch4_270_newapp_nowcast.R` does **not** read `src/stan/`; it writes its own
  Poisson plug-in `.stan` inline (`writeLines`) at the working directory, then compiles
  it — functionally the same model as `ch4_350`.

So `ch4_340/350` are currently **orphaned** (no script references them by name); they
correspond to the archived Stan plug-in approach, kept for reference.

---

## 4.2 — NowcastPNN (alternative, monthly)

A probabilistic neural network (`scripts-py/`) operating on the **monthly** reporting
triangle (occurrence month × delay, `M = 36` months history, `D = 96` months max delay).
Architecture (after Koemen et al., 2025): attention over delays + 1-D convolutions over
the delay axis + categorical embeddings (municipality 25-d, sex 2-d, age 8-d) + a
**Negative-Binomial head** with municipality-specific overdispersion. Training cohorts
1990–2015 (≥8 yr follow-up = effectively complete); 2016–2023 held out for prospective
nowcasting. Entry points: `scripts-py/run_nowcast.py`, `train_negative_binomial.py`
(see `scripts-py/README.md`).

---

## Full Chapter-4 script map

Only the four **bold** scripts remain in `scripts-R/`; all others were moved to `old-R/`.

| Script | Group | Notes |
|---|---|---|
| `ch4_010_nowcast_data_preparation` | data prep | builds the (deaths) occurrence×delay triangle + indices; feeds the Stan path |
| `ch4_020_fertility_stan` / `ch4_030_mortality_stan` | alt: Stan | hierarchical Bayesian Poisson nowcast (same factorization, full Bayes) |
| `ch4_040`–`ch4_070` | alt: lightweight nowcast | model-code / run / evaluate / plot for a standalone nowcast |
| `ch4_080_gam_nowcast_baseline` | **GAM result (baseline)** | per-municipality GAM-Dirichlet baseline fit |
| `ch4_090_gam_nowcast_complete_analysis` | GAM (orchestration) | end-to-end driver |
| **`ch4_100_gam_nowcast_proper_validation`** | **GAM result** | validated fit → `municipality_models_proper_validation` |
| **`ch4_110_gam_nowcast_critical_fixes`** | **GAM result** | → `*_predictions_corrected` |
| `ch4_120`–`ch4_140` | GAM variant | robust Huber + bootstrap uncertainty (not in final figs) |
| `ch4_150`–`ch4_200` | GAM diagnostics/eval iterations | superseded by `ch4_210`/`ch4_220` |
| **`ch4_210_gam_evaluation_top_bottom_municipalities`** | **GAM result → Fig 14** | diagnostics + per-mun fits |
| **`ch4_220_gam_diagnostic_plots_for_professor`** | **GAM result** | curated review figures |
| **`ch4_230_gam_nowcast_2016_2023_barplots`** | **GAM result → Figs 12/13** | stacked registered+nowcasted barplots |
| `ch4_240_gam_nowcast_new` | GAM variant | revised specification |
| `ch4_250` / `ch4_260` `_archivio3_*` | archived | patched line-CI variant of baseline/barplots, kept for reproducibility |
| `ch4_270_newapp_nowcast` | alt: Stan | Poisson plug-in (`reduce_sum`), national totals |
| `ch4_280`–`ch4_310` | prototypes | early/standalone nowcast explorations |
| `ch4_320_nowcasting` / `ch4_330_nowcasting_Dirichlet` | alt: NIMBLE | NIMBLE (Dirichlet delay) explorations |
| `scripts-py/*` | **§4.2 NowcastPNN** | monthly neural model |

(The table above uses the *original* filenames; the four result scripts have since been
renamed/split — see the run-order chain at the top of this section.)

**Bottom line:** the thesis Chapter-4 results (Figs 12–14) come from the **GAM
delay-adjusted births nowcast**, now in `scripts-R/` as
`ch4_010 → ch4_020 → ch4_030 → ch4_040` (+ `ch4_050` to assemble the ch5 input),
verified `rc=0`. Everything else — Stan (`old-R/ch4_020/030_*_stan`, `ch4_270_newapp`),
NIMBLE, prototypes, archived `archivio3` variants, the empty `ch4_220` stub, and the
intermediate diagnostic/fix iterations — is in `old-R/`. `src/stan/ch4_340/350` are
orphaned plug-in models. The §4.2 NowcastPNN (Python, `scripts-py/`) was left untouched
per request.
