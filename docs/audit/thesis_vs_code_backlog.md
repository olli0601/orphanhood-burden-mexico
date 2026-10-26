# Thesis ↔ code discrepancy backlog (Chapters 2–5)

> **STATUS: UNREVIEWED BACKLOG. NO ACTION HAS BEEN TAKEN ON ANYTHING BELOW.**
>
> This is a raw register produced by a systematic pass over the thesis text
> (`docs/Farinella_Elsa_PDM_Statistics_2024_2025_with_comments_from_olli.pdf`,
> extracted with `.pixi/envs/py/bin/pdftotext -layout`) against the code in
> `scripts-R/` and `R/`. It is recorded here so the analysis is not lost while
> work is focused on Chapter 1.
>
> **It has not been reviewed by Mateusz or Oliver, and nothing here has been
> agreed as a defect.** Several entries are methodological calls that belong to
> the researchers, not to whoever reads this file next. Verify any entry against
> the current code before acting on it — line numbers drift.
>
> Entries marked *(known)* were established and, where stated, already fixed
> during the 2026-08-01 session; they are listed for completeness.

**Classification:** MATCHES · INCONSISTENT (code and thesis disagree) ·
UNDOCUMENTED (code does something the thesis never mentions) · MISSING (thesis
describes something the code does not implement) · UNVERIFIABLE.

---

## Chapter 1–2 — data preparation and spatial aggregation

| # | Thesis | Code | Verdict |
|---|---|---|---|
| 1.1 | Step 3: "keep ages 15–64 for mothers, 15–84 for fathers" | `R/preprocess_fertility.R:72,84,144,151` — INEGI's `99` (unspecified parental age) silently falls out; 2.1% of 2018 births rising to 6.3% of 2022. Thesis never mentions it | INCONSISTENT *(known)* |
| 1.2 | §1.4: aggregated IMN = population-weighted mean; GM reassigned by "official thresholds" | `ch2_060:152-159` builds `rural_urban_area` from IMM `PL.5000`; **no official-threshold reassignment exists**. `ch3_050:138-146` re-derives GM by `ntile(IMN,5)` quintiles, which is not CONAPO's scheme | INCONSISTENT |
| 1.3 | Standardisation method | **Never defined anywhere in the thesis** — no "reference population", no "direct method". `R/rates.R` normalises weights across *both sexes*, so per-sex output is a per-head-of-parental-age-population rate, not a per-woman rate. **2026-10-26: switched to within-sex weights; `by_sex = FALSE` for both-sex mortality.** Method still undefined in the thesis | MISSING *(known, partly fixed)* |
| 1.4 | — | **Two different reference populations coexist.** `compute_std_fert_rate`/`compute_std_mort_rate` default to the mean population over all years present; `compute_std_rate_age_gender`/`compute_national_std_rate_age_gender` use `ref_year = 2020`. Figures standardised to different standards sit side by side | UNDOCUMENTED |
| 2.1 | §2.5: 2,457 → **519** units; median 23,800 → 66,300; min 50,013 | Corrected code gives **777**; the three numbers are mutually inconsistent | INCONSISTENT *(known, documented in `docs/ch2_grouping_diagnostics.md`)* |
| 2.2 | §2.4: 50,000 "minimised the CV of age-standardised death rates" over a 10k–100k sweep | Sweep never implemented; `ch2_020` shows CV falls monotonically to 100,000 | MISSING + INCONSISTENT *(known)* |
| 2.3 | §2.3: contiguity "via shared boundaries" | `st_touches()` left 655/2457 polygons with no neighbour | INCONSISTENT *(known, fixed)* |
| 2.4 | §2.3: populations summed each iteration | Merge loop did not refresh populations mid-round | INCONSISTENT *(known, fixed)* |
| 2.5 | §2.2 GADM v4.1 | 21 post-v4.1 INEGI municipalities had no polygon → spurious `NA` unit | INCONSISTENT *(known, fixed)* |
| 2.6 | §2.3 "converged after nineteen iterations" | Iteration count is not recorded anywhere | UNVERIFIABLE |

## Chapter 3 — EDA and registration delay

> **2026-10-26 status.** Addressed in code: 3.1 (Fig 3 now 1990–2023), 3.6 (pooled
> correlation replaced by per-year correlations of age-standardised rates), 3.7/3.8
> (Figs 4/6 count events; births on mother rows), 3.9/3.10 (Figs 5/7 and a completeness
> table added in `ch3_050`), 3.11/3.13/3.15/3.16 (Figs 8–11 added in `ch3_050`:
> event-weighted unit-year means, CONAPO-derived `GM`, area type), 3.12 (days → years),
> 3.17 (Figure 1 = `ch2_010_map_group_boundaries.pdf`). Still open: 3.2, 3.4, 3.5, 3.14
> (2010–2020 window for Figs 8–11 is now stated in the subtitle), 3.18, and the
> undefined "common exposure window" for Figs 5/7.

| # | Thesis | Code | Verdict |
|---|---|---|---|
| 3.1 | Fig 3: "Standardized age-specific mortality rates … **1990–2023**" | `ch3_030:185` filters mortality to **2010–2020** before standardising. Figure 3 can only contain 11 panels, not 34. Fertility (Fig 2) *is* computed before the analogous filter, so the two "companion" figures do not cover the same period | INCONSISTENT |
| 3.2 | Fig 3 "by age group" | `ch3_030:170` silently drops the 80–84 band; ages 0–14 absent | UNDOCUMENTED |
| 3.3 | — | `ch3_030:34` `filter(!age %in% 1:15)` compares a **character** band label to integers, so it matches nothing. Dead filter that reads as if it removes child ages. Same at `ch3_050:35`, `ch5_020:958` | UNDOCUMENTED (misleading dead code) |
| 3.4 | §3.1: adolescent fertility "84 per 1,000 in 1990 → ~60 by 2020"; "TFR ~3.4 → ~2.1" | **No TFR, no per-1,000 adolescent rate, no mean age at childbearing is computed anywhere.** These are the only hard fertility numbers in Chapter 3 | MISSING |
| 3.5 | §3.2 cause narrative (homicide, COVID) | No cause-of-death variable is read anywhere; all analysis is all-cause | UNVERIFIABLE |
| 3.6 | — | `ch3_030:296-297` computes `cor(fert_rate, IMN)` **pooled across age bands and years** — the pooled-statistic trap CLAUDE.md warns about | UNDOCUMENTED |
| 3.7 | Figs 4/6: "proportion of **births/deaths** registered with delay d" | `ch3_050:204` / `ch3_040:35` use `summarise(total = n())` — **this counts panel rows, not events**. A cell with 1 birth weighs the same as one with 5,000. The count columns are never summed | INCONSISTENT — headline |
| 3.8 | §3.3.1: "largest share of registrations occurs after very long delays (d ≥ 5)" | Contradicts the thesis's own Fig 5 (CDF at d=1 ≈ 0.87–0.96); Olli flags the contradiction in the margin. **3.7 is the mechanical explanation** — long-delay registrations spread over many tiny cells dominate the cell count while being a small share of events | INCONSISTENT (resolves a reviewer question) |
| 3.9 | Figs 5/7: cohort cumulative registration CDFs, `Dmax = 7`, 80/95/99% reference lines | **No code produces these.** No `cumsum`, `ecdf`, cohort binning or threshold logic anywhere | MISSING |
| 3.10 | §3.3.2: deaths "d=0 already about 0.97–0.99", hence "mortality does not require nowcasting" | Not computed. The load-bearing justification for the whole design rests on an unimplemented calculation | MISSING — consequential |
| 3.11 | Fig 9: "municipality-level **mean delays**" | `ch3_040:154-166` uses an **unweighted** `mean(delay)` over panel cells; the same script uses the correct `weighted.mean(delay, tot_deaths)` 60 lines later. Two incompatible definitions in one file | INCONSISTENT |
| 3.12 | Fig 8/9 axes "mean delay (years)" | `ch3_040:158,175,193,205` label the y-axis **"Average Delay (days)"**. Delay is `year_reg - year` | INCONSISTENT (label error) |
| 3.13 | Fig 8: births mean delay by GM | `ch3_050:336` plots **event-level** boxplots with `weight = births`, which `stat_boxplot` ignores; GM is the `ntile` proxy, not CONAPO's. Fig 8 as captioned has no producing code | INCONSISTENT |
| 3.14 | Figs 8–11 cover the study period | `ch3_040:155`, `ch3_050:151,308` restrict to **2010–2020**. Never stated | UNDOCUMENTED |
| 3.15 | Figs 10/11: delay by area type (Rural/Urban) | **No code produces these.** `rural_urban_area.RDS` is read only for two rate histograms | MISSING |
| 3.16 | Figs 8 vs 9 presented as a matched pair | They use **different GM definitions** (`ntile(IMN,5)` vs the real `GM`) | INCONSISTENT |
| 3.17 | — | `ch3_040` skips four figures because `municipality_shapes`/`grouped_mun_50000`/`your_data` never exist upstream. One of them is the only candidate producer of **Figure 1** | MISSING |
| 3.18 | — | `ch3_040:153`, `ch3_050:153,310` drop unknown-age **male** deaths but keep unknown-age female deaths. Asymmetric, unexplained | UNDOCUMENTED |

## Chapter 4 — GAM nowcast

The largest gap between text and code. The delay model described in §4.1.1, §4.1.3
and §4.1.4 is not implemented at all.

| # | Thesis | Code | Verdict |
|---|---|---|---|
| 4.1 | §4.1.3: truncate to max delay D, estimate π̂_d with a Dirichlet(α=1) prior | **No delay distribution is estimated anywhere in `ch4_*`.** Grep for `completeness|dirichlet|delay_dist|c_t|pi_hat|truncat` returns zero hits | MISSING — headline |
| 4.2 | §4.1.4: completeness factor `c_t` "enters the regression as a fixed log-offset"; §4.1.8 says the routine "stores π̂ and c_t, which explain the growing nowcast segment" | No `c_t` exists. The only offset is `log(population × national fert_rate)`. The growing 2020–2023 nowcast segment is **not** produced by the stated mechanism | MISSING + INCONSISTENT — headline |
| 4.3 | §4.1.5 step 1: "collapse delays for fitting … **without contamination from right truncation**" | `ch4_010:123` `filter(delay == 0)` — the response is delay-0 registrations only, i.e. precisely the truncated quantity the thesis says to avoid | INCONSISTENT — headline |
| 4.4 | §4.1.2: national rates standardised to a **fixed 2020 reference population** | `ch4_010:99-101` uses raw `births/population`, no standardisation, computed only over 1990–2005 | INCONSISTENT |
| 4.5 | §4.1.2/§4.1.7: nowcast anchored to the **year-specific** national curve | `ch4_030:105-108` uses the **arithmetic mean of 1990–2005** fertility, broadcast flat to 2016–2023. The nowcast is driven by 1990s fertility levels — the likeliest cause of Olli's "I found it unexpected that births rise again" | INCONSISTENT — headline |
| 4.6 | §4.1.2: `contrasts = c("contr.sum","contr.poly")` so Σβ_a = 0 | No `contrasts` argument anywhere; default treatment contrasts are used | INCONSISTENT |
| 4.7 | §4.1.2: "uses `mgcv::bam` for speed" | `ch4_010:148` uses `gam()`. `bam(` appears nowhere in the repo | INCONSISTENT |
| 4.8 | §4.1.2/§4.1.5/§4.1.8: "first fit Poisson, compute φ̂; **if φ̂ > 1.2, refit** with NB" — stated as "in code" | `ch4_010:147-150` fits NB unconditionally. No Poisson fit, no dispersion estimate, no 1.2 threshold anywhere | INCONSISTENT — headline |
| 4.9 | §4.1.4 Eqs (4)/(5): predictive intervals from Poisson/NB quantiles | `ch4_030:152-157` uses `±1.96·se.fit` — the SE of the fitted **mean**. Sampling variance omitted entirely | INCONSISTENT |
| 4.10 | Footnote 3: summing per-cell 95% bounds was "used in early iterations" | `ch4_030:299-301` still sums `lower_95`/`upper_95` in the shipped figure | INCONSISTENT |
| 4.11 | Figs 12/13: "for completed years, bars equal registered totals" | `ch4_030:57` builds 1990–2015 bars from `filter(delay == 0)` — low by the entire late-registration mass | INCONSISTENT |
| 4.12 | Fig 12/13 stacking | `ch4_030:238-241` computes `pmax(0, mu - registered)` (correct), but **`ch4_050:40` does `predicted + registered`** — a straight double count, disagreeing with ch4_030 in the same pipeline | INCONSISTENT + code bug — headline |
| 4.13 | `birth_data_all.RDS` "consumed by ch5_020 / ch5_040" | **Read by nothing.** `ch5_020` reads `births_long.RDS`. There is no `ch5_040`. Chapter 4's entire output feeds nothing, while the Discussion presents delay correction as one of "two pillars" | UNDOCUMENTED / dead |
| 4.14 | Fig 14a: "sign: predicted − observed" | `ch4_040:540` computes `observed - predicted`. Caption inverted relative to code | INCONSISTENT |
| 4.15 | Fig 14c "standardized residuals within ±2 SD" | `ch4_040:542` divides by a global `sd(residual)` — a z-score, not a Pearson/deviance residual. The claim is near-tautological | INCONSISTENT |
| 4.16 | Fig 14 is a 2×2 grid | Four separate PDFs; no assembly | MISSING (cosmetic) |
| 4.17 | — | `ch4_040` uses `predicted_corrected` for all Fig-14 diagnostics while `ch4_030` (Figs 12/13) applies **neither** correction. **Figs 12/13 and Fig 14 describe different prediction sets** | INCONSISTENT + dead code |
| 4.18 | — | The whole of `ch4_020` — multiplicative bias correction plus a coverage multiplier tuned to hit 95% empirical coverage — is **nowhere in the thesis**, yet Fig 14 presents its output as raw model diagnostics | UNDOCUMENTED — methodologically significant |
| 4.19 | Parent age bands | `ch4_010:53-58` uses female 15–54, male 15–64. §5 says women 15–49, men 15–79; §5.3.3 says 15–79 for both. **Three different ranges** in one document, none matching ch4 | INCONSISTENT |
| 4.20 | §4.1.5 step 5: nowcast `t ∈ {T_now − D, …, T_now}` | `ch4_030` hardcodes 2016–2023, not a D-derived window | INCONSISTENT |
| 4.21 | Train/validate/test split | `ch4_010:66-68` splits 1990–2005 / 2006–2010 / 2011–2015. §4.1 never mentions any split | UNDOCUMENTED |

## Chapter 5 — orphanhood estimation

| # | Thesis | Code | Verdict |
|---|---|---|---|
| 5.1 | §5: "women aged 15–49 and men aged 15–79" | `ch5_020:69-70`: female ≤ 50, male ≤ 60. **The male range is 19 years shorter than stated** | INCONSISTENT — headline |
| 5.2 | §5.3.3: "15 ≤ a ≤ 79 as per the definition of parent ages" | `ch5_020:245` `filter(upper > 15, upper <= 60)` — **all parental deaths at ages 60–79 are dropped**. Incidence scales linearly with D, so this is a first-order level effect on every headline number | INCONSISTENT — headline |
| 5.3 | §5: "annual estimates 1990–2023" | `ch5_020:509-512` truncates every output to `year >= 2005` | INCONSISTENT (minor) |
| 5.4 | — | `ch5_020:37-43` leaves `group_id = NA` unfiltered before `group_by(group_id, …)` | UNDOCUMENTED (spurious NA unit risk) |
| 5.5 | Eq (8): child hazard "combining both sexes", location-specific | `ch5_011:210-215` computes survival **by sex**; `ch5_020:119-121` takes an **unweighted mean over sexes** (not the pooled ratio Eq (8) specifies) and broadcasts a **single national curve** to every group. §5.3.2 and §5.3.3 both index `S` by `g` | INCONSISTENT |
| 5.6 | — | `ch5_011:96` uses an **inner join** to the grouping — municipalities absent from it are silently dropped from child mortality, uncounted | UNDOCUMENTED |
| 5.7 | — | `ch5_011:91` de-duplicates overlapping archives with `max(deaths)`. Defensible but arbitrary and undocumented | UNDOCUMENTED |
| 5.8 | Eq (10): `C(a,c) = F(a−c) × S(c)` | `ch5_020:157` joins survival on `child_age + 1`, i.e. uses **`S(c+1)`, not `S(c)`** | INCONSISTENT |
| 5.9 | Eq (10): `S(c)` is survival **from birth to age c** | The code multiplies by a **single one-year** survival probability (~0.998), not a cumulative product. At c = 17 the true factor is ~0.99 vs ~0.999 applied. The child-survival weighting is effectively inert — which is what makes Olli's "isn't this double accounting for child mortality?" answerable: no, because the first adjustment barely does anything | INCONSISTENT — substantive |
| 5.10 | Eq (11): band aggregation is a **population-weighted** mean over single ages | `ch5_020:226-232` `expected_children = mean(...)` — **unweighted**. The `population` column is carried the whole way and never used as a weight | INCONSISTENT — headline |
| 5.11 | Eq (11): band `A` is the band of the parent's **current age** | `ch5_020:221` bands by **`parent_age_at_birth` = a − c**, then joins against `deaths_df`'s current-age bands. A parent aged 40 with a 10-year-old is filed under 30–34 and multiplied by deaths of 30–34-year-olds. **Cohort alignment is systematically shifted by the child's age** | INCONSISTENT — headline |
| 5.12 | Eq (10): unmatched terms "omitted (set to zero)" | `ch5_020:174-178` is an update-join: rows with no historical match **keep the current-year fertility rate**. And `:161-163` **drops** rather than zeroes, which (given the unweighted mean in 5.10) inflates `C` for young parent bands | INCONSISTENT / code bug |
| 5.13 | Eq (12) `O_death = C × D` | `ch5_020:250-252` | MATCHES |
| 5.14 | Eqs (13)/(14) double-orphan correction | `ch5_020:279-281,322`. Band hazard is **population-weighted** here — notably inconsistent with 5.10 where weighting was dropped | MATCHES |
| 5.15 | Eq (16): `(c−1) ×` a single current-year hazard | `ch5_020:359-378` does an exact year-by-year accumulation instead. This is a **better** estimator and answers Olli's margin question — but the thesis presents Eq (16), and the reported "<1% impact" is stated for Eq (16) | INCONSISTENT (code better than text) |
| 5.16 | Eq (17): `O_new = O_death − ½O_double − O_prev` | `ch5_020:396-402` matches for the total | MATCHES |
| 5.17 | Figs 16/17/20: incidence and prevalence **by sex** | `ch5_020:408-413` computes `sum(expected_children*deaths - O_prev)` — **the ½·O_double term is omitted**. Every sex-disaggregated number in §5.4, §5.7 and the Discussion comes from an estimator differing from Eq (17). The sex split and the total are mutually inconsistent | INCONSISTENT — headline |
| 5.18 | Eq (18): prevalence indexed by **location g** | `ch5_020:443` collapses to national **before** the recursion. **No group-level prevalence is ever computed** | INCONSISTENT |
| 5.19 | §5.7 "per 100 children" | `ch5_020:1099-1103` divides a quantity lacking the double-orphan correction; `:1136` filters `year>=2007` with no stated reason; y-axis hard-clipped to (0,3) and (0,5), silently dropping values above the cap | UNDOCUMENTED |
| 5.20 | **Figure 18** (2×2 municipal comparison) | The four maps are produced but the assembly at `ch5_020:1057` is a bare expression, **never `ggsave`d**. Figure 18 has no file-producing code | MISSING |
| 5.21 | Fig 18 poverty panel | `ch5_020:817` fills by `IMN` with `low = red, high = green`, `midpoint = 0.85` hardcoded. IMN runs backwards, so red = least marginalized. Olli's "could you update the left column so high is red?" is about exactly this | UNDOCUMENTED |
| 5.22 | §5.6 "how could we quantify this?" (Olli) | `ch5_020:1369` calls **`quit(save="no", status=0)`** — the entire Spearman / Moran's I / LISA block at `:1372-1490` is unconditionally unreachable. The quantification exists as code, has never run, and is absent from the thesis | UNDOCUMENTED + dead |
| 5.23 | — | `ch5_020:1220-1222` hardcodes a **third** parent age range (15–49) for a correlation table that appears nowhere in the thesis | UNDOCUMENTED |

### Uncertainty (`ch5_030`)

| # | Thesis | Code | Verdict |
|---|---|---|---|
| 5.24 | **The thesis contains no methods section for orphanhood uncertainty.** The Discussion asserts estimates come "with uncertainty" and separately lists "end-to-end uncertainty propagation" as an *open* priority | `ch5_030` implements a full co-monotone Poisson bootstrap producing 95% UIs and two figures. No thesis figure shows an uncertainty band | UNDOCUMENTED + MISSING — headline |
| 5.25 | — | `ch5_030:64` builds the parent population as `age >= 15 & age < 60` **for both sexes**, whereas `ch5_020:69-70` uses female ≤50 / male ≤60. **The bootstrap resamples a different model from the one being bootstrapped**; its median will not centre on the published point estimate | INCONSISTENT / code bug — headline |
| 5.26 | — | `ch5_030:85-88,210-216` takes child survival from `mort_df.RDS`, the `ch5_010` placeholder in which **child deaths at ages 0–14 are hard-zeroed** (`ch5_010:59`). The bootstrap prevalence uses survival ≡ 1 for ages 0–14 while `ch5_020` uses the empirical curve. **Two different estimators** | INCONSISTENT / code bug — headline |
| 5.27 | — | `ch5_030:25` draws **one uniform per stratum shared across all 34 years** — a strong, undocumented dependence assumption that narrows year-on-year interval movement while widening levels | UNDOCUMENTED |
| 5.28 | — | `ch5_030:100,102` applies Poisson noise to **population** counts as if they were event counts (~0.3% relative; conceptually wrong) | UNDOCUMENTED |
| 5.29 | — | Uncertainty covers only sampling noise in births/deaths/population. Fertility-model, hazard, child-survival and band-aggregation uncertainty are all treated as fixed | UNDOCUMENTED |
| 5.30 | — | `ch5_030:290-293` hardcodes `target_B=300, chunk=25, tol_rel=0.03, tol_abs=0.02` with **early stopping on interval stability**, which biases interval width downward (it stops when a chunk happens to move the quantiles little) | UNDOCUMENTED |
| 5.31 | — | Output filenames say `_ui_incidence_` but contain **prevalence** rates; the plot titles correctly say "prevalence" | INCONSISTENT (cosmetic, trap-laying) |

---

## Thesis figure → producing script

| Fig | Producing code | Status |
|---|---|---|
| 1 | none (`ch3_040`'s candidate is guarded off) | **MISSING** |
| 2 | `ch3_030` std fert by age/year facets | matches |
| 3 | `ch3_030` std mort by age/year facets | **period is 2010–2020, captioned 1990–2023**; 80-84 dropped |
| 4 | `ch3_050_births_delay_by_occurrence.pdf` | **row-count bug (3.7)** |
| 5 | none | **MISSING** |
| 6 | `ch3_050_deaths_delay_by_occurrence.pdf` | **row-count bug (3.7)** |
| 7 | none | **MISSING** |
| 8 | `ch3_050_births_delay_by_mpi_category.pdf` | not as captioned (3.13) |
| 9 | `ch3_040_delay_by_marg.pdf` | unweighted mean; axis says "days" |
| 10, 11 | none | **MISSING** |
| 12, 13 | `ch4_030` barplots | dark bars = delay-0 only; CI = summed bounds |
| 14a–d | `ch4_040`, four separate PDFs | sign flipped; uses corrected predictions |
| 15 | `scripts-py/` (not R) | not wired in |
| 16 | `ch5_020_incidence.pdf` | see 5.2, 5.10, 5.11 |
| 17, 20 | `ch5_020_incidence_rate_sex.pdf`, `..._prevalence_rate_sex.pdf` | **omit ½·O_double** |
| 18 | four maps produced; assembly never saved | **MISSING as a figure** |
| 19 | `ch5_020_prevalence_rate_age.pdf` | y clipped; `year>=2007` |

**Six of twenty figures have no producing code: 1, 5, 7, 10, 11, 18.** Two more
(3, 8) exist but do not match their captions.

---

## Suggested priority (unreviewed — for discussion, not a work order)

1. **§4.1.3–4.1.4 is unimplemented** (4.1, 4.2, 4.3, 4.8) — the nowcast has no delay
   distribution, no completeness offset and no dispersion rule, and fits delay-0 counts.
   Either implement it or rewrite §4.1 to describe what runs.
2. **Parental deaths at 60–79 dropped** (5.2) — scales every headline orphanhood number.
3. **Sex-specific incidence omits the double-orphan term** (5.17).
4. **Band aggregation unweighted and keyed on age-at-birth** (5.10, 5.11) — Eq (11) is not
   what runs.
5. **The bootstrap resamples a different model than the point estimate** (5.25, 5.26).
6. **Figs 4/6 count panel rows, not events** (3.7) — and this resolves the thesis's own
   internal contradiction between Fig 4 and Fig 5.
7. **`ch4_050` double-counts** (4.12) — currently harmless only because nothing reads its output.
8. **Fig 3 is 2010–2020, captioned 1990–2023** (3.1).
9. **Six figures have no code** — including the two CDF figures carrying §3.3's quantitative
   claims and the two area-type figures carrying §3.3.4 entirely.
