# Chapter 2 — Municipality aggregation: correction and threshold diagnostics

Generated from `scripts-R/ch2_020_grouping_diagnostics.R` on a full pipeline rebuild from
raw (`ch1_010` → `ch3_050`, empty `input-data-processed/`), branch
`fix/municipality-aggregation`. Every number below is copied from that run's console block
and `output/ch2/ch2_020_grouping_diagnostics.csv`. Nothing here is from memory or from the
thesis unless explicitly attributed.

---

## 1. Why the units changed

Two defects in `R/municipality_grouping.R` made the aggregated analytical units wrong. Both
are now fixed; both changed the output substantially.

### 1.1 The contiguity graph was broken

`sf::st_touches()` on the 2,457 GADM ADM2 polygons returned:

| construction | edges | mean neighbours | polygons with none |
|---|---|---|---|
| `st_touches()` | 4,086 | 1.66 | **655** |
| `st_relate(pattern = "F***T****")` (queen) | 4,086 | 1.66 | 655 |
| `st_touches()` with `sf_use_s2(FALSE)` | 4,086 | 1.66 | 655 |
| `st_intersects()` on 10 m buffer, EPSG:6372 | **14,346** | **5.84** | **0** |

A planar contiguity graph averages 5–6 neighbours (Euler bounds it below 6), so 1.66 is not
a quirk of Mexican geography — it is a broken graph. GADM's polygons overlap by numerical
slivers rather than sharing exact boundaries, which defeats every *boundary*-based
construction identically; `spdep::poly2nb()` is in the same family and fails the same way.

The 655 polygons with no neighbour fell through to the function's nearest-centroid fallback,
so a seventh of all municipalities were merged with whatever polygon happened to be closest
by centroid, adjacent or not.

Fixed by `build_contiguity()`: `st_intersects()` on geometry projected to EPSG:6372 with an
explicit 10 m snap. It asserts on mean degree and orphan count, which is the guard that
would have caught the original defect. A buffer in *degrees* was deliberately not used —
1e-7 and 1e-3 degrees produce the identical edge count, so its value cannot be reviewed.

With correct adjacency the country is **one connected component** and the centroid fallback
now fires **zero** times at every threshold.

### 1.2 The merge loop never stopped at the threshold

The loop froze each unit's population at the start of a round and then merged every
below-threshold unit in that round. A unit therefore kept absorbing neighbours long after it
had cleared the threshold:

| | before | after |
|---|---|---|
| units at 50,000 | 552 | **777** |
| median unit population | 136,566 | 90,110 |
| median *merged* unit population | — | 79,235 |
| merged units above 100,000 | 67% | — |
| largest unit | **153 municipalities** | 55 |

`group_until_threshold()` is now a greedy one-merge-at-a-time loop that updates the unit
population immediately, so a unit stops being a merge candidate the moment it crosses the
threshold. It asserts that every unit clears the threshold and that population is conserved,
and the result is invariant to the input row order (previously it depended on GADM's row
order, so re-downloading the geometry could have silently changed every `group_id`).

Verified against a hand-computed 3×3 synthetic grid whose merge sequence can be checked by
hand — the only way to prove the stopping rule without trusting the real data.

### 1.3 The thesis figures are not reproducible

Section 2.5 reports 519 units and a median population rising 23,800 → 66,300, minimum
50,013. We get 777 units, 13,709 → 90,110, minimum 50,055.

**The reported medians are internally inconsistent, independent of any implementation.** 353
municipalities exceed 66,300 inhabitants on their own. An above-threshold unit is never a
merge candidate — it can only absorb neighbours, never shrink — so at least 353 units are
permanently above 66,300, and a *median* of 66,300 therefore requires at least 706 units in
total. 519 units and a post-merge median of 66,300 cannot come from the same run.

The pre-aggregation median of 23,800 is also unexplained: the median across the 2,457 GADM
polygons is 13,709 in 2020. A separate sweep of fourteen candidate population definitions
(2020 and 2023, various age selections), run in `notebooks/ch2_grouping_vs_thesis.ipynb`,
reaches at most 13,911; the only construction approaching 23,800 double-counts the
workbook's `TOTAL` column, which also yields 838 units rather than 519.

No attempt was made to tune parameters toward 519. The correction moves the count *away*
from it, and that is the expected direction: the old 552 was the product of over-merging.

### 1.4 A separate pre-existing defect, now guarded

21 municipality codes present in the birth/death panels have no GADM v4.1 polygon — INEGI
has created new municipalities since (e.g. `02006`, `02007`, `07120`–`07125`, `23011`).
They accounted for 0.05% of deaths and 0.03% of births, and joined to `group_id = NA`,
collapsing into a **single spurious 778th unit** indistinguishable from a real one in every
downstream table and figure.

They are now dropped explicitly with a reported count, and `ch2_060` asserts that no grouped
artefact carries an `NA` unit. **Open question for the researchers:** these municipalities
were split off from existing ones, so mapping them onto their parent unit would be better
than dropping them. That needs an INEGI split-history table we have not sourced or verified,
so it was not invented here.

---

## 2. The threshold sweep

Thesis Section 2.4 states the 50,000 threshold was chosen to minimise the coefficient of
variation of age-standardised death rates over a 10,000–100,000 sweep. **No such sweep
existed in the repository**, and nothing measured dispersion before vs after aggregation at
all — so the stated rationale for aggregating was undemonstrated.

`ch2_020_grouping_diagnostics.R` implements it, and widens it, because CV alone cannot
arbitrate the choice: aggregating always reduces noise, so a criterion that only rewards low
noise is minimised by merging the whole country into one unit. Three families of metrics are
computed at each threshold plus an ungrouped municipality baseline (threshold 0).

**Window:** occurrence years 2010–2020, all figures below. A 2010–2019 sensitivity pass runs
in the same script (2020 carries the COVID mortality shock); it moves the CV levels — median
CV 0.169 → 0.075 rather than 0.224 → 0.157 — but not the shape of any curve or any
conclusion here.

**Setup.** Rates are directly age-sex standardised via `compute_std_mort_rate()` /
`compute_std_fert_rate()` in `R/rates.R`. The baseline and every grouped run go through the
*same* code path (`unit_col = "mun"` vs `"group_id"`), and all share one standard population
built once at municipality level — otherwise `.drop_unstandardisable()` removes a different
row set at each aggregation level and the columns are quietly non-comparable. Births use
**mother rows only** (`sex` is the parent's sex; every birth appears twice). Event panels are
completed against the population grid before any zero-share is computed, because absent rows
are structural zeros.

### 2.1 Headline table (2010–2020)

| threshold | units | singleton | merged | median pop | median merged pop | max muns/unit |
|---:|---:|---:|---:|---:|---:|---:|
| 0 (municipalities) | 2457 | 2457 | 0 | 13,709 | — | 1 |
| 10,000 | 1639 | 1309 | 330 | 25,657 | 17,720 | 22 |
| 20,000 | 1269 | 860 | 409 | 40,099 | 33,136 | 22 |
| 30,000 | 1043 | 618 | 425 | 55,433 | 48,208 | 37 |
| 40,000 | 897 | 492 | 405 | 70,463 | 62,257 | 37 |
| **50,000** | **777** | 398 | 379 | 90,110 | 79,235 | 55 |
| 75,000 | 614 | 289 | 325 | 125,724 | 114,055 | 55 |
| 100,000 | 513 | 224 | 289 | 162,370 | 146,869 | 58 |

The overall median stays well above the threshold only because several hundred units are
pre-existing large municipalities that were never merged. The *merged* median is the column
that tests the stopping rule, and it sits just above the threshold at every level.

### 2.2 (a) Stability — what aggregation buys

CV of the age-standardised rate across years, median across units.

| threshold | CV deaths (both sexes) | CV births (mothers) | first-difference CV, deaths |
|---:|---:|---:|---:|
| 0 | 0.2236 | 0.1434 | 0.2737 |
| 10,000 | 0.1790 | 0.1242 | 0.2132 |
| 20,000 | 0.1655 | 0.1191 | 0.1903 |
| 30,000 | 0.1619 | 0.1178 | 0.1803 |
| 40,000 | 0.1605 | 0.1183 | 0.1754 |
| 50,000 | 0.1574 | 0.1181 | 0.1712 |
| 75,000 | 0.1559 | 0.1175 | 0.1675 |
| 100,000 | 0.1551 | 0.1183 | 0.1637 |

CV mixes real secular trend with sampling noise; the first-difference CV removes the trend.
Both tell the same story.

**Aggregation demonstrably does what it was intended to do.** But note the shape. Of the
total CV reduction available across the whole sweep (0.2236 → 0.1551), **65% is already
achieved at 10,000, 85% at 20,000 and 90% at 30,000**; the step from 30,000 to 100,000 buys
the remaining 10%. Fertility flattens even earlier — it is essentially unchanged above
20,000.

**This also means Section 2.4's stated criterion cannot have selected 50,000.** The CV falls
monotonically all the way to 100,000, so "minimise the CV of age-standardised death rates"
on its own selects the largest threshold in the sweep, not 50,000. Whatever produced the
50,000 figure, it was not this criterion applied to this quantity.

### 2.3 (b) Sparsity — "too many sparse event counts", quantified

Shares over unit × year × sex × age cells, completed against the population grid.

| threshold | zero-death cells | death cells <5 | zero-birth cells | unit-years with zero deaths |
|---:|---:|---:|---:|---:|
| 0 | 45.29% | 78.96% | 22.92% | 0.766% |
| 10,000 | 28.17% | 66.77% | 16.02% | 0.0055% |
| 20,000 | 22.11% | 55.76% | 13.97% | 0.0072% |
| 30,000 | 19.05% | 47.17% | 12.49% | 0.0087% |
| 40,000 | 17.45% | 41.06% | 11.40% | 0 |
| 50,000 | 16.37% | 35.92% | 10.45% | 0 |
| 75,000 | 15.20% | 28.63% | 8.97% | 0 |
| 100,000 | 14.79% | 24.48% | 8.09% | 0 |

The zero-count problem in its acute form — a whole unit-year with no deaths at all — is
**solved by 10,000** and gone by 40,000. Cell-level zeros fall steeply to 30,000 and then
flatten; the residual ~15% is structural (young age bands where deaths are genuinely rare),
not something a larger threshold can remove. Small counts (<5) keep falling throughout,
which is the one stability argument that does still favour larger thresholds.

### 2.4 (c) Heterogeneity loss — "blurring regional heterogeneity", quantified

Let `W_u` be unit population, `R̄_u` the unit's mean standardised rate over the window.
Between-unit variance `V_obs = Σ W_u (R̄_u − R̄)² / Σ W_u`; `η² = V_obs(t) / V_obs(0)` is the
share of municipality-level spatial variance surviving aggregation.

| threshold | η² (rate variance retained) | within-unit SD of IMN | units spanning >1 GM | pop. reclassified GM | pop. reclassified rural/urban | units crossing a state line | compactness |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 0 | 1.000 | 0 | 0% | 0.008% | 0% | 0 | 0.315 |
| 10,000 | 0.968 | 0.00091 | 13.6% | 1.54% | 0.56% | 40 | 0.272 |
| 20,000 | 0.924 | 0.00222 | 21.8% | 3.77% | 1.75% | 70 | 0.248 |
| 30,000 | 0.891 | 0.00329 | 27.7% | 5.46% | 3.01% | 91 | 0.234 |
| 40,000 | 0.861 | 0.00430 | 33.3% | 6.96% | 3.69% | 110 | 0.222 |
| 50,000 | 0.841 | 0.00574 | 36.9% | 8.57% | 4.82% | 116 | 0.212 |
| 75,000 | 0.800 | 0.00739 | 42.5% | 11.2% | 6.47% | 119 | 0.198 |
| 100,000 | 0.777 | 0.00895 | 48.5% | 13.6% | 8.18% | 118 | 0.184 |

Unlike the benefit curves, **the cost accumulates close to linearly and does not saturate**.
At 50,000, 37% of units mix more than one marginalization grade, 8.6% of the population sits
in a unit assigned a different `GM` from its own municipality's, and 4.8% is reclassified
across the rural/urban line — a variable Chapters 3 and 5 stratify on directly.

Sign convention (CLAUDE.md): **higher `IMN` means *less* marginalized**. The within-unit SD
is orientation-free, so that convention does not affect it. `GM` is correctly oriented.

**Poisson-corrected signal and noise.** Under `d ~ Poisson`, the sampling variance of a
directly standardised rate is `Var(R_u) ≈ Σ w_sa² · d_u,sa / N_u,sa²` (delta method). Then
`V_sig = max(0, V_obs − V̄_samp)` and `NSR = V̄_samp / V_sig`:

| threshold | NSR, single-year rate | NSR, 11-year mean | V_sig relative to baseline |
|---:|---:|---:|---:|
| 0 | 0.298 | 0.0271 | 1.000 |
| 20,000 | 0.174 | 0.0158 | 0.935 |
| 30,000 | 0.150 | 0.0136 | 0.903 |
| 50,000 | 0.119 | 0.0108 | 0.855 |
| 100,000 | 0.0857 | 0.0078 | 0.791 |

Note the scale difference between the two NSR columns, which is a substantive finding: over
an 11-year window, sampling noise is only ~3% of between-municipality variance **even at the
ungrouped municipality level**. The noise problem is a *single-year* problem. Any analysis
that reports multi-year means needs far less aggregation than one that maps a single year.

Caveats, stated so they are not rediscovered later: the Poisson assumption ignores
over-dispersion, so `NSR` is a **lower bound** on real noise; and `NSR` is proposed here, not
inherited from the thesis.

**Deprivation gradient.** Population-weighted OLS of `R̄_u` on unit `IMN` gives β = −0.000805
at baseline and −0.000748 at 50,000 — essentially flat, i.e. **no meaningful attenuation of
the deprivation gradient at any threshold**. This is the one heterogeneity metric that does
*not* argue against aggregation. It should be read as descriptive only: aggregation can
inflate |β| through ecological correlation even while real signal is lost, so a stable β is
not evidence that nothing was lost — `η²` and the `GM` reclassification share are the
metrics that show what was.

---

## 3. What this does and does not decide

**Decided by the code, not by preference:** the adjacency and stopping-rule bugs are fixed,
and at the currently configured 50,000 threshold the pipeline produces **777 units**.

**Not decided here — this is the researchers' call.** The evidence above does not obviously
support 50,000:

- The stability benefit is ~84% captured by 30,000 and flat thereafter.
- The acute zero-count problem is gone by 10,000–40,000.
- The heterogeneity cost keeps rising roughly linearly, with no saturation.
- Section 2.4's stated criterion (minimise CV) selects 100,000, not 50,000, so the original
  justification for the number does not hold.

A reading of these curves that stops where the benefit saturates but before the cost
compounds would land in the **20,000–40,000** range. That is a reading, not a
recommendation. Lowering the threshold would raise the unit count further (1,043 at 30,000)
and make every per-group model downstream — the Chapter 4 GAM, the Chapter 5 bootstrap —
noisier still; that cost is real and is not quantified here. `ch2_010` already writes a
30,000 grouping alongside the 50,000 one, so the alternative is a one-line change in
`R/grouped_mun.R`'s default path rather than new code.

**Downstream consequence, either way.** Going from the old 552 units to 777 cuts per-unit
event counts by roughly 30%. Every model fitted per group gets noisier. That is the correct
trade — the 552 units were the product of a bug, not of a 50,000 threshold — but it should
be stated in the write-up, and Chapters 4 and 5 have not yet been re-run against the
corrected units.

---

## 4. Reproducing

```bash
pixi run Rscript scripts-R/ch2_010_group_mun.R            # grouping (prints the unit count)
pixi run Rscript scripts-R/ch2_020_grouping_diagnostics.R # the sweep
```

Outputs (`output/` is gitignored):

- `output/ch2/ch2_020_grouping_diagnostics.{RDS,csv}` — long tidy, every metric × threshold × window
- `output/ch2/ch2_020_grouping_diagnostics_wide.csv` — one row per threshold
- `ch2_020_tradeoff_noise_vs_signal.pdf` — the decision figure
- `ch2_020_stability_cv_by_threshold.pdf`, `ch2_020_sparsity_zero_shares.pdf`,
  `ch2_020_heterogeneity_retention.pdf`, `ch2_020_gradient_attenuation.pdf`,
  `ch2_020_unit_population_distribution.pdf`

The script asserts `η²(0) = 1`, population conservation at every threshold, and monotonicity
of the unit count, zero-cell share, between-unit variance and median unit population; a
non-monotone series raises a warning rather than passing silently.
