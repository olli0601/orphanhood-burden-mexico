# Chapters 1 and 2 — what was done, what was decided, what is still open

Single reference for the Chapter 1–2 work. Detail lives in
`docs/ch2_grouping_diagnostics.md` (the threshold evidence),
`docs/ch2_cleanup_changes.md` (before/after numbers for every code change),
`docs/memo_unknown_parental_age.md` (the `99` decision for Oliver) and
`docs/audit/thesis_vs_code_backlog.md` (~60 unreviewed Chapter 3–5 findings).

Everything below was measured on the pipeline, not inferred.

---

## 1. Where it started

Section 2.5 of the thesis reports 519 analytical units and a median municipal
population rising 23,800 → 66,300. The repository produced 552 units, 13,709 →
136,566. The reported medians are **internally inconsistent independent of any
implementation**: 353 municipalities exceed 66,300 inhabitants on their own, and
an above-threshold unit is never a merge candidate, so a post-merge median of
66,300 requires at least 706 units. 519 units and that median cannot come from
the same run.

Chasing 519 was abandoned. The work became: make the code do what it is supposed
to do, make the pipeline cheap and verifiable to re-run, and record what the
evidence actually supports.

## 2. Chapter 1 — acquisition made correct and cached

### Defects fixed

| | before | after |
|---|---|---|
| `ch1_011` extraction | **13 GB re-extracted on every run** (no skip condition, `zstd -f`) | 2.7 s, 51/51 verified from a payload manifest |
| `ch1_012` panels | ~10.7 min; skipped on `file.exists()` alone | 7.6 s; skips only when rows **and** event totals match a manifest |
| `ch1_010` downloads | skipped on a *directory* existing | verified against recorded members and sizes |
| verification | none | `ch1_020_verify_inputs.R` re-checks everything in seconds |

**A latent bug the false skips were hiding:** `ch1_010` could never actually
unzip the natalidad archives — `utils::unzip` aborts on the non-UTF8 filename
`catalogos/año_nacimiento.csv` with "Illegal byte sequence". The directory marker
made a permanently-failing extraction look complete; `ch1_011`'s repair loop
existed to compensate. Now fixed at source by extracting only
`conjunto_de_datos/*.csv`.

**Archives overlap.** 91 member paths are shipped by more than one `.tar.zst`
(catalogue and dictionary files), 60 of them with differing sizes, so their
on-disk content depends on extraction order. No microdata is affected and no
pipeline step reads them; the verifier excludes them rather than pretending they
certify anything.

**Fault-injection tested.** Deleting an extracted DBF and silently truncating a
panel are both detected and repaired, and `ch1_020` fails specifically on each.
A verifier that only passes is worthless.

### Decisions taken

- Manifests are **derived, machine-local and gitignored**, so a fresh clone
  correctly rebuilds everything on first run.
- Force flags rather than script edits: `ORPHAN_FORCE_DOWNLOAD`,
  `ORPHAN_FORCE_EXTRACT`, `ORPHAN_FORCE_PANELS`. The manifests certify that files
  are unchanged *since they were built*, not that the code that built them is
  unchanged — so editing preprocessing requires the force flag.
- **Never run two pipeline scripts concurrently.** Running `ch1_010` while
  `ch1_012` was reading the same CSV crashed R with a bus error mid-rebuild.

## 3. Chapter 2 — the grouping was wrong twice over

### The contiguity graph

`sf::st_touches()` returned 4,086 edges, mean 1.66 neighbours, and **655 of 2,457
polygons with no neighbour at all**. A planar contiguity graph averages 5–6.
GADM's polygons overlap by numerical slivers rather than sharing exact
boundaries, so every boundary-based construction fails identically —
`st_touches`, `st_relate` queen, `spdep::poly2nb`, s2 on or off. Those 655
orphans fell through to a nearest-centroid fallback and were welded to whatever
polygon was closest by centroid, adjacent or not.

`build_contiguity()` now uses `st_intersects` on geometry projected to EPSG:6372
with an explicit 10 m snap: **mean 5.84, zero orphans, one connected component** —
and it asserts on those numbers, which is the guard that would have caught the
original defect. A buffer in *degrees* was deliberately not used: 1e-7 and 1e-3
degrees give the identical edge count, so its value cannot be reviewed.

### The stopping rule

The merge loop froze unit populations at the start of each round, so a unit kept
absorbing neighbours long after it had cleared the threshold.

| at a 50,000 threshold | before | after |
|---|---|---|
| units | 552 | **777** |
| median unit population | 136,566 | 90,110 |
| median *merged* unit population | — | 79,235 |
| largest unit | **153 municipalities** | 55 |
| centroid fallbacks | many | 0 |

Now a greedy one-merge-at-a-time loop with the population updated immediately.
Verified against a hand-computed 3×3 synthetic grid, plus population conservation
and invariance to input row order (previously the result depended on GADM's row
order, so re-downloading the geometry could have silently changed every unit).

### Two further defects

- **21 post-GADM-v4.1 INEGI municipalities** had no polygon and collapsed into a
  spurious 778th `NA` unit across every grouped artefact. Now dropped with a
  reported count and asserted in `ch2_060`.
- **`ch3_040`'s deprivation regression had zero usable cases** —
  `poverty_index = first(mpi)` over a frame starting in 2005 while `mpi` covers
  2010–2020, so it was `NA` for all 777 units. Fixed, it returns correlation
  −0.236 (p = 2.7e-11): more deprived units have longer registration delays, the
  direction Chapter 3 argues for but was not measuring.

## 4. Chapter 2 — the cleanup

Done in four groups. Groups A and B were required to change nothing and were
verified by **25/25 data artefacts staying byte-identical**; C and D change
figures deliberately.

**A — one writer per artefact.** Five names were written by both `ch1_015` and
`ch2_060` with different content, and three artefacts were written twice as
byte-identical copies. Two live consequences: `ch5_020` was **broken in the
documented run order** (it left-joins `group_id` onto `geo_info.RDS` believing the
column is absent; after a full run it isn't), and `ch2_050` was plotting 12
municipalities with `capital = NA` as a spurious factor level. The
`ch2_010 ↔ geo_info` circularity is gone — it was reading a file `ch2_060`
rewrites *filtered by the grouping ch2_010 itself produces*.

**B — dead code and runtime.** `ch2_040`: **~7 min → 58 s** (a 34-iteration figure
loop recomputed the same 8.4 s standardisation every time). `ch2_050` had three
print-only per-year loops that saved nothing — one with a shadowing bug making its
filter a no-op — plus an MPI block guarded on a file `ch1_010` never downloads
whose contents were unused anyway. Nine stray `print()` calls had grown
`Rplots.pdf` to **136 MB**.

**C — figure correctness.** The delay figures counted **panel rows, not events**,
so a cell with 1 death weighed the same as one with 5,000; long-delay
registrations are spread across many tiny cells, so their share was
systematically inflated. This is the mechanical reason the thesis contradicts
itself between Figure 4 and Figure 5. A copy-pasted filter was deleting **real
female deaths at 65–79** from the mortality age figures. One figure was the
fertility series saved under a mortality name. `plot_std_rate()` accepted a
`y_lim` and silently ignored it.

**D — one rate definition.** `compute_std_rate()` was a third standardisation
scheme, self-flagged as an unverified reconstruction, which collapsed **sex** as
well as age — so for fertility it summed the mother's and father's row of every
birth. It also skipped the NA guard and built its reference population over a
frame still multiplied by `year_reg`. Retired to `old-R/`; all call sites now use
the reviewed helpers.

**Correction worth recording:** removing the fertility double count does *not*
halve the rates. The old denominator also spanned both sexes, so the error
largely self-cancelled — the 2020 median moves 0.04849 → 0.05733 (+18%). The old
figure was a rate per head of the combined mother+father population; the new one
is births per woman. The level barely moves; the meaning is what changed.

**The figures were never reproducible.** `Polychrome::createPalette` is stochastic
and was called unseeded in six places, so colours changed on every run — verified
by running an unmodified script twice. Now seeded.

## 5. The threshold question

Thesis Section 2.4 says 50,000 was chosen to minimise the CV of age-standardised
death rates over a 10k–100k sweep. **That sweep never existed.**
`ch2_020_grouping_diagnostics.R` implements it and widens it, because CV alone
cannot arbitrate: aggregating always reduces noise, so a criterion that only
rewards low noise is minimised by merging the whole country into one unit.

| threshold | units | CV death rate | zero death cells | between-unit variance retained | pop. reclassified GM |
|---:|---:|---:|---:|---:|---:|
| 0 (municipalities) | 2457 | 0.224 | 45.3% | 1.000 | 0 |
| 20,000 | 1269 | 0.166 | 22.1% | 0.924 | 3.8% |
| 30,000 | 1043 | 0.162 | 19.1% | 0.891 | 5.5% |
| **50,000** | **777** | 0.157 | 16.4% | 0.841 | 8.6% |
| 100,000 | 513 | 0.155 | 14.8% | 0.777 | 13.6% |

**Section 2.4's stated criterion cannot have produced 50,000** — the CV falls
monotonically to 100,000, so "minimise the CV" selects the largest threshold in
the sweep. 65% of the available reduction lands by 10,000, 85% by 20,000, 90% by
30,000, then it is flat, while the heterogeneity cost keeps rising close to
linearly with no saturation.

A second finding: over an 11-year window, Poisson sampling noise is only ~3% of
between-municipality variance **even ungrouped**. The noise problem is a
*single-year* problem, which changes how much aggregation the analysis needs.

**The threshold is left at 50,000.** Where to sit on the trade-off is a
statistical decision for the researchers; `ch2_010` already writes a 30,000
grouping alongside it, so switching is a one-line change.

---

## 6. Open questions — for Mateusz and Oliver

1. **Unknown parental age (`99`).** 2.12% of births in 2018 rising to **11.69% in
   2024** (5.5×), silently dropped by an age filter never intended as a missing-data
   rule. Biases fertility down by a growing factor, so part of the measured recent
   decline is a coding artefact — in exactly the window Chapter 4 attributes to
   registration delay. Not missing-at-random. See the memo; nothing implemented.
2. **8.28% of all deaths have no denominator.** The 80–84 band exists in the
   deaths panel but `load_population(keep_child = FALSE)` loads no population for
   it. One word (`keep_old = TRUE`) recovers 926,155 deaths and changes every rate
   in `ch2_040`.
3. **Fertility effectively covers 1990–2023, not 1985–2024.** The births filter
   spans 1985–2024 but population covers 1990–2023, so 14.94% of births (almost
   all 1985–89 and 2024) are dropped. Inside 1990–2023 the loss is 0.00%.
4. **The deaths panel starts in 1900.** 6,387 rows of pre-1985 deaths reach
   `ch3_040`/`ch3_050` as 80–90-year registration delays. Current behaviour
   preserved; a `year_min` argument makes changing it a one-word edit.
5. **The both-sex weight normalisation.** Per-sex standardised values carry only
   their share of the weight mass (~46%/54%), so they are not per-woman or per-man
   rates. Whether to normalise within sex — changing every published level — is
   definitional, and the thesis defines no standardisation method at all.
6. **`ch2_060`'s GM classification.** The IMM intervals overlap and `case_when` is
   first-match-wins, so every `min_value` condition is inert and the effective cut
   points are the per-class maxima — a rule that is undocumented and is not
   CONAPO's. It also pools three IMM vintages whose IMN scales are not comparable.
7. **~60 Chapter 3–5 findings** remain unreviewed in
   `docs/audit/thesis_vs_code_backlog.md`, several of which change headline
   numbers (parental deaths at 60–79 dropped; the sex split omits the
   double-orphan term; the bootstrap resamples a different model than the point
   estimate; the nowcast's delay-distribution method is unimplemented).

## 7. How to re-run

```bash
pixi run Rscript scripts-R/ch1_020_verify_inputs.R   # seconds — run this instead of ch1_010/011/012
pixi run Rscript scripts-R/ch1_015_bootstrap_from_processed.R
pixi run Rscript scripts-R/ch2_010_group_mun.R
pixi run Rscript scripts-R/ch2_020_grouping_diagnostics.R
pixi run Rscript scripts-R/ch2_040_clean_mort.R
pixi run Rscript scripts-R/ch2_050_clean_fert.R
pixi run Rscript scripts-R/ch2_060_prepare_datasets.R
```

`notebooks/ch1_ch2_review.ipynb` shows the current state; regenerate its inputs
with `pixi run Rscript notebooks/export_ch1_ch2.R`.
