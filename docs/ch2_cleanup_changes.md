# Chapter 2 cleanup — what changed and why

Companion to `docs/ch2_grouping_diagnostics.md`. Every number below was measured
on the pipeline, before and after. Work was done in four groups; the first two
were required to change nothing, the last two deliberately change figures.

## Verification summary

| | data artefacts | ch2 figures | ch3 figures |
|---|---|---|---|
| after Groups A+B | **25/25 identical** | 53 identical, 6 changed | 47 identical, 11 changed |
| after Groups C+D | **25/25 identical** | 14 identical, 44 changed, 1 removed | 47 identical, 11 changed |

The data artefacts never moved. `ch2_020_grouping_diagnostics.RDS` is also
identical throughout — it was already using the reviewed code path, so any change
there would have meant the cleanup broke something.

---

## Group A — one writer per artefact name

`population.RDS`, `geo_info.RDS`, `births.RDS`, `deaths.RDS` and `marg_index.RDS`
were each written by both `ch1_015` and `ch2_060` with **different content**, so a
file's meaning depended on how far the pipeline had run. `marg_index.RDS` was a
municipal IMM table after ch1_015 and a group-level interpolated index after
ch2_060. Three further artefacts were written twice as byte-identical copies.

| old | now |
|---|---|
| `population.RDS` | `population_mun_adult.RDS` (ch1_015, 15–79) · `population_mun.RDS` (ch2_060, 0–79) |
| `geo_info.RDS` | `geo_info_mun.RDS` (ch1_015) · `geo_info_mun_grouped.RDS` (ch2_060) |
| `marg_index.RDS` | `marg_index_mun.RDS` (ch1_015) · folded into `index_grouped_mun.RDS` (ch2_060) |
| `births.RDS` / `deaths.RDS` | ch1_015 only; ch2_060 now reads them |
| `mpi_grouped_mun.RDS` | deleted — `mpi_imputed.RDS` is written directly |

**Two real consequences, not just tidiness:**

- **`ch5_020` was broken in the documented run order.** It reads `geo_info.RDS`
  and left-joins `group_id` onto it, commented "geo_info has no group_id". After a
  full run the file *did* carry `group_id`, so the join produced
  `group_id.x`/`group_id.y` and a later `select(mun, group_id)` errored. Fixed by
  pointing it at the municipal vintage.
- **`ch2_050` was plotting 12 municipalities wrong.** It read whichever
  `geo_info.RDS` was on disk; after a full pipeline that was ch2_060's *filtered*
  version, so 12 municipalities had `capital = NA` and formed a spurious factor
  level in the fertility-vs-MPI scatter. Now zero.

The **ch2_010 ↔ geo_info circularity** is also gone. ch2_010 needs only
`{mun, mun_name, state_name}` as a name crosswalk, but was reading a file ch2_060
rewrites *filtered by the grouping ch2_010 itself produces* — so the municipality
universe shrank on every re-run.

**Open question left for the researchers:** ch1_015 filtered deaths to ≥1985,
ch2_060 applied no floor, and ch2_060's version is what survived — so the panel
carries 6,387 rows of deaths dated as early as **1900**, reaching `ch3_040` and
`ch3_050` as 80–90-year registration delays. Current behaviour is preserved and a
`year_min` argument added; changing it is a one-word edit.

## Group B — dead code and runtime

- **`ch2_040`: ~7 min → 58 s.** The 34-year figure loop recomputed
  `compute_std_rate(mort)` on the full table every iteration (8.4 s per call,
  ~283 s of a ~300 s loop) for a result that does not depend on the loop variable.
- **`ch2_050` had three print-only per-year loops**, none of which saved anything.
  One had a shadowing bug: `for (year in years)` shadowed the `year` *column*, so
  `filter(year == year)` was TRUE for every row and each iteration plotted all 34
  years under a single-year title.
- **`ch2_050`'s MPI block was entirely inert** — guarded on a CONAPO file
  `ch1_010` never downloads, and nothing inside it was used even when present.
- Two `grid.arrange()` calls on permanently-empty lists emitted blank pages; nine
  stray `print()` calls were appending to a **136 MB `Rplots.pdf`** at the repo root.
- `ch2_060`'s "skipping index_marg" message was **false** — `index_marg` is built,
  written, and read by `ch3_020`.

## Group C — figure correctness

**The delay figures were measuring the wrong quantity.** `summarise(count = n())`
counts *panel rows*, not events, so a municipality-sex-age cell holding 1 death
weighed the same as one holding 5,000. Long-delay registrations are scattered
across many tiny cells, so this systematically inflated their apparent share. Now
sums the counts. This is the same defect logged for `ch3_040`/`ch3_050` as backlog
item 3.7, and it is the mechanical explanation for the thesis contradicting itself
between Figure 4 (long delays dominate) and Figure 5 (CDF at d=1 ≈ 0.87–0.96).

**A copy-pasted filter deleted real deaths.** `ch2_040` carried
`filter(!(sex == "female" & age %in% c("65-69","70-74","75-79")))`, which is a
reproductive-age cap in the *fertility* script. In the mortality script it removed
real female deaths at 65–79, truncating the female curve at 64 while the male ran
to 79 — in a figure whose purpose is the male/female comparison.

**`ch2_050_std_mort_rate_age.pdf` was the fertility series mislabelled as
mortality** (same data as `ch2_050_std_fert_rate_age.pdf`, different title and axis
label). Removed; the real mortality-by-age figure is `ch2_040_std_rate_age_2023.pdf`.

Two titles contradicted their own filters ("2012 to 2023" and "2017 to 2023" over
2005:2023 data). `plot_std_rate()` accepted a `y_lim` argument and **silently
ignored it**; three call sites passed `c(0, 0.15)` believing the axes were clipped.
Now honoured via `coord_cartesian`, which clips the view without dropping points
from the `geom_smooth` fits.

**Silent data loss is now reported at runtime:**

```
ch2_040: drop_na() removed 1,300,908 rows and   989,327 deaths ( 8.84% of all deaths)
ch2_050: drop_na() removed   242,138 rows and 26,134,692 births (14.94% of all births)
```

- **Mortality, 8.84%.** 926,155 deaths (8.28%) are lost because the **80–84 band
  exists in the deaths panel but has no denominator** — `load_population()` is
  called with `keep_old = FALSE`. This is a one-word fix
  (`keep_old = TRUE`) that would change every rate in the script, so it is left
  for the researchers. The remaining 63,570 are unknown-age deaths, which genuinely
  cannot be age-standardised.
- **Fertility, 14.94%.** Almost all of it is **1985–1989 (13.85%) and 2024 (1.09%)**:
  the births filter spans 1985–2024 but the population table covers 1990–2023, so
  the wider window is inert and `fert.RDS` effectively covers 1990–2023. Within
  1990–2023 the loss is 4,439 births (0.00%).

**The delay figures were never reproducible.** `Polychrome::createPalette` is
stochastic and was called unseeded in six places, so colours changed on every run —
verified by running an unmodified script twice and diffing. Now seeded.

## Group D — one rate definition

`compute_std_rate()` was a **third** standardisation scheme, self-flagged in-file as
an unverified reconstruction. It grouped by `(mun, year)` only, collapsing **sex** as
well as age. It also skipped `.drop_unstandardisable()` and built its reference
population with `sum()` over a frame still multiplied by `year_reg`, distorting the
weights. All nine call sites now use the reviewed
`compute_std_mort_rate()` / `compute_std_fert_rate()` with `unit_col = "mun"`;
`compute_std_rate()` and `min_max_norm()` are retired to `old-R/`.

Measured effect on the 2020 median municipal rate:

| | old | new | ratio |
|---|---|---|---|
| fertility | 0.04849 | 0.05733 | **1.18** |
| mortality | 0.00724 | 0.00551 | **0.76** |

**Fertility — correcting an expectation.** Collapsing sex summed the mother row and
the father row of every birth, which sounds like a factor-of-two error. It is not,
because the *denominator* also spanned both sexes: the old figure was roughly
`(births_mothers + births_fathers) / population_both`, which partially
self-cancels. The old number was a rate per head of the combined mother+father
population — conflating two different exposure populations and not interpretable as
fertility. The new number is **births per woman**: mother rows only, weights summing
to 1 over the female bands, which is what `ch2_020` already did. The level moves by
about +18% at the median; the point is that it now has a defined meaning.

**Mortality.** The 0.76 ratio is mostly a change of *reference population*: the old
function standardised to a fixed year-2020 population, the reviewed helper to the
mean population over the years present. Both are legitimate direct standardisations;
the reviewed one is what Chapter 3 and `ch2_020` use, so the pipeline is now
internally consistent rather than carrying two standards side by side.

### Documented, deliberately not changed

- **The both-sex weight normalisation.** `.std_rate_by_unit()` normalises weights
  across both sexes but sums over ages within one sex, so a per-sex value carries
  only its share of the weight mass (~46% female / ~54% male). Summing sexes is
  correct for mortality; for fertility you must pass mother rows only. Whether the
  per-sex figures should instead be normalised *within* sex — making each a true
  per-woman / per-man rate and changing every published level — is a definitional
  choice. The thesis defines no standardisation method at all, so there is nothing
  to check it against.
- **`ch2_060`'s GM classification.** The IMM intervals overlap and `case_when` is
  first-match-wins, so every `min_value` condition is inert and the effective cut
  points are the per-class maxima — an undocumented rule that is not CONAPO's. It is
  also derived from three IMM vintages pooled, whose IMN scales are not comparable.
  Changing it changes GM assignment and therefore Chapter 3 figures.
- **The 30,000 grouping**, which nothing reads, is kept as the live alternative for
  the threshold debate.
