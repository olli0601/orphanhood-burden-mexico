# Unknown parental age (INEGI code `99`) — problem and options

*Prepared for a decision by Mateusz and Oliver. Nothing has been changed in the code; the
current behaviour described below is what runs today.*

## What happens

INEGI codes unspecified parental age as `99`. `R/preprocess_fertility.R` keeps mothers aged
15–64 and fathers 15–84, so `99` falls outside the range and is silently discarded.

This was never a decision. The range comes from Elsa's original code
(`old-R/ch1_030_data_preprocessing_mort_fert.R:133-143`), chosen — per its own comment — to
match a US orphanhood paper. The thesis documents it only as an "age classification" step and
never mentions unspecified ages. **No count of the discarded births is reported anywhere.**

## Scale, and it is growing fast

| registration year | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 |
|---|---|---|---|---|---|---|---|
| unknown **mother's** age | **2.12%** | 2.22% | 3.81% | 4.96% | 6.33% | 9.21% | **11.69%** |
| unknown **father's** age | 11.5% | 11.8% | 12.6% | 14.1% | 15.5% | 18.4% | **20.4%** |

**The unknown-mother share is 5.5× what it was in 2018, and by 2024 more than one birth in
nine is discarded.** The 2023–24 acceleration is the steepest part of the series.

Verified against the panels: 2022 has 1,891,388 raw records, 1,764,347 in the kept age range,
and `fert_2022.RDS` holds 1,755,810 — the 119,786 unknown-age births are simply gone.

## Why it matters

The discarded births leave the **numerator**. The denominator is CONAPO's population
projection, which knows nothing about birth-certificate coding and is unaffected. So every
fertility rate is biased down by roughly the unknown share.

Because that share is now 5.5× its 2018 level, **part of the measured recent fertility decline is
an artefact of coding, not demography.** That falls in exactly the window Chapter 4's nowcast
attributes to registration delay, so there is a real risk of "correcting" a shortfall for the
wrong cause, and then double-counting it.

## Complication: it is not missing-at-random

Records with unknown mother's age are systematically badly completed:

| | mother's age = 99 | mother's age known |
|---|---|---|
| education specified | **58.4%** | 92.9% |
| activity specified | **67.6%** | 94.4% |

They are a different population of records, not a random sample of births. Any redistribution
that assumes they resemble well-completed records is making an assumption the data contradicts.

## Options

1. **Drop — status quo.** Assumes those births did not happen. Rates biased down by a factor
   of (1−u), with u rising over time, so the *trend* is distorted as well as the level.
2. **Redistribute proportionally within a stratum.** Assumes unknown-age mothers resemble
   known-age ones in that stratum. Raises rates by 1/(1−u): 2.2% in 2018, 6.8% in 2022, 13.2% in 2024.
   The stratum matters — *municipality × year* preserves local age profiles but is noisy in
   small municipalities; *state × year* is more stable but imposes the state profile on
   deprived municipalities, which is precisely where the coding gaps concentrate.
3. **Model-based imputation** conditional on covariates. More defensible under non-MAR,
   considerably heavier to build and to describe in the paper.
4. **Whichever is chosen: report the count.** It is currently invisible, and that alone is a
   reporting defect.

## What needs deciding

- **The stratum**, if redistributing.
- **Framing: correction or bounded sensitivity?** The non-MAR evidence argues for a
  sensitivity — publishing the drop and redistribute bounds — rather than a single adjusted
  series presented as the truth.

## Fathers are a separate question

`edad_padn = 99` is 15.5% of 2022 births (20.4% by 2024), but **70.3% of those carry no father information at
all** (`act_pad` and `escol_pad` both unspecified). The registration variable confirms it:

| who appeared at the registry | father's age known | father's age = 99 |
|---|---|---|
| both parents | **91.5%** | 23.2% |
| mother only | 5.8% | **60.8%** |

Most are **absent fathers, not unknown-aged ones**. Redistributing all of them would invent
roughly 206,000 fathers in 2022 alone. Only the ~30% with corroborating father data are
candidates for redistribution. INEGI's own data dictionary does not distinguish the two cases.

## Cost of acting

The fix lives in `R/preprocess_fertility.R` and needs a panel rebuild
(`ORPHAN_FORCE_PANELS=1`, ~11 minutes). Chapter 1 is now manifest-cached, so this is cheap and
safe to redo — but note the manifests certify that files are unchanged since they were built,
not that the code that built them is unchanged, so the force flag is required.

## Open issue: state fallback puts births into zero-population cells (deferred, 2026-10-26)

Found after `redistribute_unknown_age()` went in. **Not fixed; parked deliberately.**

- `fert.RDS` (municipal, `ch2_050`) has 9 rows with `fert_rate = Inf`, all in Oaxaca
  municipalities 20047 and 20488.
  - **5 are new:** fractional male births (0.013–0.225) in bands with no known-age father and
    `population = 0`.
  - **4 predate the change:** real known-age births in `population = 0` cells.
- **Cause:** the profile cell is mun × year × **year_reg** × sex. A late-registered unknown-age
  father is usually alone in his cell, so he skips his own municipality's profile and takes the
  state profile. That profile covers every band, including bands with zero population in tiny
  municipalities.
  - For example, 20488 has known-age fathers in 2004, 2005 and 2015, but each unknown-age father
    (registered 2010 or 2020) still got the Oaxaca profile.
- **Grouped panels are unaffected:** `fert_by_grouped_mun.RDS` has 0 `Inf` rates and 0
  zero-population rows (checked 2026-10-26). Chapter 3+ uses the grouped panels.
- **Options, for decision with Oliver:**
  1. Add a fallback level mun × year × sex, pooled over year_reg, before state. This fixes 4 of
     the 5 new rows; 20047/2021 has no known-age father at all.
  2. Drop year_reg from the profile key, but assign the redistributed births back to their own
     year_reg.
  3. Handle `population == 0` explicitly in `ch2_050` (drop, or `NA` rate). This is the only option
     that also covers the 4 older rows.
- **Not yet measured:** how much unknown-age mass goes through the state and national fallbacks
  across the full panel. The function's `message()` prints it on every run.
