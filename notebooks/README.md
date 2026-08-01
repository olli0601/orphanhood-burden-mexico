# notebooks/

One notebook, one export script. Both are **read-only with respect to the pipeline** — they
read what `scripts-R/` produced and write only into `input-data-processed/py-export/`
(gitignored, like the rest of `input-data-processed/`).

## Setup

The Python packages live in a separate pixi environment (`py`) with its own solve group, so it
cannot perturb the R environment. Run both from the repo root:

```bash
pixi install -e py
pixi run Rscript notebooks/export_ch1_ch2.R          # refresh the parquet inputs
pixi run -e py jupyter lab notebooks/ch1_ch2_review.ipynb
```

## Contents

| File | What it does |
|---|---|
| `export_ch1_ch2.R` | Reshapes the Chapter 1–2 artefacts to parquet: panel manifest, events by occurrence/registration year, municipalities and their units, the adjacency comparison, the threshold sweep, and the unknown-parental-age series |
| `ch1_ch2_review.ipynb` | The Chapter 1–2 review — what the pipeline does now, what was wrong before, and what is still open |

## Rules

- **Never re-implement a pipeline computation here.** The notebook reads results; it does not
  recompute them. The single exception is the adjacency comparison in `export_ch1_ch2.R`, which
  calls the pipeline's own `build_contiguity()` — the point is to show the fix is reproducible,
  not to reimplement it. If a variant needs evaluating, run it in R and export the result, or it
  will drift from the pipeline silently.
- **Nothing here writes into `input-data-processed/`** except the `py-export/` subdirectory.
- Narrative and decisions belong in `docs/`, not in notebook prose that will drift. Start at
  `docs/ch1_ch2_summary.md`.
- Re-run `export_ch1_ch2.R` after any pipeline change, or the notebook renders stale exports.

## What was removed

`ch1_inputs_verification.ipynb`, `ch2_grouping_vs_thesis.ipynb` and the three `export_*.R`
scripts that fed them were exploratory: they diagnosed the aggregation bugs against the thesis
and are now superseded. Their durable findings live in `docs/ch2_grouping_diagnostics.md`,
`docs/ch2_cleanup_changes.md` and `docs/audit/thesis_vs_code_backlog.md`; the fixes they
motivated are in `R/municipality_grouping.R` and `scripts-R/ch2_*`.
