# daoh 0.2.6

## Changes (default units)

* DAOH is now reported in **days by default** across the package, for a single
  consistent and interpretable unit. `bland_altman_daoh()`, `plot_daoh_dist()`,
  `daoh_reclassify()`, `daoh_reclassify_centile()`, and `daoh_icc()` now default
  to `use_pc = FALSE` (days); pass `use_pc = TRUE` for percentage of the period.
* `daoh_summary()` gains a `use_pc` argument (default `FALSE`, days).
* **Behaviour change:** code relying on the previous percentage defaults must
  now pass `use_pc = TRUE`. For `daoh_icc()` and `daoh_reclassify()` the result
  is unchanged (the statistic is invariant to the per-period rescaling); for
  `bland_altman_daoh()`, `plot_daoh_dist()`, and `daoh_summary()` the reported
  values switch from percentage to days.

---

# daoh 0.2.5

## Features

* `plot_daoh_ba()` gains a `show_loa` argument. When `TRUE`, the mean-bias
  and 95% limits-of-agreement values are printed directly on the plot beside
  their reference lines (in addition to the caption). Defaults to `FALSE`, so
  existing plots are unchanged.

---

# daoh 0.2.4

## Changes

* `plot_daoh_dist()` now labels the y-axis `"Count"` (the absolute number of
  observations per bin) rather than the ambiguous `"Frequency"`, and gains a
  `ylab` argument to override it.

---

# daoh 0.2.3

## Features

* `plot_daoh_dist()` gains a `use_pc` argument: set `use_pc = FALSE` to plot
  DAOH in days rather than as a percentage of the period (axis label follows).
* `bland_altman_daoh()` now records `units` (`"%"` or `"days"`) in its result,
  and `plot_daoh_ba()` labels its axes and caption from that unit, so a
  Bland-Altman computed with `use_pc = FALSE` is plotted in days. Results from
  earlier versions (without `units`) are still treated as percentages.

---

# daoh 0.2.2

## Performance

* `bland_altman_daoh()`, `daoh_reclassify()`, `daoh_reclassify_centile()`,
  and `daoh_icc()` now join their inputs with `data.table` instead of base
  `merge()`. On multi-million-row inputs each call is roughly an order of
  magnitude faster; results are unchanged (row order within the join may
  differ, which the computed statistics do not depend on).

---

# daoh 0.2.1

## Bug fixes

* **Timezone handling for `POSIXct` inputs.** Two related defects affected the
  `exact` method (and any `POSIXct` input) on machines whose local timezone is
  not UTC:
  - `as.Date()` on `POSIXct` converts via UTC, so local times earlier than the
    UTC offset (e.g. mornings in New Zealand) were assigned to the *previous*
    calendar day. This shifted index dates and admission/discharge dates.
  - `hospital_time()` measured `exact` intervals from the origin at 00:00 UTC,
    offsetting every value by the local UTC offset (about half a day in NZ)
    relative to the whole-day numbers used for the index-date period windows,
    which misaligned all interval clipping at period boundaries.

  Both now use the input's local calendar date (plus local clock-time fraction
  for `exact`). Results from `Date` inputs are unchanged. Results for
  `POSIXct` inputs in non-UTC timezones will change — they were previously
  wrong.

## Tests

* Corrected two test expectations: `calc_daoh()` returns one row per index
  date (with `daoh = period`) when there are no admissions, and the
  nights-vs-days systematic difference equals the *days* episode count
  (same-day stays contribute zero nights, so the nights method does not count
  them as episodes).
* Added a regression test for local-timezone handling of `POSIXct` inputs.

---

# daoh 0.2.0

## Performance

* `calc_daoh()` now uses a fully vectorised `data.table` pipeline instead of
  an R-level loop over patients. The key changes:
  - `foverlaps()` matches admissions to index periods in a single C-level pass,
    correctly handling patients with multiple index dates.
  - Interval merging uses a one-pass cumulative-max sweep (`ave()`) rather than
    a sequential R loop per patient, giving O(n log n) overall complexity.
  - Dead time and DAOH are computed by vectorised arithmetic over all patients
    simultaneously.
  - All column assignments use `data.table::set()` (no `:=`), which avoids
    `data.table`'s `cedta()` namespace check and works correctly under both
    `devtools::load_all()` and a standard package install.
* `data.table (>= 1.14.0)` added to `Imports`.

## Bug fixes

* Fixed a latent issue where `min(dod)` on an empty group (all patients
  alive) could emit a spurious "no non-missing arguments" warning.

---

# daoh 0.1.0

* Initial CRAN release.
* `calc_daoh()`: calculates DAOH for one or more patients given a
  `data.frame` of admissions and a `data.frame` of index dates.
  Supports three hospital-time algorithms (`nights`, `days`, `exact`) and
  three death-handling methods (`midday`, `midnight`, `zero`).
  A configurable gap tolerance (default 12 h) merges near-adjacent admissions
  before summing hospital time.
* `hospital_time()`: converts admission/discharge date pairs to numeric
  intervals under the chosen algorithm.
* `dead_time()`: calculates dead days within a follow-up period.
* `merge_intervals()`: merges overlapping or near-adjacent intervals with a
  configurable gap tolerance. Exported for direct use.
* `daoh_summary()`: summary statistics (mean, SD, percentiles) across a list
  of `calc_daoh()` results.
* `bland_altman_daoh()` / `plot_daoh_ba()`: Bland–Altman agreement analysis
  and plot comparing two sets of DAOH values.
* `daoh_icc()`: intraclass correlation coefficient between two DAOH vectors.
* `daoh_reclassify()` / `daoh_reclassify_centile()` / `plot_daoh_reclassify()`:
  centile-based reclassification analysis and plot.
* `plot_daoh_dist()`: distribution plot for a single set of DAOH values.
* `load_example()`: loads the bundled example dataset.
