# daoh

**R package for calculating Days Alive and Out of Hospital (DAOH)**

Supports all three hospital-time algorithms (nights, days,
exact) and all three death-handling approaches (midday, midnight, zero), with
tools for comparison, and visualisation.

## Installation

```r
# From source (once on CRAN or GitHub)
# install.packages("daoh")

# From local source
devtools::install("path/to/daoh_package/daoh")
```

## Quick start

```r
library(daoh)

# Load the built-in day-stay example
ex <- load_example("daystay")

# Nights algorithm: same-day admission = 0 nights → DAOH = 30
calc_daoh(ex$events, ex$index_dates, period = 30, method = "nights")

# Days algorithm: same-day admission = 1 day → DAOH = 29
calc_daoh(ex$events, ex$index_dates, period = 30, method = "days")
```

## Key functions

| Function                  | Purpose                               |
| ------------------------- | ------------------------------------- |
| `calc_daoh()`             | Main DAOH calculation                 |
| `load_example()`          | Load synthetic example datasets       |
| `bland_altman_daoh()`     | Bland–Altman agreement statistics     |
| `daoh_icc()`              | Intraclass correlation across methods |
| `daoh_reclassify()`       | Quartile reclassification analysis    |
| `plot_daoh_dist()`        | Distribution plot (Figure 2 style)    |
| `plot_daoh_ba()`          | Bland–Altman plot                     |
| `plot_daoh_reclassify()`  | Reclassification heatmap              |

## Examples

Three synthetic datasets are included:

- **`"daystay"`** — single day-stay illustrating the nights vs days difference
- **`"death"`** — four admissions + in-period death (Figure 1 worked example)
- **`"population"`** — 500-patient cohort for method comparison

See `vignette("getting_started", package = "daoh")` for a full walkthrough.

## Citation
DOI: 10.5281/zenodo.20671491
