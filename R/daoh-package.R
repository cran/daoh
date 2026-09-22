#' daoh: Days Alive and Out of Hospital (DAOH) Calculation
#'
#' Calculates Days Alive and Out of Hospital (DAOH) from administrative
#' admission/discharge/mortality data using three algorithms (nights, days,
#' exact) and three death-handling approaches (midday, midnight, zero).
#' Includes tools for comparing methods via Bland-Altman analysis, intraclass
#' correlation, and reclassification statistics, plus plotting functions.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats quantile sd ave
#' @importFrom utils read.csv
#' @import data.table
NULL

## Required for data.table's cedta() check when the package is loaded via
## devtools::load_all() rather than a proper install (where import(data.table)
## in NAMESPACE would set this automatically).
.datatable.aware <- TRUE

## Suppress R CMD check NOTEs for ggplot2 non-standard evaluation variables
utils::globalVariables(c("average", "difference", "daohPC",
                          "Group_A", "Group_B", "Count"))

## Suppress R CMD check NOTEs for data.table non-standard evaluation variables
## ("." is data.table's list() alias used in j and by expressions)
utils::globalVariables(c(
  ".",
  "row_id", "idx_num", "period_end_num", "ev_start", "ev_end",
  "cs", "ce", "cum_max_ce", "prev_max_ce", "new_grp", "grp", "grp_dih",
  "dih", "n_episodes", "dd", "daoh", "daohPC", "dod",
  "patientID", "i.patientID"
))
