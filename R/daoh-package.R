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

#' @importFrom stats quantile sd
#' @importFrom utils read.csv
NULL

## Suppress R CMD check NOTEs for ggplot2 non-standard evaluation variables
utils::globalVariables(c("average", "difference", "daohPC",
                          "Group_A", "Group_B", "Count"))
