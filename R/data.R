#' Synthetic patient example: single day-stay admission
#'
#' A minimal example designed to illustrate the difference between the
#' 'nights' and 'days' algorithms for a same-day (day-stay) admission.
#' Under 'nights' the patient contributes 0 nights (DAOH = 30).
#' Under 'days' the patient contributes 1 day (DAOH = 29).
#'
#' @format A list with two data.frames:
#' \describe{
#'   \item{`events`}{One row: day-stay admission on the index date, no death.}
#'   \item{`index_dates`}{One row: the index date for this patient.}
#' }
#' @examples
#' data(example_daystay)
#' calc_daoh(example_daystay$events, example_daystay$index_dates,
#'           period = 30, method = "nights")
#' calc_daoh(example_daystay$events, example_daystay$index_dates,
#'           period = 30, method = "days")
"example_daystay"


#' Synthetic patient example: multiple admissions and death
#'
#' A patient with four hospital admissions who dies 8 days after the last 
#' discharge, within a 30-day follow-up period. Illustrates all seven DAOH variants.
#'
#' @format A list with two data.frames:
#' \describe{
#'   \item{`events`}{Four rows of admissions plus date of death.}
#'   \item{`index_dates`}{One index date (day of first admission).}
#' }
#' @examples
#' data(example_death)
#' # All seven variants
#' for (meth in c("nights", "days", "exact")) {
#'   for (dm in c("midday", "midnight", "zero")) {
#'     res <- calc_daoh(example_death$events, example_death$index_dates,
#'                      period = 30, method = meth, death_method = dm)
#'     cat(meth, dm, "DAOH =", round(res$daoh, 2), "\n")
#'   }
#' }
"example_death"


#' Synthetic population: 500 patients, mixed admission patterns
#'
#' A larger synthetic dataset suitable for demonstrating summary statistics,
#' Bland-Altman analysis, and reclassification. Patients have between 1 and
#' 5 admissions over a 365-day period, with a 5% mortality rate.
#'
#' @format A list with two data.frames:
#' \describe{
#'   \item{`events`}{Hospital events with columns patientID, admission,
#'     discharge, dod.}
#'   \item{`index_dates`}{One index date per patient.}
#' }
#' @examples
#' data(example_population)
#' res_n <- calc_daoh(example_population$events, example_population$index_dates,
#'                    period = 90, method = "nights")
#' res_d <- calc_daoh(example_population$events, example_population$index_dates,
#'                    period = 90, method = "days")
#' bland_altman_daoh(res_n, res_d)
"example_population"
