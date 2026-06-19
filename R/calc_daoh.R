#' Calculate Days Alive and Out of Hospital (DAOH)
#'
#' The main calculation function. For each patient-index-date pair, computes
#' DAOH using the specified hospital-time algorithm and death-handling approach.
#'
#' @section DAOH formula:
#'
#' \deqn{\text{DAOH} = \max\!\bigl(0,\; T - H - D\bigr)}
#'
#' where:
#' \describe{
#'   \item{\eqn{T}}{Period length in days (e.g., 90).}
#'   \item{\eqn{H}}{Total hospital time within the period (days), computed
#'     from merged, boundary-truncated intervals. See [hospital_time()].}
#'   \item{\eqn{D}}{Total dead time within the period (days), computed from
#'     date of death. See [dead_time()].}
#' }
#'
#' Overlapping hospital events are merged using a 12-hour gap tolerance before
#' summing: any two admissions separated by <= 12 hours are treated as a
#' single continuous episode. This removes double-counting and models the
#' clinical reality that rapid re-admissions represent continuous care.
#'
#' @section Algorithm differences:
#'
#' For \eqn{N} merged hospital episodes in the period, the systematic
#' difference between algorithms is:
#' \deqn{H^{\text{days}} - H^{\text{nights}} \approx N_{\text{episodes}}}
#' because each episode contributes one additional day under the days
#' algorithm (admission = 00:00, discharge = 24:00 vs. both at noon).
#' Therefore:
#' \deqn{\text{DAOH}^{\text{nights}} - \text{DAOH}^{\text{days}} \approx N_{\text{episodes}}}
#'
#' @param events `data.frame` with one row per hospital event and columns:
#'   \describe{
#'     \item{`patientID`}{Character or factor patient identifier.}
#'     \item{`admission`}{Date or POSIXct admission date/time.}
#'     \item{`discharge`}{Date or POSIXct discharge date/time.}
#'     \item{`dod`}{Date of death, or `NA` if alive (optional; if absent,
#'       all patients are assumed to have survived).}
#'   }
#' @param index_dates `data.frame` with columns `patientID` and `indexDate`
#'   (Date). Each row defines one DAOH observation window. A patient may
#'   have multiple index dates (e.g., multiple surgical episodes).
#' @param period Numeric. Follow-up period in days. Default 90.
#' @param method Character. Hospital-time algorithm: `"nights"` (default),
#'   `"days"`, or `"exact"`.
#' @param death_method Character. How to handle death: `"midday"` (default),
#'   `"midnight"`, or `"zero"` (sets DAOH = 0 for any death in period).
#' @param gap_hours Numeric. Gap tolerance in hours for merging adjacent
#'   admissions. Default 12.
#' @param origin Date. Numeric reference date. Default `"1970-01-01"`.
#'
#' @return A `data.frame` with one row per patient-index-date pair and columns:
#'   \describe{
#'     \item{`patientID`}{Patient identifier.}
#'     \item{`indexDate`}{Index date.}
#'     \item{`n_episodes`}{Number of merged hospital episodes in period.}
#'     \item{`dih`}{Days in hospital (numeric, using chosen algorithm).}
#'     \item{`dd`}{Days dead within the period (0 if survived or death=0).}
#'     \item{`daoh`}{DAOH in days.}
#'     \item{`daohPC`}{DAOH as a percentage of the period (0-100).}
#'   }
#'
#' @examples
#' # --- Example 1: Simple day-stay (highlights nights vs days difference) ---
#' events <- data.frame(
#'   patientID = "P1",
#'   admission = as.Date("2020-03-10"),
#'   discharge = as.Date("2020-03-10"),  # same-day admission
#'   dod       = NA
#' )
#' idx <- data.frame(patientID = "P1", indexDate = as.Date("2020-03-10"))
#'
#' calc_daoh(events, idx, period = 30, method = "nights")$daoh  # 30 days
#' calc_daoh(events, idx, period = 30, method = "days")$daoh    # 29 days
#'
#' # --- Example 2: Death handling ---
#' events2 <- data.frame(
#'   patientID = "P2",
#'   admission = as.Date("2020-03-10"),
#'   discharge = as.Date("2020-03-12"),
#'   dod       = as.Date("2020-03-20")   # died 8 days after discharge
#' )
#' idx2 <- data.frame(patientID = "P2", indexDate = as.Date("2020-03-10"))
#'
#' calc_daoh(events2, idx2, period = 30, method = "nights",
#'           death_method = "midday")$daoh   # credits pre-death days
#' calc_daoh(events2, idx2, period = 30, method = "nights",
#'           death_method = "zero")$daoh     # returns 0
#'
#' @export
calc_daoh <- function(events, index_dates,
                      period       = 90,
                      method       = c("nights", "days", "exact"),
                      death_method = c("midday", "midnight", "zero"),
                      gap_hours    = 12,
                      origin       = as.Date("1970-01-01")) {

  method       <- match.arg(method)
  death_method <- match.arg(death_method)
  gap_days     <- gap_hours / 24

  # Validate inputs
  stopifnot(is.data.frame(events), is.data.frame(index_dates))
  required_ev  <- c("patientID", "admission", "discharge")
  required_idx <- c("patientID", "indexDate")
  missing_ev   <- setdiff(required_ev,  names(events))
  missing_idx  <- setdiff(required_idx, names(index_dates))
  if (length(missing_ev))  stop("events is missing columns: ",  paste(missing_ev,  collapse=", "))
  if (length(missing_idx)) stop("index_dates is missing columns: ", paste(missing_idx, collapse=", "))
  if (!"dod" %in% names(events)) events$dod <- NA

  origin_date <- as.Date(origin)

  results <- vector("list", nrow(index_dates))

  for (i in seq_len(nrow(index_dates))) {
    pid       <- as.character(index_dates$patientID[i])
    idx_date  <- as.Date(index_dates$indexDate[i])
    idx_num   <- as.numeric(idx_date - origin_date)
    end_num   <- idx_num + period

    # All events for this patient
    pt_ev <- events[as.character(events$patientID) == pid, , drop = FALSE]

    dod_val <- if (nrow(pt_ev) > 0 && !all(is.na(pt_ev$dod))) {
      valid_dods <- pt_ev$dod[!is.na(pt_ev$dod)]
      if (length(valid_dods) > 0) as.Date(valid_dods[1]) else NA
    } else NA

    # Drop events with unparseable (NA) admission or discharge dates
    pt_ev <- pt_ev[!is.na(pt_ev$admission) & !is.na(pt_ev$discharge), ]

    # Compute hospital intervals within the period
    if (nrow(pt_ev) > 0) {
      intervals <- hospital_time(pt_ev$admission, pt_ev$discharge,
                                 method = method, origin = origin_date)
      merged    <- merge_intervals(intervals$start, intervals$end, gap = gap_days)
    } else {
      merged    <- data.frame(start = numeric(0), end = numeric(0))
    }

    dih        <- clip_and_sum(merged, idx_num, end_num)
    n_episodes <- sum(merged$start < end_num & merged$end > idx_num)

    # Dead time
    dd_val <- dead_time(dod_val, idx_num, end_num, death_method, origin_date)

    if (is.na(dd_val)) {
      # death_method == "zero" and patient died in period
      results[[i]] <- data.frame(
        patientID  = pid,
        indexDate  = idx_date,
        n_episodes = n_episodes,
        dih        = dih,
        dd         = as.numeric(end_num - pmax(idx_num, as.numeric(as.Date(dod_val) - origin_date))),
        daoh       = 0,
        daohPC     = 0
      )
    } else {
      daoh_val <- max(0, period - dih - dd_val)
      results[[i]] <- data.frame(
        patientID  = pid,
        indexDate  = idx_date,
        n_episodes = n_episodes,
        dih        = dih,
        dd         = dd_val,
        daoh       = daoh_val,
        daohPC     = 100 * daoh_val / period
      )
    }
  }

  if (length(results) == 0 || all(sapply(results, is.null))) {
    return(data.frame(patientID  = character(),
                      indexDate  = as.Date(character()),
                      n_episodes = integer(),
                      dih        = numeric(),
                      dd         = numeric(),
                      daoh       = numeric(),
                      daohPC     = numeric()))
  }
  do.call(rbind, results)
}
