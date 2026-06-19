#' Convert admission/discharge date pairs to numeric time intervals
#'
#' Applies one of the three DAOH hospital-time algorithms to convert
#' admission and discharge dates (or datetimes) into numeric time intervals
#' expressed in fractional days from a reference origin.
#'
#' @section Mathematical definitions:
#'
#' Let \eqn{a_i} be the admission date and \eqn{d_i} the discharge date for
#' event \eqn{i}, expressed as calendar dates (integers at midnight).
#'
#' \strong{Nights algorithm}
#' \deqn{h_i = d_i - a_i}
#' Equivalent to assuming both admission and discharge occur at 12:00 (noon).
#' A same-day admission contributes 0 nights. This matches the conventional
#' hospital "length of stay" metric.
#'
#' \strong{Days algorithm}
#' \deqn{h_i = (d_i + 1) - a_i}
#' Equivalent to assuming admission at 00:00 and discharge at 24:00 of the
#' respective dates. A same-day admission contributes 1 day. For any
#' admission, \eqn{h_i^{\text{days}} = h_i^{\text{nights}} + 1}, so the
#' total difference across \eqn{N} merged episodes equals \eqn{N} days:
#' \deqn{H^{\text{days}} - H^{\text{nights}} \approx N_{\text{episodes}}}
#'
#' \strong{Exact algorithm}
#' \deqn{h_i = t_i^{\text{discharge}} - t_i^{\text{admission}}}
#' Uses recorded timestamps directly (in fractional days). Partial days are
#' included.
#'
#' @param admission_dates Date or POSIXct vector of admission dates/times.
#' @param discharge_dates Date or POSIXct vector of discharge dates/times.
#' @param method Character string: `"nights"`, `"days"`, or `"exact"`.
#' @param origin Date or POSIXct used as numeric zero. Defaults to
#'   `"1970-01-01"`. All returned values are days since origin.
#'
#' @return A data.frame with columns `start` (numeric, days since origin) and
#'   `end` (numeric, days since origin) representing the hospital time
#'   interval under the chosen algorithm.
#'
#' @examples
#' # Same-day admission: nights=0, days=1
#' hospital_time(
#'   admission_dates = as.Date("2020-03-01"),
#'   discharge_dates = as.Date("2020-03-01"),
#'   method = "nights"
#' )
#' hospital_time(
#'   admission_dates = as.Date("2020-03-01"),
#'   discharge_dates = as.Date("2020-03-01"),
#'   method = "days"
#' )
#'
#' # Two-night stay
#' hospital_time(
#'   admission_dates = as.Date("2020-03-01"),
#'   discharge_dates = as.Date("2020-03-03"),
#'   method = "nights"   # 2 nights
#' )
#'
#' @export
hospital_time <- function(admission_dates, discharge_dates,
                          method = c("nights", "days", "exact"),
                          origin = as.Date("1970-01-01")) {
  method <- match.arg(method)

  # Convert to numeric (days since origin)
  to_num <- function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
      as.numeric(as.Date(x) - as.Date(origin))
    } else {
      as.numeric(as.Date(x) - as.Date(origin))
    }
  }
  to_num_exact <- function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
      as.numeric(difftime(x, as.POSIXct(origin), units = "days"))
    } else {
      as.numeric(as.Date(x) - as.Date(origin))
    }
  }

  if (method == "nights") {
    # Admission = noon, discharge = noon of respective dates
    starts <- to_num(admission_dates) + 0.5
    ends   <- to_num(discharge_dates) + 0.5
  } else if (method == "days") {
    # Admission = start of day (00:00), discharge = end of day (24:00 = next 00:00)
    starts <- to_num(admission_dates)
    ends   <- to_num(discharge_dates) + 1.0
  } else {  # exact
    starts <- to_num_exact(admission_dates)
    ends   <- to_num_exact(discharge_dates)
  }

  # Ensure end >= start
  ends <- pmax(ends, starts)

  data.frame(start = starts, end = ends)
}


#' Calculate dead time within a DAOH period
#'
#' @section Mathematical definitions:
#'
#' Let \eqn{t_0} be the index date (numeric, days) and \eqn{T} the period
#' length in days, so the period window is \eqn{[t_0,\; t_0 + T]}.
#'
#' \strong{Midday death} (default):
#' \deqn{D = [t_{\text{DOD}} + 0.5,\; t_0 + T]}
#' i.e., the patient is assumed to die at noon on the date of death.
#'
#' \strong{Midnight death} (conservative):
#' \deqn{D = [t_{\text{DOD}},\; t_0 + T]}
#' The patient is assumed to die at midnight (start of the day of death),
#' maximising dead time.
#'
#' \strong{Death = 0}:
#' If the patient died at any time within \eqn{[t_0, t_0 + T]}, DAOH is
#' set to 0 directly (handled in [calc_daoh()], not here).
#'
#' If \eqn{t_{\text{DOD}}} is outside the period, dead time is 0.
#'
#' @param dod Date or `NA`. Date of death.
#' @param period_start Numeric. Start of the DAOH period (days since origin).
#' @param period_end   Numeric. End of the DAOH period (days since origin).
#' @param death_method Character: `"midday"`, `"midnight"`, or `"zero"`.
#'   For `"zero"`, this function returns `NA` as a signal that the caller
#'   should set DAOH to 0.
#' @param origin Date used as numeric zero.
#'
#' @return Numeric: dead time in days within the period. `NA` if
#'   `death_method = "zero"` and death occurred in the period (caller should
#'   set DAOH to 0).
#'
#' @keywords internal
dead_time <- function(dod, period_start, period_end,
                      death_method = c("midday", "midnight", "zero"),
                      origin = as.Date("1970-01-01")) {
  death_method <- match.arg(death_method)

  if (is.na(dod)) return(0)

  dod_num <- as.numeric(as.Date(dod) - as.Date(origin))

  # Death outside the period -> no dead time
  if (dod_num > period_end || dod_num < period_start) return(0)

  if (death_method == "zero") {
    return(NA_real_)  # Signal: caller sets DAOH = 0
  }

  death_offset <- if (death_method == "midday") 0.5 else 0.0
  death_time   <- dod_num + death_offset

  # Dead interval: [death_time, period_end], clipped to period
  dead_start <- max(death_time, period_start)
  dead_end   <- period_end
  max(0, dead_end - dead_start)
}
