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
#' @section Implementation:
#'
#' Internally uses \pkg{data.table} for a fully vectorised pipeline:
#' \enumerate{
#'   \item \code{foverlaps} matches each admission to every index period it
#'     overlaps (handles multiple index dates per patient correctly).
#'   \item Admissions are clipped to their matched period boundary.
#'   \item A single-pass cumulative-max sweep (via \code{ave}) groups
#'     overlapping/near-adjacent clips into merged episodes — equivalent to
#'     [merge_intervals()], O(n log n) overall.
#'   \item Dead time and DAOH are computed by vectorised arithmetic.
#' }
#' Column assignments use \code{data.table::set()} and base-R \code{ave()}
#' throughout (no \code{:=}), so the function works correctly when loaded
#' via \code{devtools::load_all()} as well as from an installed package.
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

  # ── Input validation ──────────────────────────────────────────────────────
  stopifnot(is.data.frame(events), is.data.frame(index_dates))
  required_ev  <- c("patientID", "admission", "discharge")
  required_idx <- c("patientID", "indexDate")
  missing_ev   <- setdiff(required_ev,  names(events))
  missing_idx  <- setdiff(required_idx, names(index_dates))
  if (length(missing_ev))
    stop("events is missing columns: ",     paste(missing_ev,  collapse = ", "))
  if (length(missing_idx))
    stop("index_dates is missing columns: ", paste(missing_idx, collapse = ", "))
  if (!"dod" %in% names(events)) events$dod <- NA

  origin_date <- as.Date(origin)

  # as.Date() on POSIXct converts via UTC by default, which shifts any local
  # time earlier than the UTC offset (e.g. NZ mornings) onto the previous
  # calendar day. format() respects the input's timezone.
  local_date <- function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
      as.Date(format(x, "%Y-%m-%d"))
    } else {
      as.Date(x)
    }
  }

  # ── Empty-input fast path ─────────────────────────────────────────────────
  empty_out <- data.frame(
    patientID  = character(), indexDate  = as.Date(character()),
    n_episodes = integer(),   dih        = numeric(),
    dd         = numeric(),   daoh       = numeric(),
    daohPC     = numeric(),   stringsAsFactors = FALSE
  )
  if (nrow(index_dates) == 0L) return(empty_out)

  # ── Index-date table ──────────────────────────────────────────────────────
  # row_id is a unique handle for each (patientID, indexDate) pair that
  # survives all subsequent joins and restores input order at the end.
  idx_dt <- data.table::data.table(
    row_id    = seq_len(nrow(index_dates)),
    patientID = as.character(index_dates$patientID),
    indexDate = local_date(index_dates$indexDate),
    idx_num   = as.numeric(local_date(index_dates$indexDate) - origin_date)
  )
  # Use set() — avoids data.table's cedta() check (triggered only by :=)
  data.table::set(idx_dt, j = "period_end_num",
                  value = idx_dt$idx_num + period)

  # ── DOD per patient: first non-NA date of death found in events ───────────
  dod_dt <- data.table::data.table(
    patientID = as.character(events$patientID),
    dod       = local_date(events$dod)
  )
  dod_dt <- dod_dt[!is.na(dod)]
  # Guard the grouped min(): when no events carry a death date the filtered
  # table is empty, and data.table evaluates j once on the empty group to infer
  # types, triggering min(numeric(0)) -> "no non-missing arguments" warning.
  dod_dt <- if (nrow(dod_dt) > 0L) {
    dod_dt[, .(dod = min(dod, na.rm = TRUE)), by = patientID]
  } else {
    data.table::data.table(patientID = character(0L),
                           dod       = as.Date(character(0L)))
  }

  # ── Convert admissions to numeric intervals via hospital_time() ───────────
  ev_clean <- events[!is.na(events$admission) & !is.na(events$discharge), ,
                     drop = FALSE]

  if (nrow(ev_clean) > 0L) {
    ht    <- hospital_time(ev_clean$admission, ev_clean$discharge,
                           method = method, origin = origin_date)
    ev_dt <- data.table::data.table(
      patientID = as.character(ev_clean$patientID),
      ev_start  = ht$start,
      ev_end    = ht$end
    )

    # ── foverlaps: match each admission to every index period it overlaps ──
    # Key layout: [exact-match col(s), range_start, range_end].
    # Column names in x and y need not match — positional.
    data.table::setkeyv(idx_dt, c("patientID", "idx_num",  "period_end_num"))
    data.table::setkeyv(ev_dt,  c("patientID", "ev_start", "ev_end"))

    joined <- data.table::foverlaps(ev_dt, idx_dt, type = "any", nomatch = NULL)
    # joined: all idx_dt cols (row_id, patientID, indexDate, idx_num,
    # period_end_num) + ev_dt cols (i.patientID, ev_start, ev_end)

    if (nrow(joined) > 0L) {
      # Clip each matched admission to its index period.
      # Use set() — no cedta() check, safe with devtools::load_all().
      data.table::set(joined, j = "cs",
                      value = pmax(joined$ev_start, joined$idx_num))
      data.table::set(joined, j = "ce",
                      value = pmin(joined$ev_end,   joined$period_end_num))
      joined <- joined[joined$ce > joined$cs, ]   # drop zero-overlap clips
    }

    if (nrow(joined) > 0L) {
      # Sort within each index period for the interval-merge sweep
      data.table::setorder(joined, row_id, cs)

      # ── One-pass O(n) interval merge ──────────────────────────────────
      # A new merged group starts when cs > running-max(all previous ce) + gap.
      # Computed per-row_id group using base-R ave() — no := needed.
      #
      # cum_max_ce[i]  = max(ce[1..i]) within the group
      # prev_max_ce[i] = cum_max_ce[i-1] (i.e., lag-1 of cum_max_ce)
      joined$cum_max_ce  <- ave(joined$ce, joined$row_id, FUN = cummax)
      joined$prev_max_ce <- ave(joined$cum_max_ce, joined$row_id,
                                FUN = function(x) c(-Inf, x[-length(x)]))
      joined$new_grp     <- joined$cs > joined$prev_max_ce + gap_days
      joined$grp         <- as.integer(
        ave(joined$new_grp, joined$row_id, FUN = cumsum)
      )

      # Span of each merged group: [min(cs), max(ce)] within (row_id, grp)
      grp_dt <- joined[, .(grp_dih = max(ce) - min(cs)), by = .(row_id, grp)]

      # Per index period: total dih and n_episodes
      dih_dt <- grp_dt[, .(dih = sum(grp_dih), n_episodes = .N), by = row_id]

    } else {
      dih_dt <- data.table::data.table(
        row_id = integer(0L), dih = numeric(0L), n_episodes = integer(0L)
      )
    }
  } else {
    dih_dt <- data.table::data.table(
      row_id = integer(0L), dih = numeric(0L), n_episodes = integer(0L)
    )
  }

  # ── Left-join dih onto all index dates ────────────────────────────────────
  # dih_dt[idx_dt] returns every idx_dt row, NA where unmatched.
  result_dt <- dih_dt[idx_dt, on = "row_id"]
  na_rows   <- which(is.na(result_dt$dih))
  if (length(na_rows) > 0L) {
    data.table::set(result_dt, i = na_rows, j = "dih",        value = 0.0)
    data.table::set(result_dt, i = na_rows, j = "n_episodes", value = 0L)
  }

  # ── Attach date of death ──────────────────────────────────────────────────
  result_dt <- dod_dt[result_dt, on = "patientID"]

  # ── Vectorised dead time ──────────────────────────────────────────────────
  dod_num   <- suppressWarnings(
    as.numeric(as.Date(result_dt$dod) - origin_date)
  )
  in_period <- !is.na(dod_num) &
               dod_num >= result_dt$idx_num &
               dod_num <= result_dt$period_end_num

  # Offset: midday = 0.5 d, midnight/zero = 0.0 d.
  # For death_method = "zero", report midnight-equivalent dd alongside daoh = 0.
  offset <- if (death_method == "midday") 0.5 else 0.0

  dd <- numeric(nrow(result_dt))
  if (any(in_period)) {
    dd[in_period] <- pmax(
      0,
      result_dt$period_end_num[in_period] -
        pmax(dod_num[in_period] + offset, result_dt$idx_num[in_period])
    )
  }
  data.table::set(result_dt, j = "dd", value = dd)

  # ── DAOH ──────────────────────────────────────────────────────────────────
  daoh_vec <- pmax(0, period - result_dt$dih - result_dt$dd)
  if (death_method == "zero") daoh_vec[in_period] <- 0
  data.table::set(result_dt, j = "daoh",   value = daoh_vec)
  data.table::set(result_dt, j = "daohPC", value = 100 * daoh_vec / period)

  # ── Restore input order and return as plain data.frame ────────────────────
  data.table::setorder(result_dt, row_id)

  data.frame(
    patientID  = result_dt$patientID,
    indexDate  = result_dt$indexDate,
    n_episodes = as.integer(result_dt$n_episodes),
    dih        = result_dt$dih,
    dd         = result_dt$dd,
    daoh       = result_dt$daoh,
    daohPC     = result_dt$daohPC,
    stringsAsFactors = FALSE
  )
}
