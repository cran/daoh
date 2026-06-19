#' Merge overlapping or near-adjacent time intervals
#'
#' Given a set of intervals \[start, end\], merges those that overlap or are
#' separated by less than `gap` time units. This is used to consolidate
#' hospital admissions that are separated by short gaps (e.g., < 12 hours),
#' treating them as a single continuous episode.
#'
#' @param starts Numeric or POSIXct vector of interval start times.
#' @param ends   Numeric or POSIXct vector of interval end times (must be >=
#'   the corresponding start).
#' @param gap    Numeric. Intervals with a gap smaller than this value are
#'   merged. Units must match `starts`/`ends`. Default 0.5 (= 12 hours if
#'   times are in days).
#'
#' @return A data.frame with columns `start` and `end` representing the
#'   merged intervals, sorted by start time.
#'
#' @examples
#' # Two admissions 10 hours apart -> merged into one
#' merge_intervals(
#'   starts = c(0, 1.5),
#'   ends   = c(1.0, 2.5),
#'   gap    = 0.5
#' )
#'
#' # Two admissions 2 days apart -> kept separate
#' merge_intervals(
#'   starts = c(0, 3),
#'   ends   = c(1, 4),
#'   gap    = 0.5
#' )
#'
#' @export
merge_intervals <- function(starts, ends, gap = 0.5) {
  if (length(starts) == 0) return(data.frame(start = numeric(0), end = numeric(0)))
  stopifnot(length(starts) == length(ends))
  stopifnot(all(!is.na(starts)), all(!is.na(ends)))

  ord <- order(starts)
  s <- starts[ord]
  e <- ends[ord]

  # Ensure no zero-length intervals
  e <- pmax(e, s)

  merged_s <- s[1]
  merged_e <- e[1]
  out_s <- c()
  out_e <- c()

  for (i in seq_along(s)[-1]) {
    if (s[i] <= merged_e + gap) {
      # Extend current interval
      merged_e <- max(merged_e, e[i])
    } else {
      out_s <- c(out_s, merged_s)
      out_e <- c(out_e, merged_e)
      merged_s <- s[i]
      merged_e <- e[i]
    }
  }
  out_s <- c(out_s, merged_s)
  out_e <- c(out_e, merged_e)

  data.frame(start = out_s, end = out_e)
}


#' Clip intervals to a window and return total length
#'
#' Clips merged intervals to \[window_start, window_end\] and returns the sum
#' of their lengths. Used internally to calculate total time in hospital or
#' dead within the DAOH period.
#'
#' @param intervals data.frame with columns `start` and `end` (output of
#'   [merge_intervals()]).
#' @param window_start Numeric start of the clipping window.
#' @param window_end   Numeric end of the clipping window.
#'
#' @return Scalar numeric: total length of intervals within the window.
#'
#' @keywords internal
clip_and_sum <- function(intervals, window_start, window_end) {
  if (nrow(intervals) == 0) return(0)
  clipped_s <- pmax(intervals$start, window_start)
  clipped_e <- pmin(intervals$end,   window_end)
  lengths   <- pmax(0, clipped_e - clipped_s)
  sum(lengths)
}
