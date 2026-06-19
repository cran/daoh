#' Compute DAOH summary statistics across all methods and periods
#'
#' @param results_list Named list of data.frames, each the output of
#'   [calc_daoh()] for one method/period combination.
#' @param quantiles Numeric vector of quantiles to report. Default
#'   `c(0.10, 0.25, 0.50)`.
#' @return A data.frame with one row per method/period and columns for mean,
#'   median, and the requested quantiles.
#' @export
daoh_summary <- function(results_list, quantiles = c(0.10, 0.25, 0.50)) {
  stopifnot(is.list(results_list), !is.null(names(results_list)))

  rows <- lapply(names(results_list), function(nm) {
    x <- results_list[[nm]]$daohPC
    qs <- quantile(x, quantiles, na.rm = TRUE)
    row <- data.frame(
      label = nm,
      n     = length(x),
      mean  = mean(x, na.rm = TRUE),
      sd    = sd(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    for (j in seq_along(quantiles)) {
      row[[paste0("p", round(quantiles[j] * 100))]] <- qs[j]
    }
    row
  })
  do.call(rbind, rows)
}


#' Bland-Altman statistics for two DAOH variants
#'
#' Computes the mean difference, standard deviation of differences, and
#' 95% limits of agreement between two DAOH vectors (matched by
#' patientID x indexDate).
#'
#' @param res_a,res_b data.frames (output of [calc_daoh()]) for two methods.
#'   Must have columns `patientID`, `indexDate`, `daoh`.
#' @param use_pc Logical. If `TRUE` (default) use `daohPC`; otherwise `daoh`.
#' @return A list with elements `mean_diff`, `sd_diff`, `loa_lower`,
#'   `loa_upper`, and `data` (data.frame of paired values for plotting).
#' @export
bland_altman_daoh <- function(res_a, res_b, use_pc = TRUE) {
  col <- if (use_pc) "daohPC" else "daoh"

  merged <- merge(res_a[, c("patientID", "indexDate", col)],
                  res_b[, c("patientID", "indexDate", col)],
                  by = c("patientID", "indexDate"),
                  suffixes = c("_a", "_b"))

  va <- merged[[paste0(col, "_a")]]
  vb <- merged[[paste0(col, "_b")]]

  diff  <- va - vb
  avg   <- (va + vb) / 2
  md    <- mean(diff, na.rm = TRUE)
  sd_d  <- sd(diff, na.rm = TRUE)

  list(
    mean_diff = md,
    sd_diff   = sd_d,
    loa_lower = md - 1.96 * sd_d,
    loa_upper = md + 1.96 * sd_d,
    centile_01 = quantile(diff, 0.01, na.rm = TRUE),
    centile_99 = quantile(diff, 0.99, na.rm = TRUE),
    data = data.frame(average = avg, difference = diff)
  )
}


#' Intraclass Correlation Coefficient across DAOH methods
#'
#' Computes two-way mixed ICC (consistency) treating each DAOH calculation
#' method as a rater, following Shrout & Fleiss (1979). Requires the
#' \pkg{irr} package.
#'
#' @param results_list Named list of data.frames (output of [calc_daoh()]).
#'   All elements must share the same patientID x indexDate pairs.
#' @param use_pc Logical. Use `daohPC` (default `TRUE`) or `daoh`.
#' @return The output of [irr::icc()] for the combined method matrix.
#' @export
daoh_icc <- function(results_list, use_pc = TRUE) {
  if (!requireNamespace("irr", quietly = TRUE))
    stop("Package 'irr' required. Install with: install.packages('irr')")

  col <- if (use_pc) "daohPC" else "daoh"

  # Build wide matrix keyed on patientID + indexDate
  base <- results_list[[1]][, c("patientID", "indexDate")]
  mat  <- base

  for (nm in names(results_list)) {
    tmp <- results_list[[nm]][, c("patientID", "indexDate", col)]
    names(tmp)[3] <- nm
    mat <- merge(mat, tmp, by = c("patientID", "indexDate"))
  }

  # Extract just the numeric columns
  num_cols <- names(results_list)
  irr::icc(mat[, num_cols], model = "twoway", type = "consistency", unit = "single")
}


#' Quartile reclassification across two DAOH methods
#'
#' Assesses the practical impact of switching from one DAOH method to
#' another by computing the proportion of patients who move between
#' quartiles. Analogous to the Net Reclassification Index (NRI).
#'
#' @param res_a,res_b data.frames (output of [calc_daoh()]).
#' @param n_groups Integer. Number of groups (default 4 = quartiles).
#' @param use_pc Logical. Use `daohPC` (default) or `daoh`.
#' @return A list with:
#'   \describe{
#'     \item{`confusion_matrix`}{Table of group assignments under a vs b.}
#'     \item{`pct_reclassified`}{Percentage of patients changing group.}
#'     \item{`mean_group_shift`}{Mean absolute group shift.}
#'   }
#' @examples
#' # See vignette("getting_started", package = "daoh")
#' @export
daoh_reclassify <- function(res_a, res_b, n_groups = 4, use_pc = TRUE) {
  col <- if (use_pc) "daohPC" else "daoh"

  merged <- merge(res_a[, c("patientID", "indexDate", col)],
                  res_b[, c("patientID", "indexDate", col)],
                  by = c("patientID", "indexDate"),
                  suffixes = c("_a", "_b"))

  va <- merged[[paste0(col, "_a")]]
  vb <- merged[[paste0(col, "_b")]]

  # Use combined distribution to define group boundaries
  breaks <- quantile(c(va, vb), probs = seq(0, 1, length.out = n_groups + 1),
                     na.rm = TRUE)

  # Deduplicate interior breaks (common with DAOH due to spike at 100%)
  interior <- unique(breaks[-c(1, length(breaks))])
  breaks   <- c(-Inf, interior, Inf)

  effective_groups <- length(breaks) - 1
  if (effective_groups < n_groups) {
    warning(sprintf(
      "Duplicate quantile boundaries reduced effective groups from %d to %d. This is expected with DAOH data due to clustering at 100%%.",
      n_groups, effective_groups
    ))
  }

  ga <- cut(va, breaks = breaks, labels = FALSE, include.lowest = TRUE)
  gb <- cut(vb, breaks = breaks, labels = FALSE, include.lowest = TRUE)

  confusion <- table(a = ga, b = gb)
  pct_reclassified <- 100 * mean(ga != gb, na.rm = TRUE)
  mean_shift <- mean(abs(ga - gb), na.rm = TRUE)

  list(
    confusion_matrix   = confusion,
    pct_reclassified   = pct_reclassified,
    mean_group_shift   = mean_shift,
    data               = data.frame(patientID  = merged$patientID,
                                    indexDate  = merged$indexDate,
                                    group_a    = ga,
                                    group_b    = gb,
                                    group_diff = gb - ga)
  )
}


#' Centile-boundary reclassification across two DAOH methods
#'
#' For each specified centile boundary, classifies patients as inside or
#' outside that boundary under each algorithm — using each algorithm's own
#' empirical threshold — then reports the proportion classified differently.
#' This quantifies the clinical impact of algorithm choice at cut-points such
#' as "bottom 10% poor outcome", which are common in adaptive trial enrichment
#' and secondary outcome definitions.
#'
#' Unlike [daoh_reclassify()], which uses a pooled distribution to set group
#' boundaries, this function applies each algorithm's threshold independently.
#' That reflects the realistic scenario in which a researcher picks a single
#' algorithm, defines a centile-based threshold from their own study population,
#' and applies it — so the threshold itself changes with the algorithm.
#'
#' @param res_a,res_b data.frames (output of [calc_daoh()]). Must have columns
#'   `patientID`, `indexDate`, and `daohPC` (or `daoh` if `use_pc = FALSE`).
#' @param boundaries Numeric vector of centile probabilities at which to
#'   evaluate reclassification. Values below 0.5 define lower ("poor outcome")
#'   boundaries; values at or above 0.5 define upper ("excellent outcome")
#'   boundaries. Default `c(0.05, 0.10, 0.90, 0.95)`.
#' @param use_pc Logical. Use `daohPC` (default `TRUE`) or `daoh`.
#'
#' @return A data.frame with one row per boundary and columns:
#'   \describe{
#'     \item{`boundary`}{Human-readable label, e.g. `"Bottom 5%"` or
#'       `"Top 10%"`.}
#'     \item{`centile`}{The probability supplied in `boundaries`.}
#'     \item{`threshold_a`}{Empirical centile value for algorithm A.}
#'     \item{`threshold_b`}{Empirical centile value for algorithm B.}
#'     \item{`n_patients`}{Number of matched patient-index pairs evaluated.}
#'     \item{`n_reclassified`}{Number of patients classified differently
#'       across the boundary.}
#'     \item{`pct_reclassified`}{Percentage classified differently.}
#'   }
#' @seealso [daoh_reclassify()] for group-based (quartile) reclassification.
#' @examples
#' # See vignette("getting_started", package = "daoh")
#' @export
daoh_reclassify_centile <- function(res_a, res_b,
                                     boundaries = c(0.05, 0.10, 0.90, 0.95),
                                     use_pc = TRUE) {
  stopifnot(is.numeric(boundaries),
            all(boundaries > 0 & boundaries < 1))

  col <- if (use_pc) "daohPC" else "daoh"

  merged <- merge(res_a[, c("patientID", "indexDate", col)],
                  res_b[, c("patientID", "indexDate", col)],
                  by     = c("patientID", "indexDate"),
                  suffixes = c("_a", "_b"))

  va <- merged[[paste0(col, "_a")]]
  vb <- merged[[paste0(col, "_b")]]

  rows <- lapply(boundaries, function(bnd) {

    thresh_a <- quantile(va, bnd, na.rm = TRUE)
    thresh_b <- quantile(vb, bnd, na.rm = TRUE)

    ## Bottom boundaries: classify as "below threshold" (poor outcome)
    ## Top boundaries:    classify as "above threshold" (excellent outcome)
    if (bnd < 0.5) {
      group_a <- va <= thresh_a
      group_b <- vb <= thresh_b
      label   <- paste0("Bottom ", round(bnd * 100), "%")
    } else {
      group_a <- va >= thresh_a
      group_b <- vb >= thresh_b
      label   <- paste0("Top ", round((1 - bnd) * 100), "%")
    }

    ok     <- !is.na(group_a) & !is.na(group_b)
    n_ok   <- sum(ok)
    n_rcls <- sum(group_a[ok] != group_b[ok])

    data.frame(
      boundary         = label,
      centile          = bnd,
      threshold_a      = round(unname(thresh_a), 2),
      threshold_b      = round(unname(thresh_b), 2),
      n_patients       = n_ok,
      n_reclassified   = n_rcls,
      pct_reclassified = round(100 * n_rcls / n_ok, 2),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
