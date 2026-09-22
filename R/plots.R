#' Plot the distribution of DAOH scores
#'
#' Produces a histogram of DAOH scores with deaths overlaid in a
#' contrasting colour. The y-axis is log-scaled to aid visualisation of
#' the bimodal distribution.
#'
#' @param result data.frame (output of [calc_daoh()]).
#' @param log_y Logical. Use log10 y-axis (default `TRUE`).
#' @param title Character. Plot title.
#' @param bins Integer. Number of histogram bins (default 50).
#' @param use_pc Logical. Plot DAOH in days (`daoh`, the default) or as a
#'   percentage of the period (`daohPC`, when `TRUE`).
#' @param ylab Character. y-axis label. Defaults to `"Count"` (the y-axis
#'   shows the absolute number of observations per bin).
#'
#' @return A ggplot2 object.
#' @export
plot_daoh_dist <- function(result, log_y = TRUE,
                           title = "DAOH distribution", bins = 50,
                           use_pc = FALSE, ylab = "Count") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' required.")

  xlab     <- if (use_pc) "DAOH (%)" else "DAOH (days)"
  # daohPC and daoh are both listed in globalVariables(), so bare-symbol aes
  # is safe under R CMD check and matches the rest of the package's style.
  hist_aes <- if (use_pc) ggplot2::aes(x = daohPC) else ggplot2::aes(x = daoh)
  deaths   <- result[result$dd > 0, ]

  p <- ggplot2::ggplot(result, hist_aes) +
    ggplot2::geom_histogram(fill = "grey60", colour = "white",
                            bins = bins, boundary = 0) +
    ggplot2::labs(title = title,
                  x = xlab, y = ylab) +
    ggplot2::theme_bw()

  if (nrow(deaths) > 0) {
    p <- p + ggplot2::geom_histogram(data = deaths, hist_aes,
                                     fill = "black", colour = NA,
                                     bins = bins, boundary = 0)
  }

  if (log_y) {
    p <- p + ggplot2::scale_y_continuous(
      trans = "log1p",
      breaks = c(0, 1, 10, 100, 1000, 10000, 100000),
      labels = scales::comma
    )
  }
  p
}


#' Plot a Bland-Altman comparison of two DAOH methods
#'
#' @param ba_result Output of [bland_altman_daoh()].
#' @param method_a,method_b Character labels for the two methods.
#' @param use_hex Logical. Use `geom_hex` for density (default `TRUE`,
#'   recommended for large datasets).
#' @param show_loa Logical. When `TRUE`, the numeric mean-bias and 95%
#'   limits-of-agreement values are printed directly on the plot, next to
#'   their reference lines (in addition to the caption). Default `FALSE`.
#' @return A ggplot2 object.
#' @export
plot_daoh_ba <- function(ba_result, method_a = "Method A",
                         method_b = "Method B", use_hex = TRUE,
                         show_loa = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' required.")

  dat <- ba_result$data
  md  <- ba_result$mean_diff
  loa <- c(ba_result$loa_lower, ba_result$loa_upper)

  # Units follow whatever bland_altman_daoh() was computed on. Older results
  # (before the `units` field existed) are assumed to be percentages.
  unit <- if (!is.null(ba_result$units)) ba_result$units else "%"
  usym <- if (unit == "%") "%" else paste0(" ", unit)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = average, y = difference))

  if (use_hex) {
    p <- p + ggplot2::geom_hex(bins = 60) +
      ggplot2::scale_fill_viridis_c(option = "plasma")
  } else {
    p <- p + ggplot2::geom_point(alpha = 0.3, size = 0.5)
  }

  p <- p +
    ggplot2::geom_hline(yintercept = md,  linetype = "solid",  colour = "red") +
    ggplot2::geom_hline(yintercept = loa, linetype = "dashed", colour = "red") +
    ggplot2::labs(
      title = paste("Bland-Altman:", method_a, "vs", method_b),
      x = paste0("Mean (", method_a, " + ", method_b, ") / 2 (", unit, ")"),
      y = paste0("Difference (", method_a, " - ", method_b, ") (", unit, ")"),
      caption = sprintf("Mean diff = %.2f%s; 95%% LoA: %.2f%s to %.2f%s",
                        md, usym, loa[1], usym, loa[2], usym)
    ) +
    ggplot2::theme_bw()

  # Optionally label the reference lines with their values. annotate() takes
  # plain vectors (no data mask / global-variable bindings), keeping this
  # R CMD check-clean. Labels are right-aligned to the widest mean value.
  if (isTRUE(show_loa)) {
    x_pos <- max(dat$average, na.rm = TRUE)
    p <- p + ggplot2::annotate(
      "label",
      x     = x_pos,
      y     = c(loa[2], md, loa[1]),
      label = c(sprintf("Upper LoA: %.2f%s", loa[2], usym),
                sprintf("Mean bias: %.2f%s", md,     usym),
                sprintf("Lower LoA: %.2f%s", loa[1], usym)),
      hjust = 1, vjust = -0.3, size = 3,
      colour = "red", fill = "white", alpha = 0.8
    )
  }

  p
}


#' Visualise quartile reclassification between two DAOH methods
#'
#' Plots a heatmap of the reclassification table (method A group vs
#' method B group) with cell counts and an annotation of the overall
#' reclassification rate.
#'
#' @param reclass_result Output of [daoh_reclassify()].
#' @param method_a,method_b Character labels for the two methods.
#' @return A ggplot2 object.
#' @export
plot_daoh_reclassify <- function(reclass_result,
                                  method_a = "Method A",
                                  method_b = "Method B") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' required.")

  cm <- as.data.frame(reclass_result$confusion_matrix)
  names(cm) <- c("Group_A", "Group_B", "Count")
  cm$Group_A <- factor(cm$Group_A)
  cm$Group_B <- factor(cm$Group_B)

  ggplot2::ggplot(cm, ggplot2::aes(x = Group_B, y = Group_A, fill = Count)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = Count), colour = "white",
                       fontface = "bold", size = 4) +
    ggplot2::scale_fill_gradient(low = "#3182bd", high = "#08519c") +
    ggplot2::labs(
      title = sprintf("Reclassification: %.1f%% of patients change group",
                      reclass_result$pct_reclassified),
      x = paste(method_b, "group"),
      y = paste(method_a, "group"),
      fill = "n"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12))
}
