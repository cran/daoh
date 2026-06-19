#' Plot the distribution of DAOH scores
#'
#' Produces a histogram of DAOH scores (as percentage) with deaths
#' overlaid in a contrasting colour.
#' The y-axis is log-scaled to aid visualisation of the bimodal
#' distribution.
#'
#' @param result data.frame (output of [calc_daoh()]).
#' @param log_y Logical. Use log10 y-axis (default `TRUE`).
#' @param title Character. Plot title.
#' @param bins Integer. Number of histogram bins (default 50).
#'
#' @return A ggplot2 object.
#' @export
plot_daoh_dist <- function(result, log_y = TRUE,
                           title = "DAOH distribution", bins = 50) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' required.")

  deaths <- result[result$dd > 0, ]

  p <- ggplot2::ggplot(result, ggplot2::aes(x = daohPC)) +
    ggplot2::geom_histogram(fill = "grey60", colour = "white",
                            bins = bins, boundary = 0) +
    ggplot2::labs(title = title,
                  x = "DAOH (%)", y = "Frequency") +
    ggplot2::theme_bw()

  if (nrow(deaths) > 0) {
    p <- p + ggplot2::geom_histogram(data = deaths,
                                     ggplot2::aes(x = daohPC),
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
#' @return A ggplot2 object.
#' @export
plot_daoh_ba <- function(ba_result, method_a = "Method A",
                         method_b = "Method B", use_hex = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' required.")

  dat <- ba_result$data
  md  <- ba_result$mean_diff
  loa <- c(ba_result$loa_lower, ba_result$loa_upper)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = average, y = difference))

  if (use_hex) {
    p <- p + ggplot2::geom_hex(bins = 60) +
      ggplot2::scale_fill_viridis_c(option = "plasma")
  } else {
    p <- p + ggplot2::geom_point(alpha = 0.3, size = 0.5)
  }

  p +
    ggplot2::geom_hline(yintercept = md,  linetype = "solid",  colour = "red") +
    ggplot2::geom_hline(yintercept = loa, linetype = "dashed", colour = "red") +
    ggplot2::labs(
      title = paste("Bland-Altman:", method_a, "vs", method_b),
      x = paste0("Mean (", method_a, " + ", method_b, ") / 2 (%)"),
      y = paste0("Difference (", method_a, " - ", method_b, ") (%)"),
      caption = sprintf("Mean diff = %.2f%%; 95%% LoA: %.2f%% to %.2f%%",
                        md, loa[1], loa[2])
    ) +
    ggplot2::theme_bw()
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
