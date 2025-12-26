#' @title Plot False Discovery Rate (FDR) across PAMR thresholds
#'
#' @description
#' Generates a ggplot visualization of the False Discovery Rate (FDR)
#' computed from a PAMR model. The function takes as input the output of
#' \code{pamr.fdr()}, extracts the Median FDR, the 90th percentile FDR,
#' and the number of significant genes across thresholds, identifies the
#' optimal threshold based on a chosen FDR cutoff, and annotates it on the plot.
#'
#' @param myfdr A list returned by \code{pamr.fdr()}, containing FDR
#'   estimation results across thresholds.
#' @param d The PAMR training dataset used to compute \code{myfdr}.
#'   Must contain \code{x} (expression matrix) and \code{y} (class labels).
#' @param fdr_cutoff Numeric value defining the FDR threshold used to
#'   select the optimal PAMR threshold. Default is \code{0.05}.
#' @param base Base font size for the ggplot theme. Default is \code{16}.
#'
#' @return A ggplot object visualizing FDR metrics across thresholds.
#'
#' @details
#' The function identifies the first threshold where the Median FDR falls
#' below \code{fdr_cutoff}. This threshold and the corresponding number
#' of significant genes are printed and annotated on the plot.
#'
#' @examples
#' \dontrun{
#' library(pamr)
#' data(d)
#' pamr_model <- pamr.train(d)
#' myfdr <- pamr.fdr(pamr_model, d)
#' plot_fdr(myfdr, d, fdr_cutoff = 0.05, base = 18)
#' }
#'
#' @export
plot_fdr <- function(myfdr, d, fdr_cutoff = 0.05, base = 16) {

  # ---- Validate inputs ----
  if (!"results" %in% names(myfdr)) {
    stop("'myfdr' must contain a 'results' component from pamr.fdr().")
  }

  if (!all(c("x", "y") %in% names(d))) {
    stop("'d' must contain components 'x' (matrix) and 'y' (class labels).")
  }

  # ---- Build FDR dataframe ----
  fdr_df <- data.frame(
    Threshold = myfdr$results[, "Threshold"],
    Median_FDR = myfdr$results[, "Median FDR"],
    Percentile_90_FDR = myfdr$results[, "90th percentile of FDR"],
    Number_of_Genes = myfdr$results[, "Number of significant genes"]
  )

  # ---- Select optimal threshold ----
  idx <- which(fdr_df$Median_FDR < fdr_cutoff)

  if (length(idx) == 0) {
    stop("No threshold satisfies Median FDR < ", fdr_cutoff)
  }

  idx <- min(idx)
  selected_threshold <- fdr_df$Threshold[idx]
  selected_genes <- fdr_df$Number_of_Genes[idx]

  # ---- Informative messages ----
  message("Selected threshold (FDR < ", fdr_cutoff, ") = ", selected_threshold)
  message("Number of significant genes = ", selected_genes)

  # ---- Plot ----
  ggplot2::ggplot(fdr_df, ggplot2::aes(x = .data$Threshold)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$Median_FDR, color = "Median FDR"),
      size = 1.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$Percentile_90_FDR, color = "90th percentile FDR"),
      size = 0.9, linetype = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = fdr_cutoff,
      linetype = "dotted",
      color = "black",
      size = 1
    ) +
    ggplot2::geom_vline(
      xintercept = selected_threshold,
      linetype = "dashed",
      color = "purple",
      size = 1
    ) +
    ggplot2::annotate(
      "text",
      x = selected_threshold,
      y = fdr_cutoff + 0.05,
      label = paste0("Selected threshold = ", round(selected_threshold, 3)),
      color = "purple",
      hjust = -0.1
    ) +
    ggplot2::annotate(
      "text",
      x = selected_threshold,
      y = fdr_cutoff + 0.15,
      label = paste0("Significant features = ", selected_genes),
      color = "purple",
      hjust = -0.1
    ) +
    ggplot2::scale_color_manual(
      values = c("Median FDR" = "blue", "90th percentile FDR" = "red")
    ) +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "False Discovery Rate (FDR) across PAMR thresholds",
      x = "Threshold",
      y = "Estimated FDR",
      color = "FDR type"
    )
}
