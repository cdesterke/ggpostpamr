#' @title Plot PAMR cross‑validated class probabilities at a chosen threshold
#'
#' @description
#' Visualizes the cross‑validated class probabilities produced by
#' \code{pamr.cv()} for a user‑defined threshold. The function extracts the
#' probability matrix corresponding to the closest available threshold,
#' reshapes it into long format, and generates a scatterplot showing the
#' predicted probability for each class across samples.
#'
#' @param cv_results A list returned by \code{pamr.cv()} with
#'   \code{prob}, \code{threshold}, \code{size}, and \code{y} components.
#'   The argument \code{pamr.cv(..., probability = TRUE)} must have been used.
#' @param threshold_value Numeric value indicating the desired PAMR threshold.
#'   The closest available threshold in \code{cv_results$threshold} is used.
#' @param palette Character string specifying the ColorBrewer palette used
#'   for class colors. Default is \code{"Set2"}.
#' @param point_size Numeric value controlling the size of points in the plot.
#'   Default is \code{2}.
#' @param legend_position Character string specifying the position of the legend.
#'   Accepted values are \code{"right"}, \code{"bottom"}, \code{"top"},
#'   \code{"left"}, or \code{"none"}. Default is \code{"right"}.
#' @param base Base font size for the ggplot theme. Default is \code{16}.
#'
#' @return A ggplot object visualizing the cross‑validated class probabilities.
#'
#' @details
#' The function extracts the probability matrix corresponding to the threshold
#' closest to \code{threshold_value}. If the probability matrix has no column
#' names, class labels are inferred from \code{cv_results$y}. The output plot
#' displays one point per sample and per class, colored according to the
#' predicted class.
#'
#' @examples
#' \dontrun{
#' library(pamr)
#' data(d)
#' model <- pamr.train(d)
#' cvres <- pamr.cv(model, d)
#' plot_pamr_cv_prob_threshold(cvres, threshold_value = 2.5,
#'                             palette = "Dark2",
#'                             point_size = 3,
#'                             legend_position = "bottom")
#' }
#'
#' @export






plot_pamr_cv_prob_threshold <- function(cv_results,
                                        threshold_value,
                                        palette = "Set2",
                                        point_size = 2,
                                        legend_position = "right",
                                        base = 16) {
  
  # ---- Check required packages ----
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required but not installed.")
  if (!requireNamespace("tidyr", quietly = TRUE))
    stop("Package 'tidyr' is required but not installed.")
  
  # ---- Validate inputs ----
  if (is.null(cv_results$prob))
    stop("'cv_results$prob' is missing. Did you run pamr.cv(..., probability=TRUE) ?")
  
  if (is.null(cv_results$threshold))
    stop("'cv_results$threshold' is missing in cv_results.")
  
  # ---- Find closest threshold ----
  idx <- which.min(abs(cv_results$threshold - threshold_value))
  
  # ---- Extract probability matrix ----
  probs <- cv_results$prob[, , idx]
  true_classes <- cv_results$y
  
  # ---- Ensure column names exist ----
  if (is.null(colnames(probs))) {
    colnames(probs) <- levels(factor(true_classes))
  }
  
  # ---- Build dataframe ----
  df_prob <- as.data.frame(probs)
  df_prob$sample <- seq_along(true_classes)
  df_prob$true_class <- true_classes
  
  df_long <- tidyr::pivot_longer(
    df_prob,
    cols = colnames(probs),
    names_to = "class",
    values_to = "probability"
  )
  
  # ---- Plot ----
  ggplot2::ggplot(df_long,
                  ggplot2::aes(x = sample, y = probability, color = class)) +
    ggplot2::geom_point(size = point_size, alpha = 1) +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::labs(
      x = "Samples",
      y = "Probability of classification (CV)",
      color = "Predicted class",
      title = paste0(
        "pamr CV — threshold = ", round(cv_results$threshold[idx], 3),
        " (", cv_results$size[idx], " features)"
      )
    ) +
    ggplot2::scale_color_brewer(palette = palette)
}
