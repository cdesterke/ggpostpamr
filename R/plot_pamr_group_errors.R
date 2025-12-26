#' @title Plot misclassification error by group for a PAMR model
#'
#' @description
#' Generates a line plot showing the misclassification error per group
#' across different PAMR thresholds (i.e., number of genes used).
#'
#' @param pamr_model A PAMR model object produced by \code{pamr.train()}.
#' @param pamr_data A list containing \code{x} (expression matrix) and \code{y} (class labels),
#'   typically the same structure used by \code{pamr.train()}.
#' @param cv_results Cross‑validation results from \code{pamr.cv()}.
#' @param pal Either a ColorBrewer palette name (e.g. "Set1") or a custom vector of colors.
#' @param base Base font size for the ggplot theme.
#' @param legend_position Position of the legend. One of:
#'   \code{"right"}, \code{"left"}, \code{"top"}, \code{"bottom"}, \code{"none"}.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' data(d)
#' library(pamr)
#' pamr_model <- pamr.train(d)
#' cv_results <- pamr.cv(pamr_model, d)
#' plot_pamr_group_errors(pamr_model, d, cv_results, pal = "Set1", legend_position = "bottom")
#' }
#'
#' @export
plot_pamr_group_errors <- function(pamr_model,
                                   pamr_data,
                                   cv_results,
                                   pal = "Set1",
                                   base = 18,
                                   legend_position = "right") {
  
  # ---- Check required packages ----
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required but not installed.")
  if (!requireNamespace("RColorBrewer", quietly = TRUE))
    stop("Package 'RColorBrewer' is required but not installed.")
  if (!requireNamespace("pamr", quietly = TRUE))
    stop("Package 'pamr' is required but not installed.")
  
  # ---- Extract CV information ----
  thresholds <- cv_results$threshold
  sizes <- cv_results$size
  groups <- unique(pamr_data$y)
  
  # ---- Prepare storage ----
  out_list <- vector("list", length(thresholds) * length(groups))
  k <- 1
  
  # ---- Compute misclassification per group ----
  for (i in seq_along(thresholds)) {
    pred <- pamr::pamr.predict(
      pamr_model,
      pamr_data$x,
      threshold = thresholds[i],
      type = "class"
    )
    
    for (g in groups) {
      true_g <- pamr_data$y == g
      misclass <- mean(pred[true_g] != g)
      
      out_list[[k]] <- data.frame(
        ngenes = sizes[i],
        group = g,
        error = misclass,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
  
  errors_by_group <- do.call(rbind, out_list)
  
  # ---- Plot ----
  ggplot2::ggplot(errors_by_group,
                  ggplot2::aes(x = ngenes, y = error, color = group)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::scale_color_brewer(palette = pal) +
    ggplot2::labs(
      x = "Number of features",
      y = "Misclassification error / group",
      color = "Groups",
      title = "Misclassification by group / features"
    )
}


