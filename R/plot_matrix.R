#' Plot a confusion matrix for PAMR predictions at a selected FDR threshold
#'
#' @description
#' Computes PAMR predictions at the first threshold where the median FDR is
#' below a user-defined cutoff, then displays the corresponding confusion
#' matrix as a heatmap.
#'
#' @param pamr_model A PAMR model returned by \code{pamr.train()}.
#' @param pamr_data A list containing \code{x} (matrix of predictors) and
#'   \code{y} (factor of class labels), typically the same list used for training.
#' @param myfdr A list returned by \code{pamr.fdr()}.
#' @param fdr_cutoff Numeric. FDR threshold used to select the PAMR threshold.
#'   Default is 0.05.
#' @param base Numeric. Base font size for the ggplot theme. Default is 18.
#'
#' @return A ggplot object representing the confusion matrix.
#'
#' @examples
#' \dontrun{
#' library(pamr)
#'
#' data(d)  # dataset included in pamr
#'
#' model <- pamr.train(d)
#' fdr   <- pamr.fdr(model, d)
#'
#' plot_matrix(
#'   pamr_model = model,
#'   pamr_data  = d,
#'   myfdr      = fdr,
#'   fdr_cutoff = 0.05
#' )
#' }
#'
#' @export

plot_matrix <- function(pamr_model,
                        pamr_data,
                        myfdr,
                        fdr_cutoff = 0.05,
                        base = 18) {

  # ---- Check required inputs ----
  if (!"results" %in% names(myfdr)) {
    stop("'myfdr' must contain a 'results' component from pamr.fdr().")
  }

  # ---- Select optimal threshold ----
  idx <- which(myfdr$results[, "Median FDR"] < fdr_cutoff)

  if (length(idx) == 0) {
    stop("No threshold satisfies Median FDR < ", fdr_cutoff)
  }

  idx <- min(idx)
  selected_threshold <- myfdr$results[idx, "Threshold"]

  # ---- Predictions ----
  predictions <- pamr::pamr.predict(
    fit = pamr_model,
    newx = pamr_data$x,
    threshold = selected_threshold
  )

  # ---- Harmonize factor levels ----
  classes <- union(levels(factor(pamr_data$y)), levels(factor(predictions)))
  predictions_f <- factor(predictions, levels = classes)
  reference_f   <- factor(pamr_data$y, levels = classes)

  # ---- Confusion matrix ----
  conf_matrix <- caret::confusionMatrix(predictions_f, reference_f)
  conf_df <- as.data.frame(conf_matrix$table)

  # ---- Error rate ----
  overall_error_rate <- 1 - sum(diag(conf_matrix$table)) / sum(conf_matrix$table)

  # ---- Plot ----
  ggplot2::ggplot(
    conf_df,
    ggplot2::aes(
      x = .data$Reference,
      y = .data$Prediction,
      fill = .data$Freq
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$Freq),
      color = "black",
      size = 5
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::labs(
      title = paste0(
        "Confusion matrix (FDR ≤ ", fdr_cutoff,
        ") | Error rate: ", round(overall_error_rate, 3)
      ),
      subtitle = paste0("Selected threshold = ", round(selected_threshold, 3)),
      x = "Real classes",
      y = "Predicted classes",
      fill = "Count"
    )
}
  