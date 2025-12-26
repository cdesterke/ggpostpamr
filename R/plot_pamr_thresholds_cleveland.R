#' @title Cleveland plot of PAMR gene metrics at a given threshold
#'
#' @description
#' Produces a Cleveland dot plot of the gene-level metrics returned by
#' \code{pamr.listgenes()} for a specified threshold. The function reshapes
#' the PAMR output into long format and visualizes the different metrics
#' (e.g., scores, centroid differences) for the top \code{n_genes} ranked
#' genes. A horizontal dashed line at zero is added to highlight sign changes.
#'
#' @param pamr_model A PAMR model object produced by \code{pamr.train()}.
#' @param pamr_data A list containing the training data used by PAMR,
#'   typically the same list passed to \code{pamr.train()}.
#' @param threshold Numeric value specifying the PAMR threshold at which
#'   gene metrics should be extracted. This argument is required by
#'   \code{pamr.listgenes()}.
#' @param n_genes Integer indicating the number of top-ranked genes to display
#'   in the Cleveland plot. Default is \code{20}.
#' @param base_size Base font size for the ggplot theme. Default is \code{14}.
#' @param palette Character string specifying the ColorBrewer palette used
#'   for coloring the different PAMR metrics. Default is \code{"Set2"}.
#' @param dot_size Numeric value controlling the size of the points in the plot.
#'   Default is \code{3}.
#' @param legend_position Character string specifying the position of the legend.
#'   Accepted values are \code{"right"}, \code{"left"}, \code{"top"},
#'   \code{"bottom"}, or \code{"none"}. Default is \code{"right"}.
#'
#' @return A ggplot2 object representing the Cleveland dot plot of PAMR metrics.
#'
#' @details
#' The function relies on \code{pamr.listgenes()} to extract gene-level
#' statistics at a given threshold. All numeric columns returned by PAMR
#' (e.g., scores, centroid differences) are reshaped into long format and
#' plotted as separate groups. The genes are displayed in their original
#' ranking order, and a dashed horizontal line at zero helps visualize
#' positive and negative metric values.
#'
#' @examples
#' \dontrun{
#' library(pamr)
#' data(d)
#' model <- pamr.train(d)
#'
#' plot_pamr_thresholds_cleveland(
#'   pamr_model = model,
#'   pamr_data = d,
#'   threshold = 1.5,
#'   n_genes = 17,
#'   base_size = 16,
#'   palette = "Set1",
#'   dot_size = 4,
#'   legend_position = "bottom"
#' )
#' }
#'
#' @export






plot_pamr_thresholds_cleveland <- function(pamr_model,
                                           pamr_data,
                                           threshold,
                                           n_genes = 20,
                                           base_size = 14,
                                           palette = "Set2",
                                           dot_size = 3,
                                           legend_position = "right") {
  
  # ---- Check required packages ----
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required but not installed.")
  if (!requireNamespace("tidyr", quietly = TRUE))
    stop("Package 'tidyr' is required but not installed.")
  if (!requireNamespace("pamr", quietly = TRUE))
    stop("Package 'pamr' is required but not installed.")
  
  # ---- Validate threshold ----
  if (missing(threshold) || is.null(threshold))
    stop("Argument 'threshold' must be provided for pamr.listgenes().")
  
  # ---- Extract genes and metrics for this threshold ----
  gl <- pamr::pamr.listgenes(fit = pamr_model,
                             data = pamr_data,
                             threshold = threshold)
  
  df <- as.data.frame(gl, stringsAsFactors = FALSE)
  
  if (!"id" %in% names(df))
    stop("pamr.listgenes() output must contain an 'id' column.")
  
  # Numeric columns
  num_cols <- setdiff(names(df), "id")
  df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
  
  df$rank <- seq_len(nrow(df))
  
  df_long <- tidyr::pivot_longer(
    df,
    cols = num_cols,
    names_to = "group",
    values_to = "score"
  )
  
  df_long <- dplyr::filter(df_long, rank <= n_genes)
  
  # ---- Plot ----
  ggplot2::ggplot(df_long,
                  ggplot2::aes(x = rank, y = score, color = group)) +
    ggplot2::geom_point(size = dot_size, alpha = 1) +
    
    # Ligne horizontale pointillée à score = 0
    ggplot2::geom_hline(yintercept = 0,
                        linetype = "dashed",
                        color = "grey40") +
    
    ggplot2::scale_x_continuous(
      breaks = df$rank[1:n_genes],
      labels = df$id[1:n_genes]
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = base_size) +
    
    # ---- Position de la légende ----
    ggplot2::theme(legend.position = legend_position) +
    
    ggplot2::labs(
      x = "Gene sets",
      y = "pamr metrics",
      color = "Metric",
      title = paste0(
        "pamr metrics at threshold = ", threshold,
        " (", n_genes, " items)"
      )
    ) +
    ggplot2::scale_color_brewer(palette = palette)
}
