#' @title Boxplot of a PAMR gene expression across groups with automatic statistical test
#'
#' @description
#' Produces an individual gene-level boxplot from a PAMR data object and performs
#' an appropriate statistical test depending on the number of groups. When the
#' dataset contains exactly two groups, a Wilcoxon rank-sum test is applied.
#' When more than two groups are present, a Kruskal–Wallis test is used.
#' The resulting p-value is displayed in the plot title.
#'
#' @param pamr_data A PAMR data list, typically the same list used to train a
#'   PAMR model, containing elements \code{x}, \code{y}, \code{genenames},
#'   and optionally \code{geneid}.
#' @param gene_name Character string specifying the gene to visualize. The
#'   function searches both \code{pamr_data$genenames} and \code{pamr_data$geneid}.
#' @param palette Character string specifying the ColorBrewer palette used for
#'   group colors. Default is \code{"Set2"}.
#' @param base_size Numeric value controlling the base font size of the plot.
#'   Default is \code{14}.
#' @param dot_size Numeric value controlling the size of jittered points.
#'   Default is \code{2}.
#' @param legend_position Character string specifying the position of the legend.
#'   Accepted values are \code{"right"}, \code{"left"}, \code{"top"},
#'   \code{"bottom"}, or \code{"none"}. Default is \code{"right"}.
#'
#' @details
#' The function extracts the expression vector of the selected gene from the
#' PAMR data matrix and constructs a tidy data frame for visualization.
#' A statistical test is automatically selected:
#' \itemize{
#'   \item \strong{Wilcoxon rank-sum test} if there are exactly two groups.
#'   \item \strong{Kruskal–Wallis test} if there are more than two groups.
#' }
#' The global p-value is displayed in the plot title along with the name of the
#' test used. The visualization includes a boxplot, jittered points, and
#' customizable aesthetics.
#'
#' @return A \code{ggplot2} object representing the gene expression boxplot.
#'
#' @examples
#' \dontrun{
#' library(pamr)
#' data(d)
#'
#' plot_pamr_gene_by_group(
#'   pamr_data = d,
#'   gene_name = "FGF13",
#'   palette = "Dark2",
#'   base_size = 16,
#'   dot_size = 3,
#'   legend_position = "bottom"
#' )
#' }
#'
#' @export


plot_pamr_gene_by_group <- function(pamr_data,
                                    gene_name,
                                    palette = "Set2",
                                    base_size = 14,
                                    dot_size = 2,
                                    legend_position = "right") {
  
  # ---- Check required packages ----
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required but not installed.")
  
  # ---- Identify gene ----
  idx <- which(pamr_data$genenames == gene_name |
                 pamr_data$geneid == gene_name)
  if (length(idx) == 0)
    stop("Gene not found in pamr_data.")
  
  # ---- Extract expression values ----
  values <- pamr_data$x[idx, ]
  
  df <- data.frame(
    sample = seq_along(values),
    expression = as.numeric(values),
    group = factor(pamr_data$y)
  )
  
  n_groups <- length(levels(df$group))
  
  # ---- Statistical test ----
  if (n_groups == 2) {
    # Wilcoxon test
    test <- stats::wilcox.test(expression ~ group, data = df, exact = FALSE)
    pval_global <- signif(test$p.value, 3)
    test_label <- "Wilcoxon"
    
  } else {
    # Kruskal–Wallis test
    test <- stats::kruskal.test(expression ~ group, data = df)
    pval_global <- signif(test$p.value, 3)
    test_label <- "Kruskal–Wallis"
  }
  
  # ---- Plot ----
  ggplot2::ggplot(df,
                  ggplot2::aes(x = group, y = expression, color = group)) +
    ggplot2::geom_boxplot(alpha = 1, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.2, size = dot_size, alpha = 1) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::labs(
      x = "Observed class",
      y = paste("Quantification of", gene_name),
      color = "Group",
      title = paste0(
        "Group ~ ", gene_name,
        " (", test_label, " p = ", pval_global, ")"
      )
    ) +
    ggplot2::scale_color_brewer(palette = palette)
}
