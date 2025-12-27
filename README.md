# ggpostpamr
modern graphs for pamr outputs



##  R-package


## code to install ggpostpamr package
```r
library(devtools)
install_github("cdesterke/ggpostpamr")
```

## data preprocess to build a list compatible pamr

```r
complete <- expr_subset[complete.cases(expr_subset), ]
complete <-as.matrix(complete)
d <- list(x=complete,y=phenotype$condition, geneid=as.character(row.names(complete)), genenames=paste(row.names(complete)))
```
## load "d" example list from ggpostpamr package
```r
library(ggpostpamr)
data(d)
```
## build the model and error by group
```r
library(pamr)
pamr_model <- pamr.train(d)
cv_results <- pamr.cv(pamr_model, d)
plot_pamr_group_errors(pamr_model, d, cv_results, pal = "Set1", legend_position = "bottom")
```

![res](https://github.com/cdesterke/ggpostpamr/blob/main/01_plot_pamr_group_errors.png)

## select threshold on fdr
```r
myfdr <- pamr.fdr(pamr_model, d)
plot_fdr(myfdr, d, fdr_cutoff = 0.05, base = 18)
```

![res](https://github.com/cdesterke/ggpostpamr/blob/main/02_plot_fdr.png)

## confusion matrix for selected threshold
```r
plot_matrix(pamr_model, d, myfdr, fdr_cutoff = 0.05, base = 18)
```

![res](https://github.com/cdesterke/ggpostpamr/blob/main/03_plot_matrix.png)

## confusion matrix for selected threshold
```r
plot_pamr_cv_prob_threshold(cv_results, threshold_value = 1.095,
                            palette = "Dark2",
                            point_size = 3,
                           legend_position = "bottom")
```

![res](https://github.com/cdesterke/ggpostpamr/blob/main/04_plot_pamr_cv_prob_threshold.png)

## dotplot of predictive scores by group
```r
plot_pamr_thresholds_cleveland(
  pamr_model = pamr_model,
  pamr_data = d,
  threshold = 1.5,
  n_genes = 17,
  base_size = 16,
  palette = "Set1",
  dot_size = 4,
  legend_position = "bottom"
)
```

![res](https://github.com/cdesterke/ggpostpamr/blob/main/05_plot_pamr_thresholds_cleveland.png)

## boxplot for a selected feature with statistical test
```r
plot_pamr_gene_by_group(
  pamr_data = d,
  gene_name = "FGF13",
  palette = "Dark2",
  base_size = 16,
  dot_size = 3,
  legend_position = "bottom"
)
)
```

![res](https://github.com/cdesterke/ggpostpamr/blob/main/06_plot_pamr_gene_by_group.png)

# REFERENCES

> Tibshirani R., Hastie T., Narasimhan B., Chu G.  
Diagnosis of multiple cancer types by shrunken centroids of gene expression.  
Proceedings of the National Academy of Sciences (PNAS), 2002.
DOI : 10.1073/pnas.082099299

