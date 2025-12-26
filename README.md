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
