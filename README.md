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
