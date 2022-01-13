![](duck.png)

# qacReg


The **qacReg** provides easy access to multiple linear regression and logistic regression
with enhanced performance metrics, visualizations, and diagnostics.

## Installation

You can install the development version of qacReg using:

``` r
if(!require(remotes)){
  install.packages("remotes")
}
remotes::install_github("rkabacoff/qacReg")
```

## Example

Here are two basic examples. Read the Getting Started vignettes
for more information.

``` r
library(qacReg)

# multiple regression (mpg is quantitative)
fit <- regress(mpg ~ hp + wt + cyl, mtcars)
info(fit)
diagnostics(fit)
plots(fit)
relimp(fit)
performance(fit)

# logistic regression (am is binary)
fit <- regress(am ~ hp + wt + mpg, mtcars)
info(fit)
diagnostics(fit)
plots(fit)
relimp(fit)
performance(fit)
```

