
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

Here is a basic example:

``` r
library(qacReg)
mtcars$cyl <- factor(mtcars$cyl)
fit <- mreg(mpg ~ hp + wt + cyl, mtcars)
summary(fit)
diagnostics(fit)
plot(fit)
```

