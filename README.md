![](qacReg.png)

# qacReg


The **qacReg** provides easy access to linear and logistic regression with enhanced performance metrics, visualizations, and diagnostics. 

## Installation

You can install the development version of qacReg using:

``` r
if(!require(remotes)){
  install.packages("remotes")
}
remotes::install_github("rkabacoff/qacReg")
```

## Workflow

The package supports a simple workflow embodied in 6 functions:

 ```r
 library(qacReg)
 model <- regress(formula, data)       # fit a linear or logistic model
 info(model)                           # review detailed results
 diagnostics(model)                    # perform regression diagnostics
 me_plots(model)                       # visualize conditional relationships
 relimp(model)                         # assess variable importance
 performance(model)                    # evaluate predictive performance
 ```
 See the **Vignettes** for examples.
 
### Why a duck?
 
**qacReg** is on of a suite of packages developed in the [*Quantitative
Analysis Center*](https://www.wesleyan.edu/qac) (QAC) at Wesleyan University. **QAC = QUACK**. It seemed appropriate at the time.
