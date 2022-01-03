#'@title Diagnostics for Logistic Regression
#'
#'@description
#'
#'Provides diagnostics tests and graphs for logistic regression
#'
#'
#'@param x an object of class \code{c("glm")}
#'@param alpha numeric; transparency for plot points (default=0.4)
#'@param span numeric; smoothing parameter for loess fit lines (default=0.8)
#'@param ... not currently used
#'
#'@export
#'
#'@import car
#'@import ggplot2
#'@importFrom stats binomial coef
#'
#'@details
#'The \code{diagnostics} function is a wrapper for several
#'diagnostic graphing functions and tests. The function evaluates
#'the following:
#'\describe{
#'  \item{Linearity}{Linearity of the explanatory-response relationship
#'  are assessed via Ceres plots}
#'  (\link[car]{ceresPlots}).
#'  \item{Multicollinearity}{Variance inflation factors (\link[car]{vif})
#'  are plotted. If there is a single predictor variable, this section
#'  is skipped.}
#'  \item{Outliers, leverage, and influence}{An influence plot
#'  (\link{gginfluencePlot}) and Bonferroni Outlier Test
#'  (\link[car]{outlierTest}) are provided.}
#'}
#'
#'@seealso \code{\link{diagnostics}}, \code{\link[car]{vif}},
#' \code{\link[car]{outlierTest}}, \code{\link[car]{influencePlot}},
#' \code{\link[car]{ceresPlots}}
#'
#'@return a list containing the \code{ggplot2} graphs and statistical tests
#'
#'@examples
#'mtcars$cyl <- factor(mtcars$cyl)
#'fit <- glm(am ~ hp + cyl + wt, family=binomial, mtcars)
#'diagnostics(fit)

diagnostics.glm <- function(x, alpha=.4, span=.8, ...){


  heading("DIAGNOSTICS FOR LOGISTIC REGRESSION")
  cat("\n")

  # linearity
  if (length(stats::coef(x)) > 2){
    #crplots <- ggcrPlots(x, alpha=alpha, span=span)
    ceresPlots(x)
    # print(crplots)
  } else {
    # simple scatter plot
  }

  #multicolinearity
  heading("Multicollinearity")
  print(car::vif(x))
  vifplot <- NULL
  if (length(stats::coef(x)) > 2){
    vifplot <- ggvifPlot(x)
  }


  #outliers
  cat("\n")

  influenceplot <- gginfluencePlot(x, alpha)

  heading("Outliers")
  outliers <- outlierTest(x)
  print(outliers)

  # output graphs
  print(vifplot)
  print(influenceplot)

  results <- list(
                  vifplot,
                  influenceplot,
                  outliers)

  invisible(results)
}
