#'@title Regression Diagnostics for Generalized Linear Models
#'
#'@description
#'
#'Provides regression diagnostics for a generalized linear model fit with
#'\code{\link{glm}} or \code{\link{regression}}. Currently, binary logistic
#'regression models are supported.
#'
#'@param x an object of class \code{c("glm")}
#'@param alpha numeric; transparency for plot points (default=0.4)
#'@param span numeric; smoothing parameter for loess fit lines (default=0.8)
#'@param plot logical; If \code{TRUE} (the default), graphs are printed. Otherwise,
#'they are returned invisibly.
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
#'diagnostic plotting functions:
#'\describe{
#'  \item{Linearity}{Linearity of the explanatory-response relationships
#'  are assessed via Component + Residual (partial residual) plots
#'  (\code{\link{ggcrPlots}}). If there is a single predictor, a scatter plot
#'  with linear and loess lines is produced.}
#'  \item{Multicollinearity}{Multicollinearity is assessed via variance inflation factors
#'  (\code{\link{ggvifPlot}}). If there is a single predictor variable, this section
#'  is skipped.}
#'  \item{Outliers, leverage, and influence}{A influence plot identifies
#'  outliers and influential observations (\code{\link{gginfluencePlot}}).}
#'}
#'
#'@note
#'Each function relies heavily on the \code{car} package. See the
#'help for individual functions for details.
#'
#'@seealso \code{\link{diagnostics}}, \code{\link[car]{vif}},
#' \code{\link[car]{outlierTest}}, \code{\link[car]{influencePlot}},
#' \code{\link[car]{crPlots}}
#'
#'@return
#'A three component list containing \code{ggplot2} graphs:
#'crplots, vifplot, and influenceplot.
#'@examples
#' fit <- glm(caesarian ~ age + bp + delivery.time, family = binomial, data = caesarian)
#' diagnostics(fit)

diagnostics.glm <- function(x, alpha=.4, span=.8, plot=TRUE, ...){

  # linearity
  if (length(stats::coef(x)) > 2){
    #crplots <- ggcrPlots(x, alpha=alpha, span=span)
    crplots <- ggcrPlots(x)
    # print(crplots)
  } else {
    # simple scatter plot
  }

  #multicolinearity
  vifplot <- NULL
  if (length(stats::coef(x)) > 2){
    vifplot <- ggvifPlot(x)
  }

  #outliers
  influenceplot <- gginfluencePlot(x, alpha)

  # output graphs
  if(plot){
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
    print(crplots)
    print(vifplot)
    print(influenceplot)
  }

  results <- list(crplots,
                  vifplot,
                  influenceplot)

  invisible(results)
}
