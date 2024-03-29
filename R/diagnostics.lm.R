#'@title Regression Diagnostics for Linear Models
#'
#'@description
#'
#'Provides regression diagnostics for a linear models fit with
#'\code{\link{lm}} or \code{\link{regress}}.
#'
#'
#'@param x an object of class \code{"lm"}
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
#'@importFrom stats coef median pchisq hat var
#'@importFrom grDevices devAskNewPage
#'
#'@details
#'The \code{diagnostics} function is a wrapper for several
#'diagnostic plotting functions:
#'\describe{
#'  \item{Normality}{Normality of the (studentized) residuals is assessed
#'  via a Normal Q-Q plot (\code{\link{qq_plot}}).}
#'  \item{Linearity}{Linearity of the explanatory-response relationships
#'  are assessed via Component + Residual (partial residual) plots
#'  (\code{\link{cr_plots}}). If there is a single predictor, a scatter plot
#'  with linear and loess lines is produced.}
#'  \item{Constant variance}{Homoscedasticity is evaluated via
#'  a Spread-Level plot (\code{\link{spread_plot}}).}
#'  \item{Multicollinearity}{Multicollinearity is assessed via variance inflation factors
#'  (\code{\link{vif_plot}}). If there is a single predictor variable, this section
#'  is skipped.}
#'  \item{Outliers, leverage, and influence}{A influence plot identifies
#'  outliers and influential observations (\code{\link{influence_plot}}).}
#'}
#'
#'@note
#'Each function relies heavily on the \code{car} package. See the
#'help for individual functions for details.
#'
#'@seealso \code{\link{diagnostics}}, \code{\link[car]{vif}},
#' \code{\link[car]{qqPlot}}, \code{\link[car]{outlierTest}}, \code{\link[car]{influencePlot}},
#' \code{\link[car]{crPlots}}, \code{\link[car]{spreadLevelPlot}}, \code{\link[car]{ncvTest}}
#'
#'@return
#'A five component list containing \code{ggplot2} graphs:
#'qqplot, crplots, slplot, vifplot, and influenceplot.
#'
#'@examples
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' diagnostics(fit)

diagnostics.lm <- function(x, alpha=.4, span=.8, plot=TRUE, ...){


  # normality
  qqplot <- qq_plot(x, alpha=alpha)


  # linearity
  if (length(stats::coef(x)) > 2){
    crplots <- cr_plots(x, alpha=alpha, span=span)
  } else {
    # simple scatter plot
    yvar <- names(x$model)[1]
    xvar <- names(x$model)[2]
    crplots <- ggplot(data=x$model,
           aes(x=.data[[xvar]], y=.data[[yvar]])) +
      geom_point(alpha=alpha) +
      geom_smooth(method="lm", formula=y ~ x,
                  color="blue", linetype="dashed",
                  se=FALSE) +
      geom_smooth(method="loess", formula=y~x,
                  color="indianred2", linetype="solid",
                  se=FALSE, span=span) +
      labs(title="Scatter plot",
           subtitle=paste("Assessing linearity")) +
      theme_bw() +
      theme(plot.subtitle=element_text(size=9))
  }


  # homoscedasticity
  slplot <- spread_plot(x, alpha=alpha, span=span)


  #multicolinearity
  vifplot <- NULL
  if (length(stats::coef(x)) > 2){
    vifplot <- vif_plot(x)
    #print(vifplot)
  }

  influenceplot <- influence_plot(x, alpha)

  # output graphs
  if(plot){
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
    print(qqplot)
    print(crplots)
    print(slplot)
    print(vifplot)
    print(influenceplot)
  }


  results <- list(qqplot,
                  crplots,
                  slplot,
                  vifplot,
                  influenceplot)

  invisible(results)
}
