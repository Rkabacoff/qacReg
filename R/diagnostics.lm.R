#'@title Diagnostics for Linear Models
#'
#'@description
#'
#'Provides diagnostics tests and graphs for a linear model
#'
#'
#'@param x an object of class \code{c("lm")}
#'@param alpha numeric; transparency for plot points (default=0.4)
#'@param span numeric; smoothing parameter for loess fit lines (default=0.8)
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
#'diagnostic graphing functions and tests. The function evaluates
#'the following:
#'\describe{
#'  \item{Normality}{Normality of the (studentized) residuals is assessed
#'  via a Q-Q plot (\link{ggqqPlot}), and Robust Jarque Bera Test
#'   (see notes).}
#'  \item{Linearity}{Linearity of the explanatory-response relationship
#'  are assessed via Component + Residual (partial residual) plots
#'  (\link{ggcrPlots}). If there is a single predictor, a scatter plot
#'  with linear and loess lines is produced.}
#'  \item{Constant variance}{Homoscedasticity is evaluated via
#'  a Spread-Level plot (\link{ggspreadLevelPlot}) and a Scores
#'  Test for Non-constant Error Variance. (\link[car]{ncvTest}),
#'  If the test is significant, a transformation of the response
#'  variable to stabilize the variance is offered.}
#'  \item{Multicollinearity}{Variance inflation factors (\link[car]{vif})
#'  are plotted. If there is a single predictor variable, this section
#'  is skipped.}
#'  \item{Outliers, leverage, and influence}{An influence plot
#'  (\link{gginfluencePlot}) and Bonferroni Outlier Test
#'  (\link[car]{outlierTest}) are provided.}
#'}
#'
#'@note The Robust Jarque Bera Test is based on the test provided is
#'the \code{DescTools} package. See \link[DescTools]{JarqueBeraTest}
#'for details.

#'@seealso \code{\link{diagnostics}}, \code{\link[car]{vif}},
#' \code{\link[car]{qqPlot}}, \code{\link[car]{outlierTest}}, \code{\link[car]{influencePlot}},
#' \code{\link[car]{crPlots}}, \code{\link[car]{spreadLevelPlot}}, \code{\link[car]{ncvTest}}
#'
#'@return a list containing the \code{ggplot2} graphs and statistical tests
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- lm(mpg ~ wt + am + disp + hp, mtcars)
#'diagnostics(fit)

diagnostics.lm <- function(x, alpha=.4, span=.8, ...){


  # heading("DIAGNOSTICS FOR MULTIPLE REGRESSION")
  # cat("\n")
  # heading("Normality")


  # normality
  qqplot <- ggqqPlot(x, alpha=alpha)
  # print(qqplot)

  # normality.test(x)
  # cat("\n")

  # linearity
  if (length(stats::coef(x)) > 2){
    crplots <- ggcrPlots(x, alpha=alpha, span=span)
    # print(crplots)
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
    # print(crplots)
  }


  # homoscedasticity
  # heading("Homoscedasticity")
  slplot <- ggspreadLevelPlot(x, alpha=alpha, span=span)
  #print(slplot)

  # cat("Score Test of Non-Constant Error Variance\n")
  # ncv <- car::ncvTest(x)
  # cat("Null hypothesis: constant variance\n")
  # cat("Chi-square(" , ncv$Df, ") = ", round(ncv$ChiSquare,4) ,
  #     " p < ", format.pval(ncv$p, 4), "\n", sep="")

  # trans <- NULL
  # if(ncvTest(x)$p < 0.05){
  #   cat("The test suggest non-constant variance.\n")
  #   resid <- log(abs(rstudent(x)))
  #   fitval <- x$fitted.values
  #   mod <- suppressWarnings(MASS::rlm(log(resid) ~ log(fitval)))
  #   trans <- 1 - (stats::coef(mod))[2]
  #   ptrans <- c(2, 1, .5, 0, -.5, -1, -2)
  #   ptranslbls <- c("y^2", "y", "sqrt(y)", "log(y)",
  #                   "1/sqrt(y)", "1/y", "1/y^2" )
  #   pos <- which.min(abs(ptrans-trans))
  #   cat("A", ptranslbls[pos], "transformation may",
  #       "help to stabilize the variance.\n")
  # }
  #
  #  cat("\n")

  #multicolinearity
  # heading("Multicollinearity")
  # print(car::vif(x))
  vifplot <- NULL
  if (length(stats::coef(x)) > 2){
    vifplot <- ggvifPlot(x)
    #print(vifplot)
  }


  #outliers
  # cat("\n")

  influenceplot <- gginfluencePlot(x, alpha)
  #print(influenceplot)

  # heading("Outliers")
  # outliers <- outlierTest(x)
  # print(outliers)

  # output graphs
  oask <- grDevices::devAskNewPage(TRUE)
  on.exit(grDevices::devAskNewPage(oask))
  print(qqplot)
  print(crplots)
  print(slplot)
  print(vifplot)
  print(influenceplot)

  results <- list(qqplot,
                  crplots,
                  slplot,
                  # ncv,
                  # trans,
                  vifplot,
                  influenceplot)

  invisible(results)
}
