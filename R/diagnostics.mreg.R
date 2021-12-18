#'@title Diagnostics for Multiple Regression
#'
#'@description
#'
#'Provides diagnostics tests and graphs for a linear model
#'
#'
#'@param x an object of class \code{c("mreg", "lm")}
#'@param output A parameter with two levels which indicates
#'whether to display brief diagnostics or
#'extended (additional) diagnostics.
#'
#'@export
#'
#'@import car
#'
#'@details
#'Provides diagnostics including a graph
#'for evaluating normality (\code{\link[car]{qqplot}}),
#', a graph for evaluating linearity (\code{\link[car]{crPlots}}),
#', a graph for evaluating homoscedasticity (\code{\link[car]{spreadLevelPlot}})
#'as well as a test for heteroscedasticity (\code{\link[car]{ncvTest}})
#'with a suggested power transformation when it is found.
#'In addition, if \code{output = "extended"} it provides
#'a test of multicollinearity (\code{\link[car]{vif}}),
#'a test for identifying outliers (\code{\link[car]{outlierTest}}),
#'and a graph for evaluating influential observations
#'(\code{\link[car]{influencePlot}}).
#'
#'If the plots do not look right, you may need to clear your plots
#'by clicking the broom in the plots window in the bottom right corner
#'of Rstudio
#'
#'If " Error in plot.new : figure margins too large" is returned,
#'you must expand the plots window in the bottom right pane of
#'Rstudio. The error is because this pane is not large enough for the plot,
#'so by making the plot area larger the function will work.
#'
#'
#'@seealso \code{\link[qacReg]{diagnostics}}, \code{\link[car]{vif}},
#' \code{\link[car]{qqplot}}, \code{\link[car]{outlierTest}}, \code{\link[car]{influencePlot}},
#' \code{\link[car]{crPlots}}, \code{\link[car]{spreadLevelPlot}}, \code{\link[car]{ncvTest}}
#'
#'@return NULL
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- mreg(mpg ~ wt + am + disp + hp, mtcars)
#'diagnostics(fit)
#'diagnostics(fit, output = "extended")

diagnostics.mreg <- function(x, output = c("brief", "extended")){

  output <- match.arg(output)

  heading("DIAGNOSTICS FOR MULTIPLE REGRESSION")

  # normality
  cat("Assessing normality of residuals\n")
  cat("  * Q-Q Plot\n\n")
  qqplot <- ggqqPlot(x)
  print(qqplot)

  # linearity
  cat("Assessing linearity of relationships\n")
  cat("  * Component + Residuals Plots \n\n")
  crplots <- ggcrPlots(x)
  print(crplots)

  # homoscedasticity
  cat("Assessing homoscedasticity\n")
  cat("  * Spread-Level Plot \n\n")
  slplot <- ggSpreadLevelPlot(x)
  print(slplot)

  cat("  * Score Test of Non-Constant Error Variance\n")
  ncv <- car::ncvTest(x)
  cat("    Null hypothesis: constant variance\n")
  cat("    Chi-square(" , ncv$Df, ") = ", round(ncv$ChiSquare,4) ,
      " p < ", format.pval(ncv$p, 4), "\n", sep="")

  if(ncvTest(x)$p < 0.05){
    cat("    The test suggest non-constant variance.\n")
    resid <- log(abs(rstudent(x)))
    fitval <- x$fitted.values
    mod <- suppressWarnings(MASS::rlm(log(resid) ~ log(fitval)))
    trans <- 1 - (coefficients(mod))[2]
    ptrans <- c(2, 1, .5, 0, -.5, -1, -2)
    ptranslbls <- c("y^2", "y", "sqrt(y)", "log(y)",
                    "1/sqrt(y)", "1/y", "1/y^2" )
    pos <- which.min(abs(ptrans-trans))
    cat("    A", ptranslbls[pos], "transformation may",
        "help to stabalize the variance.\n")
  }

  #multicolinearity
  cat("\n")
  cat("Assessing multicolinearity\n")
  cat("  * Variance Inflation Plot \n\n")

  vifplot <- ggvif(x)
  print(vifplot)

  #outliers
  cat("\n")
  cat("Assessing outliers, leverage, and influential observations\n")
  cat("  * Outlier Test \n")
  outliers <- outlierTest(x)
  print(outliers)
  cat("\n")
  cat("  *Influence Plot\n")

  influenceplot <- gginfluencePlot(x)
  print(influenceplot)

}
