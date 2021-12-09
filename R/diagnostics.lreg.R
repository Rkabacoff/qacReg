#'@title Diagnostics for Logistic Regression
#'
#'@description
#'
#'Provides diagnostics tests and graphs for a logistic regression model
#'
#'
#'@param x an object of class \code{lreg}
#'
#'@import car
#'
#'@export
#'
#'@details
#'Provides diagnostics including a test of
#'multicollinearity (\code{\link[car]{vif}}),
#'a graph for evaluating linearity (\code{\link[car]{ceresPlots}}),
#'a test for identifying outliers (\code{\link[car]{outlierTest}}),
#'and a graph for evaluating influential observations
#'(\code{\link[car]{influencePlot}}).
#'
#'@seealso \code{\link[qacReg]{diagnostics}}, \code{\link[car]{vif}},
#' \code{\link[car]{ceresPlots}}, \code{\link[car]{outlierTest}}, \code{\link[car]{influencePlot}}
#'
#'
#'@return NULL
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- lreg(am ~ hp + wt + mpg, mtcars)
#'diagnostics(fit)


diagnostics.lreg <- function(x){
  require(car)

    #heading for brief diagnostics

  cat("--------------- \n DIAGNOSTICS FOR LOGISTIC REGRESSION \n")


  # Multicolinearity
  cat("--------------- \n")
  cat("MULTICOLINEARITY \n")
  cat("--------------- \n")
  cat("Is there multicolinearity among any regressors? \n",
      ". GVIF Values above 5 suggest there is some multicolinearity \n",
      ". GVIF Values above 10 suggest strong multicolinearity",
      "\n")
  cat("VIF Test: \n")
  print(vif(x))

  #Linearity

  suppressWarnings(ceresPlots(x))
  mtext("Note: non-linearity is present if residuals do not match blue or purple line",
        line=4, side=1, cex=0.65, adj=-0.1)


  # Outliers
  cat("--------------- \n")
  cat("OUTLIERS \n")
  cat("--------------- \n")
  cat("Are there any outliers? \n")
  print(outlierTest(x))

  # Influential Observations

  cutoff <- 4/(nrow(x$model)-length(x$coefficients)-2)
  plot(x, which=4, cook.levels=cutoff)
  abline(h=cutoff, lty=2, col="red")
  mtext("Note: anything above the red-dotted line could be considered an outlier",
        line=4, side=1, cex=0.65, adj=-0.2)

  cat("--------------- \n")
  cat("INFLUENTIAL OBSERVATIONS \n")
  cat("--------------- \n")
  cat("What are the influential observations? \n")

  influencePlot(x, main="Influence Plot: Assessment for Influential Obs.")
  mtext("Note: influential observations have a disproportionate impact on the model",
        line=4, side=1, cex=0.65, adj=-0.1)
}
