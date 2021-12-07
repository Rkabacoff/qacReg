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

    cat("---------------------",
        "\n",
        "DIAGNOSTICS FOR LOGISTIC REGRESSION \n",
        "--------------------- \n")

    # Multicolinearity
    cat("Is there multicolinearity among any regressors? \n",
        "GVIF Values above 5 suggest there is some multicolinearity \n",
        "GVIF Values above 10 suggest strong multicolinearity",
        "\n")
    print(vif(x))


    # Linearity
    ceresPlots(x)


    # Outliers
    cat("\n",
        "---------------","\n",
        "Are there any outliers?",
        "\n")
    print(outlierTest(x))

    # Influential Observations

    cutoff <- 4/(nrow(x$model)-length(x$coefficients)-2)
    plot(x, which=4, cook.levels=cutoff)
    abline(h=cutoff, lty=2, col="red")

    cat("\n",
        "--------------- \n",
        "What are the influential observations? \n")

    influencePlot(x)

}
