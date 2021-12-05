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
#'@details
#'
#'Provides diagnostics including a graph
#'for evaluating normality (\code{qqplot}),
#', a graph for evaluating linearity (\code{crPlots}),
#', a graph for evaluating homoscedasticity
#'(\code{spreadLevelPlot}), and a test for
#'evaluating homoscedasticity (\code{ncvTest}).
#'In addition, if \code{output = "extended"} it provides
#'a test of multicollinearity (\code{vif}),
#'a test for identifying outliers (\code{outlierTest}),
#'and a graph for evaluating influential observations
#'(\code{influencePlot}).
#'
#'If the plots do not look right, you may need to clear your plots
#'by clicking the broom in the plots window in the bottom right corner
#'of Rstudio
#'
#'If " Error in plot.new() : figure margins too large" is returned,
#'you must expand the plots window in the bottom right pane of
#'Rstudio. The error is because this pane is not large enough for the plot,
#'so by making the plot area larger the function will work.
#'
#'
#'
#'
#'@return NULL
#'
#'@examples
#'fit <- mreg(mpg ~ cyl + wt, mtcars)
#'diagnostics(fit)
#'diagnostics(fit, output = "extended")

diagnostics.mreg <- function(x, output = "brief"){
  require(car)

  if (output != "brief" & output != "extended") {
    stop("output must equal either brief or extended")
  }

  if (output %in% c("brief", "extended")){

    #heading for brief diagnostics

    cat("---------------------",
        "\n",
        "DIAGNOSTICS FOR MULTIPLE REGRESSION \n",
        "--------------------- \n")

    qqPlot(x, main = "Q-Q Plot: \n Test for Normality Assumption",
           xlab="t Quantiles", ylab="Studentized Residuals")

    mtext("Note: observations should lie approximately on the straight line to meet normality",
          line=4, side=1, cex=0.65, adj=-0.1)





    # linearity

    crPlots(x, main="Components + Residuals Plots: Test for Linearity Assumption")
    mtext("Note: numerical variables should approximately match with the blue dotted-line to meet linearity",
          side=1, line=4,  adj=-0.2, cex=.65)

    # homoscedasticity test
    cat("Test for Heteroskedasticity:","\n")

    print(ncvTest(x))

    if(ncvTest(x)$p < 0.05) cat("\n The test suggests that there is Heteroskedasticity \n")

    else
      cat("\n The test suggests the model may satistfy the Homoskedasticity assumption \n")

    #power transformation
    power <- spreadLevelPlot(x,main="Spread-Level Plot: \n Test for Homoskedasticity")
    mtext("Note: homogeneity of variance is met if the residuals and fitted values exhibit a horizontal line",
          line=4, side=1, cex=0.65, adj=-0.1)
    cat("\n What power transformation of the dependent variable \n would make our model have constant conditional variance? \n")
    cat("Suggested Variance-Stabilizing Power Transformation:", power[[1]], "\n")

  }

  if (output %in% c("extended")){
    cat("\n",
        "---------------------",
        "\n",
        "EXTENDED DIAGNOSTICS \n",
        "--------------------- \n")

    #multicolinearity
    cat("Is there multicolinearity among any regressors? \n",
        "GVIF Values above 5 suggest there is some multicolinearity \n",
        "GVIF Values above 10 suggest strong multicolinearity \n")
    print(vif(x))


    # outliers
    cat("\n",
        "---------------","\n",
        "Are there any outliers?",
        "\n")
    print(outlierTest(x))

    # influential observations
    cat("\n",
        "--------------- \n What are the influential observations? \n")

    print(influencePlot(x, main="Influence Plot: \n  Assessment for Influential Observations"))
    mtext("Note: Influential observations have disproprotionate impact on the model",
          line=4, side=1, cex=0.65, adj=-0.1)

  }
}
