#'@title Diagnostics for Multiple Regressions
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
#'
#'
#'@return NULL
#'
#'@examples
#'\dontrun{
#'myreg <- mreg(mpg ~ cyl + wt, mtcars)
#'diagnostics.mreg(myreg)
#'diagnostics.mreg(myreg, output = "extended")
#'}

diagnostics.mreg <- function(x, output = "brief"){
  require(car)

  if (output != "brief" & output != "extended") {
    stop("output must equal either brief or extended")
  }

  if (output %in% c("brief", "extended")){
    # normality
    cat("---------------",
        "\n",
        "Non-normal Observations",
        "\n",
        "Which observations seem to make the model \n violate the normality assumption?",
        "\n")

    print(qqPlot(x))

    qqPlot(x, main = "Q-Q Plot: \n Test for Normality Assumption")




    # linearity

    crPlots(x, main="", cex.lab=0.9)
    title(main="Component + Residual Plots: Check for Linearity", font.main=2, line=3.1)

    # homoscedasticity plot
    cat("--------------","\n",
        "What power transform of the dependent variable would make our model have constant conditional variance?",
        "\n")
    print(spreadLevelPlot(x, main="Spread-Level Plot: \n Checking for Homoscedasticity"))
    #spreadLevelPlot(x)


    # homoskedasticity test
    cat("------------- \n",
        "Test for Heteroskedasticity:",
        "\n")
    print(ncvTest(x))

  }

  if (output %in% c("extended")){
    cat("---------------------","\n","EXTENDED DIAGNOSTICS \n")

    #multicolinearity
    cat("---------------------","\n",
        "Is there multicolinearity among any regressors?",
        "\n")
    print(vif(x)[ , 1] > 10)

    # outliers
    cat("---------------","\n",
        "Are there any outliers?",
        "\n")
    print(outlierTest(x))

    # influential observations
    cat("---------------","\n",
        "What are the influential observations?",
        "\n")

    print(influencePlot(x, main="Influence Plot: Checking for Influential Observations"))

    #influencePlot(x)

  }
}
