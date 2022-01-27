#'@title Marginal Effects Plots
#'
#'@description
#'The \code{plots} function generates one or more graphs displaying the relationship
#'between each predictor and the outcome, controlling for the other predictors
#'in the model.
#'
#'@param x an object of class \code{"lm"} or \code{"glm"}
#'@param points logical; If \code{TRUE}, raw data points are plotted (default = \code{FALSE})
#'@param size size of data points (default = 1)
#'@param jitter amount of jitter for points (from 0 to 1; default = .03)
#'@param alpha transparency of data points (from 0 to 1; default is 0.2)
#'@param ci logical; If \code{TRUE}, display 95% confidence intervals (default = \code{TRUE})
#'@param page logical; If \code{TRUE}, each graph will be printed separately.
#'@param ... additional arguments to be passed to
#'\code{\link[ggeffects]{plot.ggeffects}}.
#'
#'@details
#'The \code{plots} function uses the \code{\link[ggeffects]{ggeffect}} and
#'\code{\link[ggeffects]{plot.ggeffects}} functions from the  \code{ggeffects} package
#'to plot marginal effects for a linear or generalized linear model.
#'
#'A plot is created for each predictor variable (called the "focal" variable).
#'Predicted response values are calculated by varying the values of
#'the focal variable while keeping the non-focal variables
#'(the other predictor variables) constant.
#'Non-focal numeric variables are set to their means. If factors are
#'present in the model, predicted
#'values are calculated for each combination of
#'non-focal categorical variable levels and averaged based on category size. See the
#'\href{ggeffects homepage}{https://strengejacke.github.io/ggeffects/index.html}
#'for details.
#'
#'If \code{page = TRUE},
#'the individual graphs are combined into a single plot via the
#'the \code{\link[patchwork]{wrap_plots}} function in the patchwork package.
#'Otherwise, individual plots are printed.
#'
#'@section Limitations:
#'The \code{me_plots} function can not handle models that include
#'\code{as.factor} or \code{factor} in their formulas. Create the modified
#'variables prior to fitting the model
#'(e.g., \code{mtcars$cyl <- factor(mtcars$cyl)}).
#'Additionally, the function can not handle models that include interactions. In this
#'case, consider using the \code{ggeffects} package with grouping and
#'faceting.
#'
#'@return a list of \code{ggplot2} graphs.
#'
#'@seealso
#'\link[ggeffects]{ggeffect}, \link[ggeffects]{plot.ggeffects}
#'
#'@export
#'
#'@import ggeffects
#'@import ggplot2
#'@import effects
#'@import patchwork
#'@importFrom stats as.formula coef
#'@importFrom grDevices devAskNewPage
#'
#'@examples
#'#######################
#'# Multiple regression #
#'#######################
#'fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#'me_plots(fit)
#'
#'#######################
#'# Logistic regression #
#'#######################
#'fit2 <- glm(caesarian ~ age + bp + delivery.time, family = binomial, data = caesarian)
#'me_plots(fit2)


me_plots <- function(x, points = FALSE, size = 1,
                  alpha=.2, jitter=.03, ci= TRUE, page=FALSE, ...){

  # single predictor
  if (length(names(stats::coef(x))) < 3){
    # scatterplot
    final <- scatter_plot(x, alpha=alpha)
    return(final)
  }

  # create plots
  # check model
  if(any(grepl("factor\\(", names(x$model)))){
    stop("Unable to handle the factors in the model formula.\nSee ?me_plots.")
  }
  if(any(grepl("\\:", x$call[[2]]))){
    stop("Unable to handle the interactions.\nSee ?me_plots.")
  }

  # all variables except dv
  vars <- names(x$model)[names(x$model) != x$terms[[2]]]
  # drop variables with formula (e.g. I(X^2))
  vars <- vars[!grepl("\\(", vars)]
  myplots <- vector(mode="list", length=length(vars))
  names(myplots) <- vars
  for(i in vars){
    myplots[[i]] <- plot(ggeffect(x, i), add.data=points,
                        dot.size=size, jitter=jitter,
                        dot.alpha=alpha, ci=ci, ...)+ labs(title="")
  }

  # produce combined graphs
  if(!page){
    if (length(vars) > 1){
      caption <- "Plots show the relationship of each explanatory variable to\nthe outcome controlling for the other explanatory variables."
    } else {
      caption <- ""
    }

    final <- wrap_plots(myplots) +
      plot_annotation(title="Marginal Effects Plots", caption=caption) &
      theme(plot.caption=element_text(size=8))
    return(final)
  }

  # separate graphs
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
    for (i in names(myplots)){
      plot(myplots[[i]])
    }
    invisible(myplots)
}


