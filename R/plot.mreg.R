#'@title Plotting multiple regression
#'
#'@description
#'The plot function provides a single plot that shows separate graphs of the relationship
#'between each predictor and the outcome, controlling for the other predictors from the
#'mreg function
#'
#'@param x an object of type \code{c("mreg", "lm")}
#'@param points points is TRUE if data points are to be shown, default is FALSE
#'
#'@return a plot of class \code{ggplot}
#'
#'
#'@export
#'
#'
#'@import ggeffects
#'@import ggplot2
#'@import effects
#'@import patchwork
#'
#'@examples
#'fit <- mreg(mpg ~ hp + wt + am, mtcars)
#'plot(fit)

plot.mreg <- function(x, points = FALSE){
  vars <- names(x$model)[names(x$model) != x$terms[[2]]]
  myplots <- vector(mode="list", length=length(vars))
  names(myplots) <- vars
  for(i in vars){
    myplots[[i]] <- plot(ggeffect(x, i), add.data=points, dot.size=1, dot.alpha=.1)+ labs(title="")
  }
  final <- wrap_plots(myplots) + plot_annotation(title="Effects Plots",
                                                 subtitle="Each variable controlling for the others")
  return(final)
}
