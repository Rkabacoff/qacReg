#' @title Component+Residual Plots
#'
#' @description
#' Produces component+residual (partial residual) plots for
#' linear models.
#'
#' @note
#' The linear model can contain factors but not interactions. Plots are
#' based on all predictors, but only the numeric variables are plotted. Each plot
#' has a linear and loess fit line.
#'
#' @param x an object of type \code{"lm"}.
#' @param alpha numeric; degree of transparency for points (0 to 1, default=0.4)
#' @param span numeric; the  degree of smoothing for loess lines (default=0.75)
#'
#' @export
#' @import ggplot2
#' @import patchwork
#' @importFrom stats terms
#'
#' @details
#' This function is a modification of the \link[car]{crPlots} function
#' in the \code{car} package, using \code{ggplot2} rather than
#' \code{base} graphics.
#'
#' @return a \code{ggplot2} graph
#'
#' @seealso \link[car]{crPlot}, \link[car]{crPlots}
#' @examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- lm(mpg ~ wt + am + disp + hp, mtcars)
#'cr_plots(fit)

#'
cr_plots <- function(x, alpha=.4, span=.75){

  # numeric predictors
  findnumeric <- function(x){
    if(class(x)[1] %in% c("integer", "numeric") &
       length(unique(x)) > 2) {
      outcome <- TRUE} else {
        outcome <- FALSE
      }
    return(outcome)
  }
  index <- sapply(x$model[-1], findnumeric)
  yname <- names(x$model)[1]
  vars <- names(x$model[-1])[index]
  if(length(vars) < 1) stop("No numeric predictors")

  if (any(attr(terms(x), "order") > 1)) {
    stop("C+R plots not available for models with interactions.")
  }

  # slopes
  b <- x$coef[vars]

  myplots <- vector(mode="list", length=length(vars))
  names(myplots) <- vars
  partial.res <- residuals(x, "partial")
  y <- NULL # for CRAN
  for(i in vars){
    x1 <- model.frame(x)[, i]
    x2 <- partial.res[, i]

    df <- data.frame(y = as.numeric(x2),
                     x = as.numeric(x1))
    myplots[[i]] <- ggplot(df, aes(x, y)) +
      geom_point(alpha=alpha) +
      geom_smooth(method="lm", formula=y ~ x,
                  color="blue", linetype="dashed",
                  se=FALSE) +
      geom_smooth(method="loess", formula=y~x,
                  color="indianred2", linetype="solid",
                  se=FALSE, span=span) +
        labs(y="Component + Resid", x=i) +
      theme_bw() +
      theme(axis.title.y=element_text(size=8))
  }
  p <- wrap_plots(myplots) +
    plot_annotation(title="Components + Residuals Plots",
            subtitle=paste("Assessing linearity of", yname,
                           "with quantitative predictors")) &
    theme(plot.subtitle=element_text(size=9))
  return(p)
}
