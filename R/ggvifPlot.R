#' @title Variance Inflation Plot
#'
#' @description
#' Plots the variance inflations factors (VIF) for a linear
#' model.
#'
#' @param x an object of class \code{"lm"}.
#'
#' @export
#' @import ggplot2
#' @import car
#' @importFrom stats coef
#'
#' @details
#' This function plots results from the \link[car]{vif} function from
#' the \code{car} package. VIFs larger than 5 are highlighted.
#'
#' If all terms in an unweighted linear model have 1 df, then the usual variance-inflation factors are calculated.
#' If any terms in an unweighted linear model have more than 1 df,
#' then generalized variance-inflation factors (Fox and Monette, 1992)
#' are calculated.
#'
#' @references
#' Fox, J. and Monette, G. (1992) Generalized collinearity diagnostics.
#' \emph{JASA}, \emph{87}, 178–183.
#'
#' @seealso \link[car]{vif}
#'
#' @return a \code{ggplot2 graph}
#'
#' @examples
#' mtcars$am <- factor(mtcars$am)
#' fit <- lm(mpg ~ wt + am + disp + hp, mtcars)
#' ggvifPlot(fit)

ggvifPlot <- function(x){
  if (length(stats::coef(x)) < 3){
    stop("You need more than 1 predictor to calculate VIFs")
  }
  VIF <- car::vif(x)
  if(!is.null(dim(VIF))){
    VIF <- VIF[,1]
  }

  value <- flag <- NULL # for CRAN

  df <- data.frame(var=names(VIF), value=as.numeric(VIF))
  df$flag <- ifelse(df$value > 5, 1, 0)
  df$flag <- factor(df$flag, levels=c(0, 1))
  p <- ggplot(df, aes(x=value, y=var, color=flag)) +
    geom_point(size=2) +
    geom_segment(aes(x=0, xend=value, y=var, yend=var)) +
    geom_vline(xintercept=c(5, 10), color="grey",
               linetype="dashed") +
    scale_color_manual(values=c("black", "red"), drop=FALSE) +
    labs(subtitle="Assessing multicollinearity",
         title="Variance Inflation Factors",
         y = "",
         x="VIF",
         caption="Variables with VIF > 5 or 10 may be multicollinear") +
    theme_bw() +
    theme(legend.position="none",
          plot.subtitle=element_text(size=9))
  return(p)
}
