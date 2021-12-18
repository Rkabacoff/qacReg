#' @title Variance Inflation Plot
#'
#' @description
#' Plots the variance inflations factors (VIF) for a linear
#' model.
#'
#' @param x an object of type \code{"mreg"} or \code{"lm"}.
#'
#' @export
#' @import ggplot2
#' @import car
#'
#' @details
#' This function plots results from the \link[car]{vif} function.
#' If all terms in an unweighted linear model have 1 df, then the usual variance-inflation factors are calculated.
#' If any terms in an unweighted linear model have more than 1 df,
#' then generalized variance-inflation factors (Fox and Monette, 1992)
#' are calculated.
#'
#' @references
#' Fox, J. and Monette, G. (1992) Generalized collinearity diagnostics.
#' JASA, 87, 178–183.
#'
#' @seealso \link[car]{vif}
#'
ggvif <- function(x){
  VIF <- car::vif(x)
  if(!is.null(dim(VIF))){
    VIF <- VIF[,1]
  }
  df <- data.frame(var=names(VIF), value=as.numeric(VIF))
  df$flag <- factor(ifelse(df$value > 5, 1, 0))
  p <- ggplot(df, aes(x=value, y=var, color=flag)) +
    geom_point(size=2) +
    geom_segment(aes(x=0, xend=value, y=var, yend=var)) +
    geom_vline(xintercept=c(5, 10), color="grey",
               linetype="dashed") +
    scale_color_manual(values=c("black", "red")) +
    labs(subtitle="Assessing multicollinearity",
         title="Variance Inflation Factors",
         y = "",
         x="VIF",
         caption="Variables with VIF > 5 or 10 may be multicolinear") +
    theme_bw() +
    theme(legend.position="none",
          plot.subtitle=element_text(size=9))
  return(p)
}
