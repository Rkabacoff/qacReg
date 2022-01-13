#' @title Spread-Level Plot
#'
#' @description
#' Creates a plot for examining the possible dependence of
#' spread on level using the studentized residuals from a linear
#' model.
#'
#' @param x an object of class \code{"lm"}.
#' @param alpha numeric; degree of transparency for points (0 to 1, default=0.4).
#' @param n.labels integer; the number of largest residuals to label
#' (default=0).
#' @param span numeric; smoothing parameter for loess fit line (default=0.75)
#'
#' @export
#' @import ggplot2
#' @importFrom MASS rlm
#' @import ggrepel
#' @importFrom stats na.omit
#' @importFrom utils tail
#'
#' @details
#' This function is a modification of the \link[car]{spreadLevelPlot} function
#' in the \code{car} package, using \code{ggplot2} rather than
#' \code{base} graphics.
#'
#' @note
#' The graph plots the fitted values against the absolute studentized
#' residuals on logarithmic scales. A robust linear fit line
#' and a loess fit line are also plotted.
#'
#' @seealso \link[car]{spreadLevelPlot}, \link[MASS]{rlm}
#'
#' @return a \code{ggplot2} graph
#'
#' @examples
#' mtcars$am <- factor(mtcars$am)
#' fit <- lm(mpg ~ wt + am + disp + hp, mtcars)
#' ggspreadLevelPlot(fit)


ggspreadLevelPlot <- function(x, alpha=.4, n.labels=0, span=.75){
  y <- abs(rstudent(x))
  x <- x$fitted
  df <- data.frame(x=x, y=y)
  df <- na.omit(df)

  # remove nonpositive fitted values
  non.pos <- x <= 0
  if (any(non.pos)) {
    df <- df[!non.pos, ]
    n.non.pos <- sum(non.pos)
    warning("\n", n.non.pos, " negative",
            if (n.non.pos > 1) " fitted values"
      else " fitted value", " removed")
  }

   # which points to label
  df$absres <- abs(df$y)
  df2 <- utils::tail(df[order(df$absres),], n.labels)

  p <- ggplot(df, aes(x, y)) +
    geom_point(alpha=alpha) +
    geom_smooth(method=MASS::rlm, formula= y~x, se=FALSE, color="blue", linetype="dashed") +
    geom_smooth(se=FALSE, method="loess", formula=y~x, color="indianred2",
                span=span) +
    geom_text_repel(data=df2, aes(x=x, y=y, label=row.names(df2)), size=3) +
    theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_x_continuous(trans="log10") +
    theme(plot.subtitle=element_text(size=9)) +
    labs(x="Fitted Values",
         y= "Absolute Studentized Residuals",
         title="Spread-Level Plot",
         subtitle="Assessing constant variance",
         caption="Homoscedastic data should distribute around a horizontal line")
  return(p)
  }


