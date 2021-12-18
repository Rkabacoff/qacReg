#' @title Spread-Level Plot
#'
#' @description
#' Creates a plot for examining the possible dependence of
#' spread on level using the studentized residuals from a linear
#' model.
#'
#' @param x an object of type \code{"mreg"} or \code{"lm"}.
#' @param n.labels integer; the number of largest residuals to label.
#'
#' @export
#' @import ggplot2
#' @importFrom MASS rlm
#' @import ggrepel
#'
#' @details
#' This function is a modification of the \link[car]{spreadLevelPlot} function
#' in the \code{car} package, using \code{ggplot2} rather than
#' \code{base} graphics.
#'
#' @note
#' The graph plots the log of the fitted values against the log
#' of the absolute studentized residuals. A robust linear fit line
#' and a loess fit line are also plotted.
#'
#' @seealso \link[car]{spreadLevelPlot}
#'

ggSpreadLevelPlot <- function(x, n.labels=3){
  require(ggplot2)
  y <- log(abs(rstudent(x)))
  x <- log(x$fitted)
  df <- data.frame(x=x, y=y)
  df <- na.omit(df)

   # which points to label
  df$absres <- abs(df$y)
  df2 <- tail(df[order(df$absres),], n.labels)

  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    geom_smooth(method=MASS::rlm, formula= y~x, se=FALSE, color="blue", linetype="dashed") +
    geom_smooth(se=FALSE, method="loess", formula=y~x, color="indianred2") +
    geom_text_repel(data=df2, aes(x=x, y=y, label=row.names(df2)), size=3) +
    theme_bw() +
    labs(x="log ( Fitted Values )",
         y="log | Studentized Residuals |",
         title="Spread-Level Plot",
         subtitle="Assessing constant variance",
         caption="For homoscedastic data, points should distribute evenly around a horizontal line")
  return(p)
  }


