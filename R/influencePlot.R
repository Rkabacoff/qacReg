#' @title Regression Influence Plot
#'
#' @description
#' This function creates a “bubble” plot of
#' studentized residuals versus hat values,
#' with size of the points representing the Cook's distance.
#'
#' @note
#' Vertical reference lines are drawn at twice and three
#' times the average hat value, horizontal reference lines at
#' -2, 0, and 2 on the studentized residual scale. The \code{n.label}
#' parameter controls the number of highest residuals, highest
#' leverage points, and most influential points to label. For
#' example \code{n.label=2}, the default, will identify the two
#' points meeting each criterion, for a maximum of 6 labeled points.
#' Points meeting more than one criterion are only labeled once.
#'
#' @param x an object of type \code{"mreg"} or \code{"lm"}.
#' @param n.labels integer; the number of points to label.
#'
#' @export
#' @import ggplot2
#' @import ggrepel
#' @import car
#'
#' @details
#' This function is a modification of the \link[car]{influencePlot} function
#' in the \code{car} package, using \code{ggplot2} rather than
#' \code{base} graphics.
#'
#' @seealso \link[car]{influencePlot}
#'

gginfluencePlot <- function(x, n.labels=2){
  x <- fit$model

  x$rstudent <- rstudent(fit)
  x$hat <- hatvalues(fit)
  x$cooksd <- cooks.distance(fit)
  x$influential <- ifelse(x$cooksd < .5, 0,
                          ifelse(x$cooksd < 1, 1, 2))
  x$influential <- factor(x$influential,
                          levels=c(0, 1, 2),
                          labels=c("no","possibly", "likely"))
  p <- length(coef(fit))
  n <- sum(!is.na(x$rstudent))


  which.rstud <- order(abs(x$rstudent), decreasing = TRUE)[1:n.labels]
  which.cook <- order(x$cooksd, decreasing = TRUE)[1:n.labels]
  which.hatval <- order(x$hat, decreasing = TRUE)[1:n.labels]
  #which.all <- union(x$which.rstud, union(which.cook, which.hatval))
  which.all <- unique(c(which.rstud, which.cook, which.hatval))


  x2 <- x[which.all, c("hat", "rstudent", "influential")]
  x2$label <- row.names(x2)

  library(ggplot2)
  library(ggrepel)

  p <- ggplot(x,
         aes(x = hat, y = rstudent, size = cooksd,
             color=influential)) +
    geom_point(alpha = .5) +
    scale_color_manual(values=c("cornflowerblue", "darkgreen", "red"),
                       drop=FALSE) +
    geom_text_repel(data=x2, aes(x=hat, y=rstudent, label=label),
                    color="black", size=3) +
    #scale_size_continuous(range = c(1, 8)) +
    scale_size_binned(n.breaks=5)+
    scale_y_continuous(breaks=c(-2, 0, 2)) +
    geom_hline(yintercept=c(-2, 0, 2), color="black", linetype="dashed") +
    geom_vline(xintercept=c(2*p/n, 3*p/n), color="black", linetype="dashed") +
    theme_bw() +
    labs(title="Influence Plot",
         subtitle="Assessing outliers, leverage, and influential observations",
         x = "Hat Values",
         y="Studentized Residuals",
         size = "Cook's D",
         color="Influential?",
         caption = "Point size represents Cook's D.\nLarger points are more influential.")
  return(p)
}
