#' @title Regression Influence Plot
#'
#' @description
#' This function creates a “bubble” plot of
#' studentized residuals versus hat values,
#' with size of the points representing Cook's distances.
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
#' @param x an object of type \code{"lm"}.
#' @param n.labels integer; the number of points to label (default=2).
#' @param alpha numeric; transparency of points (0 to 1, default=0.4).
#'
#' @export
#' @import ggplot2
#' @import ggrepel
#' @import car
#' @importFrom stats coef cooks.distance hatvalues
#'
#' @details
#' This function is a modification of the \link[car]{influencePlot} function
#' in the \code{car} package, using \code{ggplot2} rather than
#' \code{base} graphics.
#'
#' @note
#' Color is used to identify points that are not influential (D < 0.5),
#' possibly influential (0.5 <= D < 1), and likely to be influential (D >= 1).
#'
#' @seealso \link[car]{influencePlot}
#'
#' @return a \code{ggplot2} graph
#'
#' @examples
#' mtcars$am <- factor(mtcars$am)
#' fit <- lm(mpg ~ wt + am + disp + hp, mtcars)
#' influence_plot(fit)

influence_plot <- function(x, alpha=0.4, n.labels=2){

  cooksd <- influential <- label <- NULL # for CRAN

  df <- x$model

  df$rstudent <- rstudent(x)
  df$hat <- stats::hatvalues(x)
  df$cooksd <- stats::cooks.distance(x)
  df$influential <- ifelse(df$cooksd < .5, 0,
                          ifelse(df$cooksd < 1, 1, 2))
  df$influential <- factor(df$influential,
                          levels=c(0, 1, 2),
                          labels=c("no","possibly", "likely"))
  p <- length(stats::coef(x))
  n <- sum(!is.na(df$rstudent))


  which.rstud <- order(abs(df$rstudent), decreasing = TRUE)[1:n.labels]
  which.cook <- order(df$cooksd, decreasing = TRUE)[1:n.labels]
  which.hatval <- order(df$hat, decreasing = TRUE)[1:n.labels]
  #which.all <- union(df$which.rstud, union(which.cook, which.hatval))
  which.all <- unique(c(which.rstud, which.cook, which.hatval))


  df2 <- df[which.all, c("hat", "rstudent", "influential")]
  df2$label <- row.names(df2)

  p <- ggplot(df,
         aes(x = hat, y = rstudent, size = cooksd,
             fill=influential)) +
    geom_point(alpha = alpha, shape=21, color="black") +
    scale_fill_manual(values=c("cornflowerblue", "darkgreen", "red"),
                       drop=FALSE) +
    geom_text_repel(data=df2, aes(x=hat, y=rstudent, label=label),
                    color="black", size=3) +
    #scale_size_continuous(range = c(1, 8)) +
    scale_size_binned(n.breaks=5)+
    scale_y_continuous(breaks=c(-2, 0, 2)) +
    geom_hline(yintercept=c(-2, 0, 2), color="black", linetype="dashed") +
    geom_vline(xintercept=c(2*p/n, 3*p/n), color="black", linetype="dashed") +
    theme_bw() +
    theme(plot.subtitle=element_text(size=9)) +
    labs(title="Influence Plot",
         subtitle="Assessing outliers, leverage, and influential observations",
         x = "Hat Values",
         y="Studentized Residuals",
         size = "Cook's D",
         fill="Influential?",
         caption = "Point size represents Cook's D.\nLarger points are more influential.")
  return(p)
}
