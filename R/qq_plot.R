#' @title Normal Q-Q Plot
#'
#' @description
#' Plots empirical quantiles of the studentized residuals
#' from a linear model against the theoretical quantiles of
#' a the normal distribution.
#'
#' @param x an object of class \code{"lm"}.
#' @param reps integer; number of bootstrap replications for the
#' confidence envelope (default=100)
#' @param conf numeric; size of confidence interval (default=\code{0.95}).
#' @param n.labels integer; the number of largest residuals to label (
#' default = 3).
#' @param alpha numeric; transparency for plotted points
#' (0 to 1, default=0.4).
#'
#' @export
#' @import ggplot2
#' @importFrom MASS rlm
#' @importFrom stats coef fitted.values model.matrix na.omit summary.lm
#' ppoints qt quantile residuals rstudent rnorm update
#' @importFrom utils tail
#' @import ggrepel
#'
#' @details
#' The function is a modification of the \link[car]{qqPlot} function
#' in the \code{car} package, using \code{ggplot2} rather than
#' \code{base} graphics. A robust linear regression line
#' is provided by the \link[MASS]{rlm} function in the \code{MASS}
#' package.
#'
#' @seealso \link[car]{qqPlot}, \link[stats]{qqnorm}, \link[MASS]{rlm}
#'
#' @return a \code{ggplot2} graph
#'
#' @examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- lm(mpg ~ wt + am + disp + hp, mtcars)
#'qq_plot(fit)

qq_plot <- function(x, reps=100, conf=0.95, n.labels=3, alpha=.4){

  if(!inherits(x, "lm")) stop("x must  be class 'lm'")
  x <- stats::update(x, na.action = "na.exclude")

  labels <- names(stats::residuals(x))
  rstudent <- stats::rstudent(x)
  index <- seq(along = rstudent)
  sumry <- stats::summary.lm(x)
  res.df <- sumry$df[2]

  good <- !is.na(rstudent)
  rstudent <- rstudent[good]
  labels <- labels[good]
  n <- length(rstudent)
  ord <- order(rstudent)
  ord.x <- rstudent[ord]
  ord.lab <- labels[ord]

  P <- stats::ppoints(n)
  z <- stats::qt(P, df = res.df - 1)

  #yhat <- na.omit(fitted.values(x)) # deal with miss rstudent here
  yhat <- stats::fitted.values(x)[good]
  S <- sumry$sigma
  Y <- matrix(yhat, n, reps) +
    matrix(stats::rnorm(n * reps, sd = S), n, reps)
  X <- stats::model.matrix(x)[good,]
  rstud <- apply(rstudent(lm(Y ~ X - 1)), 2, sort)

  lower <- apply(rstud, 1, stats::quantile, prob = (1 - conf)/2)
  upper <- apply(rstud, 1, stats::quantile, prob = (1 + conf)/2)
  df <- data.frame(labels=ord.lab, z=z, ord.x=ord.x, lower=lower, upper=upper)

  # which points to label
  df$absres <- abs(df$ord.x)
  df2 <- utils::tail(df[order(df$absres),], n.labels)

  coef <- stats::coef(suppressWarnings(MASS::rlm(ord.x ~ z)))
  a <- coef[1]
  b <- coef[2]

  p <- ggplot(df) +
    geom_ribbon(aes(x=z, ymin=lower, ymax=upper), fill="grey70", alpha=.6) +
    geom_abline(intercept=a, slope=b, color="blue") +
    geom_point(aes(x=z, y=ord.x), alpha=alpha) +
    geom_text_repel(data=df2, aes(x=z, y=ord.x, label=labels), size=3) +
    labs(title="Normal Q-Q Plot",
         x = expression(paste(italic("t"), " Quantiles")),
         y = "Studentized Residuals",
         subtitle = "Assessing normality of residuals",
         caption = "Normally distributed residuals should cluster around the line.") +
    theme_bw() +
    theme(plot.subtitle=element_text(size=9))

  return(p)
}
