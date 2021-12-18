#' @title Normal Q-Q Plot
#'
#' @description
#' Plots empirical quantiles of the studentized residuals
#' from a linear model against the theoretical quantiles of
#' a the normal distribution.
#'
#' @param x an object of type \code{"mreg"} or \code{"lm"}.
#' @param reps integer; number of bootstrap replications for
#' confidence envelope.
#' @param conf size of confidence interval (default=\code{0.95}).
#' @param n.labels integer; the number of largest residuals to label.
#'
#' @export
#' @import ggplot2
#' @importFrom MASS rlm
#' @import ggrepel
#'
#' @details
#' This function is a modification of the \link[car]{qqPlot} function
#' in the \code{car} package, using \code{ggplot3} rather than
#' \code{base} graphics.
#'
#' @seealso \link[car]{qqPlot}, \link[stats]{qqnorm}
#'
ggqqPlot <- function(x, reps=100, conf=0.95, n.labels=3){

  if(!inherits(x, "mreg") &
     !inherits(x, "lm")) stop("x must  be class 'mreg' or 'lm'")
  x <- update(x, na.action = "na.exclude")

  labels <- names(residuals(x))
  rstudent <- rstudent(x)
  index <- seq(along = rstudent)
  sumry <- summary.lm(x)
  res.df <- sumry$df[2]

  good <- !is.na(rstudent)
  rstudent <- rstudent[good]
  labels <- labels[good]
  n <- length(rstudent)
  ord <- order(rstudent)
  ord.x <- rstudent[ord]
  ord.lab <- labels[ord]

  P <- ppoints(n)
  z <- qt(P, df = res.df - 1)

  yhat <- na.omit(fitted.values(x))
  S <- sumry$sigma
  Y <- matrix(yhat, n, reps) +
    matrix(rnorm(n * reps, sd = S), n, reps)
  X <- model.matrix(x)
  rstud <- apply(rstudent(lm(Y ~ X - 1)), 2, sort)

  lower <- apply(rstud, 1, quantile, prob = (1 - conf)/2)
  upper <- apply(rstud, 1, quantile, prob = (1 + conf)/2)
  df <- data.frame(labels=ord.lab, z=z, ord.x=ord.x, lower=lower, upper=upper)

  # which points to label
  df$absres <- abs(df$ord.x)
  df2 <- tail(df[order(df$absres),], n.labels)

  coef <- coefficients(MASS::rlm(ord.x ~ z))
  a <- coef[1]
  b <- coef[2]

  p <- ggplot(df) +
    geom_point(aes(x=z, y=ord.x)) +
    # geom_line(aes(x=z, y=lower), color="red", linetype="dashed") +
    # geom_line(aes(x=z, y=upper), color="red", linetype="dashed") +
    geom_ribbon(aes(x=z, ymin=lower, ymax=upper), fill="grey70", alpha=.6) +
    geom_abline(intercept=a, slope=b, color="blue") +
    geom_text_repel(data=df2, aes(x=z, y=ord.x, label=labels), size=3) +
    labs(title="Normal Q-Q Plot",
         x = "t Quantiles",
         y = "Studentized Residuals",
         subtitle = "Assessing normality of residuals",
         caption = "Normally distributed residuals will cluster around the line.") +
    theme_bw()

  return(p)
}
