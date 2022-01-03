#' @title Model Performance
#'
#'
#' Produce indices of model performance
#'
#'
#' @param x an object of type \code{"lm"} or \code{"glm"}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The results of the methods \code{\link{performance.lm}} or
#' \code{\link{performance.glm}}
#' @export
#' @examples
#' # multiple regression
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' performance(fit)
#'
#' # logistic regression
#' # fit <- lm(am ~ hp + wt, mtcars)
#' # performance(fit)
performance <- function(x, ...) UseMethod("performance")
