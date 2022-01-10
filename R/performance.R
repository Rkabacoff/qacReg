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
#' fit1 <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' performance(fit1)
#'
#' # logistic regression
#' fit2 <- glm(am ~ hp + wt, family=binomial, mtcars)
#' performance(fit2)
performance <- function(x, ...) UseMethod("performance")
