#' @title Model Performance
#'
#' @description
#' Indices of model performance for linear and logistic regression
#' models.
#'
#' @details
#' details go here.
#'
#' @param x an object of type \code{"lm"} or \code{"glm"}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The results of the methods \code{\link{performance.lm}} or
#' \code{\link{performance.glm}}
#' @export
#'
#' @examples
#' #######################
#' # multiple regression #
#' #######################
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' performance(fit)
#'
#' #######################
#' # logistic regression #
#' #######################
#' fit2 <- glm(caesarian ~ age + bp + delivery.time, family = binomial, data = caesarian)
#' performance(fit2)
#'

performance <- function(x, ...) UseMethod("performance")
