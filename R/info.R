#' @title Summary information for lm or glm models
#'
#'
#' @description Detailed summary information of a linear or logistic
#' regression model
#'
#'
#' @param x an object of type \code{lm} or \code{glm}.
#'
#' @return The results of the methods \code{\link{info.lm}} or \code{\link{info.glm}}
#' @export
#' @examples
#' # multiple regression
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' info(fit)
#'
#' # logistic regression
#' mtcars$am <- factor(mtcars$am)
#' fit2 <- glm(am ~ hp + wt + mpg, family=binomial, mtcars)
#' info(fit2)
info <- function(x) UseMethod("info")
