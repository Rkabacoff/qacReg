#' @title Extended model summary
#'
#'
#' @description
#' Extended summary information for model fits. Currently linear and logistic
#' regression models are supported.
#'
#' @details
#' \code{info} is a generic S3 function providing detailed model information.
#' The goal is to provide more extensive information than currently
#' produced by \code{\link{summary}}. See the related methods for
#' details.
#'
#' @seealso
#' \code{\link{info.lm}}, \code{\link{info.glm}}.
#'
#' @param x an object of type \code{lm} or \code{glm}. If \code{glm}, then
#' \code{family} must be \code{binomial}.
#'
#' @return A \code{list} produced by \code{\link{info.lm}} or
#' \code{\link{info.glm}}.
#' @export
#'
#' @examples
#' # multiple regression
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' info(fit)
#'
#' # logistic regression
#' mtcars$am <- factor(mtcars$am)
#' fit2 <- glm(am ~ hp + wt + mpg, family=binomial, mtcars)
#' info(fit2)
info <- function(x){
  UseMethod("info")
}
