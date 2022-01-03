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
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' info(fit)
info <- function(x) UseMethod("info")
