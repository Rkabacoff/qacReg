#' @title Regression Diagnostics
#'
#'
#' @description
#' Regression diagnostics for linear and generalized linear models.
#' Currently linear and logistic regression models are supported.
#'
#' @details
#' The \code{diagnostics} function produces data visualizations
#' (ggplot2 graphs) for regression diagnostics. The plots depend
#' on the form of the model (\code{lm} or \code{glm}).
#'
#'
#' @param x an object of type \code{"lm"} or \code{"glm"}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The results of the methods \code{\link{diagnostics.lm}} or \code{\link{diagnostics.glm}}
#' @export
#' @seealso
#' \code{\link{diagnostics.lm}}, \code{\link{diagnostics.glm}}
#' @examples
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' diagnostics(fit)
diagnostics <- function(x, ...) UseMethod("diagnostics")
