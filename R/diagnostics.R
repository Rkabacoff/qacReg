#' @title diagnostics:
#'
#'
#' Run and print regression diagnostics
#'
#'
#' @param x an object of type \code{mreg} or \code{lreg}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The results of the methods \code{\link{diagnostics.mreg}} or \code{\link{diagnostics.lreg}}
#' @export
#' @examples
#' fit <- mreg(mpg ~ ., mtcars)
#' diagnostics(fit)
diagnostics <- function(x, ...) UseMethod("diagnostics")
