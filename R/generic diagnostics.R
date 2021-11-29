#' @title diagnostics
#'
#' Run and print regression diagnostics
#'
#' @param x an object of type \code{mreg}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The results of
#' @export
#' @examples
#' fit <- mreg(mpg ~ ., mtcars)
#' diagnostics(fit)
diagnostics <- function(x, ...) UseMethod("diagnostics")
