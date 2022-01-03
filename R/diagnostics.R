#' @title diagnostics:
#'
#'
#' Run and print regression diagnostics
#'
#'
#' @param x an object of type \code{"lm"} or \code{"glm"}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The results of the methods \code{\link{diagnostics.lm}} or \code{\link{diagnostics.glm}}
#' @export
#' @examples
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' diagnostics(fit)
diagnostics <- function(x, ...) UseMethod("diagnostics")
