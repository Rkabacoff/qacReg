#'@title Multiple Regression
#'
#'@description Fit a linear model of the form
#'  \eqn{y = \beta0 + \beta1*X1 + \beta2*X2 + ...+ \betak*Xk}.
#'
#'@param formula an object of class formula.
#'@param data a data frame.
#'@param ... arguments passed to the \code{lm} function.
#'
#'@export
#'@details
#'\code{mreg} is a wrapper for the \code{\link{lm}} function.
#'
#'@return an object of type \code{c("mreg", "lm")}
#'
#'@examples
#'fit <- mreg(mpg ~ hp + wt + am, mtcars)

mreg <- function(formula, data){
  fit <- lm(formula, data)
  fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                             ", data=", deparse(substitute(data)), ")"))
  class(fit) <- c("mreg", "lm")
  return(fit)
}

