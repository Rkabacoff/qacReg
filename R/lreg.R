#'@title Logistic Regression
#'
#'@description Fit a logistic regression model.
#'
#'@param formula an object of class formula.
#'@param data a data frame.
#'@param ... arguments passed to the \code{glm} function.
#'
#'@export
#'@details
#'\code{lreg} is a wrapper for the \code{\link{glm}} function.
#'
#'@return an object of type \code{c("lreg", "lm")}
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- lreg(am ~ hp + wt + mpg, mtcars)
#'fit
lreg <- function(formula, data){
  fit <- glm(formula, data, family=binomial)
  fit$call <- str2lang(paste("glm(formula=", deparse(substitute(formula)),
                             ", family = binomial, data=", deparse(substitute(data)), ")"))
  class(fit) <- c("lreg", "glm", "lm")
  return(fit)
}

