#' @title Regression Analysis
#'
#' @description
#' \code{regress} fits a multiple linear or
#' a logistic regression model, depending on the response variable.
#'
#' @details
#' The function is a wrapper for the \code{\link{lm}} and \code{\link{glm}}
#' functions. If the response variable has only two possible values, a logistic regression
#' model is fit using \code{glm} with \code{family=binomial}.
#' Otherwise a linear model (\code{lm}) is returned.
#'
#' @param formula on object of class \code{"formula"}
#' @param data a data frame
#' @param ... optional parameters pass to the fitting functions
#'
#' @return an object of class \code{"lm"} or \code{"glm"}.
#'
#' @importFrom stats glm lm
#' @export
#'
#' @seealso
#' \code{\link{lm}}, \code{\link{glm}}, \code{\link{info}}
#'
#' @examples
#' # multiple linear regression
#' fit <- regress(mpg ~ hp + wt, mtcars)
#' fit
#'
#' # logistic regression
#' fit <- regress(am ~ hp + wt, mtcars)
#' fit
#'
regress <- function(formula, data, ...){

  dv <- as.character(formula[[2]])
  dfname <- deparse(substitute(data))
  form <- deparse(substitute(formula))

  if (length(unique(data[[dv]])) == 2){
    #logistic regression
    fit <- glm(formula, family=binomial, data,  ...)
    scall <- paste("glm(formula=", form, ",
                   family = binomial, data=", dfname, ")")
    fit$call <- str2lang(scall)
    class(fit) <- c("glm", "lm")
  } else {
    #multiple linear regression
    fit <- lm(formula, data, ...)
    scall <- paste("lm(formula=", form, ", data=", dfname, ")")
    fit$call <- str2lang(scall)
    class(fit) <- "lm"
  }
  return(fit)
}
