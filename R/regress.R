#' @title Regression Analysis
#'
#' @description
#' \code{regress} fits a linear model (multiple regression, ANOVA) or
#' a logistic regression model, depending on the response variable.
#'
#' @details
#' The function is a wrapper for \code{\link{lm}} and \code{\link{glm}}
#' functions. If the response variable has two values (either numeric, factor,
#' or character), a logistic regression
#' model is fit (\code{glm} with \code{family=binomial}).
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
#' \code{\link{lm}}, \code{\link{glm}}
#'
#' @examples
#' # multiple linear regression
#' fit <- regress(mpg ~ hp + wt, mtcars)
#' fit
#'
#' # logistic regression
#' fit <- regress(am ~ hp + wt, mtcars)
#' fit
regress <- function(formula, data, ...){

  dv <- as.character(formula[[2]])

  if (length(unique(data[[dv]])) == 2){
    #logistic regression
    fit <- glm(formula, family=binomial, data,  ...)
    fit$call <- str2lang(paste("glm(formula=", deparse(substitute(formula)),
                               ", family = binomial, data=",
                               deparse(substitute(data)), ")"))
    class(fit) <- c("glm", "lm")
  } else {
    #multiple linear regression
    fit <- lm(formula, data, ...)
    fit$call <- str2lang(paste("lm(formula=",
                               paste(deparse(substitute(formula)), collapse=""),
                               ", data=",
                               deparse(substitute(data)), ")"))
    class(fit) <- "lm"
  }
  return(fit)
}
