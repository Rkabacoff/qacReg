#' @title Model Performance
#'
#' @description
#' Indices of model performance for linear and logistic regression
#' models. The output depends on the form of the model
#' (linear or logistic regression).
#'
#' @details
#' The \code{performance} function can be used to evaluate the predictive
#' performance of a model with new data. If a data frame is not
#' specified, the performance is evaluated on the training data
#' (i.e., the data contained in the \code{model} component of the \code{lm} or
#' \code{glm} object).
#'
#' @seealso
#' \code{\link{performance.lm}}, \code{\link{performance.glm}}
#'
#' @param x an object of type \code{"lm"} or \code{"glm"}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The results of the methods \code{\link{performance.lm}} or
#' \code{\link{performance.glm}}
#' @export
#'
#' @examples
#' #######################
#' # multiple regression #
#' #######################
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' performance(fit)
#'
#' #######################
#' # logistic regression #
#' #######################
#' fit2 <- glm(caesarian ~ age + bp + delivery.time, family = binomial, data = caesarian)
#' performance(fit2)
#'

performance <- function(x, ...) UseMethod("performance")
