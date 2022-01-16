#' @title Model Performance for Linear Models
#'
#'
#' @details
#' Produce indices of model performance for linear models
#'
#' @description
#' \code{performance} calculates the r-squared, root mean square error (RMSE),
#' and mean absolute error (MAE) applying the model to a data frame. If
#' a data frame is not specified, the model is evaluated on the
#' training data (resubstitution).
#'
#' @param x an object of class \code{"lm"}.
#' @param data a data frame.
#' @param digits integer; number of digits to print (default=4).
#' @param ... not currently used
#'
#' @import caret
#' @importFrom stats predict
#'
#' @return The results of the methods \code{\link{performance.lm}}
#' @export
#' @examples
#' # performance on training sample
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' performance(fit)
performance.lm <- function(x, data, digits=4,  ...){

  heading("Multiple Regression Performance")

  formula <- stats::as.formula(x$call[[2]])
  dv <- as.factor(x$model[[1]])
  dvname <- names(x$model)[1]

  if(missing(data)){
    data <- eval(x$call[[3]])
    dataname <- as.character(x$call[[3]])
  } else {
    dataname <- as.character(substitute(data))
  }

  pred <- stats::predict(x, data)
  dv <- as.character(x$call[[2]][[2]])
  stats <- postResample(pred, data[[dv]])

  cat("Data: ", dataname, "\n")
  cat("N:    ", nrow(x$model), "\n\n")

  cat("Model:", deparse(x$call), "\n\n")

  print(stats, digits=digits)

  invisible(stats)
}
