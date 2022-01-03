#' @title Model Performance for Linear Models
#'
#'
#' @details
#' Produce indices of model performance for linear models
#'
#'
#' @param x an object of class \code{"lm"}.
#' @param data a data frame.
#' @param N integer; number of bootstrap replications (default=20).
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
performance.lm <- function(x, data, digits=4, N=20, ...){
  heading("Multiple Regression Performance")
  if(missing(data)){
    formula <- stats::as.formula(x$call[[2]])
    data <- eval(x$call[[3]])
    dataname <- as.character(x$call[[3]])
    stats <- caret::train(formula, data, method="lm",
                           trControl=trainControl(method="boot", number=N))
    metrics <- stats$results

    mdf <- data.frame(Mean = c(metrics$RMSE, metrics$Rsquared, metrics$MAE),
                      SD = c(metrics$RMSESD, metrics$RsquaredSD, metrics$MAESD))
    row.names(mdf) <- c("RMSE", "Rsquared", "MAE")


  cat("Data:  ", dataname, "\n")
  cat("Method:", N, "bootstrap resamples\n\n")
  mdf <- data.frame(mean = c(metrics$RMSE, metrics$Rsquared, metrics$MAE),
                    sd = c(metrics$RMSESD, metrics$RsquaredSD, metrics$MAESD))
  row.names(mdf) <- c("RMSE", "Rsquared", "MAE")
  print(mdf, digits=digits)

  } else {
    pred <- stats::predict(x, data)
    dv <- as.character(x$call[[2]][[2]])
    stats <- postResample(pred, data[[dv]])
    print(stats, digits=digits)
  }
 invisible(stats)
}
