#' @title Model Performance for Logistic Regression
#'
#'
#' @details
#' Produce indices of model performance for logistic regression
#'
#'
#' @param x an object of class \code{"glm"}.
#' @param data a data frame. If no dataset is provided, the model training
#' data is used.
#' @param digits integer; number of digits to print (default=4).
#' @param p numeric; probability cutoff for classifying cases (default = 0.5)
#' @param plot logical; If \code{TRUE} the results are plotted.
#' @param ... not currently used
#'
#' @import caret
#' @importFrom stats as.formula binomial predict
#'
#' @return The results of the methods \code{\link{performance.glm}}
#' @export
#' @examples
#' # performance on training sample
#' fit <- glm(am ~ hp + wt, family=binomial, data = mtcars)
#' performance(fit, plot=TRUE)
performance.glm <- function(x, data, digits=4, p=0.5, plot=FALSE, ...){

  formula <- stats::as.formula(x$call[[2]])
  dv <- as.factor(x$model[[1]])
  dvname <- names(x$model)[1]
  dvLevels <- levels(dv)

  if(missing(data)){
    data <- eval(x$call[[4]])
    dataname <- as.character(x$call[[4]])
  } else {
    dataname <- as.character(substitute(data))
  }

  prob <- stats::predict(x, data, type="response")
  pred <- factor(ifelse(prob > p, dvLevels[2], dvLevels[1]))
  stats <- caret::confusionMatrix(pred, dv, positive=dvLevels[2])

  cattbl <- round(prop.table(table(x$model[1])), 2)
  catnames <- dimnames(cattbl)[[1]]


  heading("LOGISTIC REGRESSION PERFORMANCE")

  # category frequencies

  cat("Data              :", dataname, "\n")
  cat("N                 :", nrow(x$model), "\n")
  cat("Response variable :", dvname, "\n")
  cat("Category Balance  : ",
      catnames[1], " (", cattbl[1], "%", ") ",
      catnames[2], " (", cattbl[2], "%", ") ", "\n", sep="")
  cat("Predicted category:", stats$positive, "\n")
  cat("Prob to classify  : >=", p, "\n\n", sep="")

  mdl <- as.character(x$call)
  cat("Model: ",
      mdl[1], "(", mdl[2], ",\n",
      "           family = ", mdl[3],
      ", data = ", mdl[4], ")", "\n\n", sep="")

  heading("Confusion Matrix")
  cat("\n")
  names(dimnames(stats$table)) <- c("Predicted", "Actual")
  print(stats$table)
  cat("\n")
  heading("Overall Statistics")
  cat("\n")
  cat("Accuracy:",
      round(stats$overall[1], digits), "\n")
  cat("97% CI  : (",
      round(stats$overall[3], digits),
      ", ", round(stats$overall[4], digits), ")\n", sep="")
  cat("No Information Rate: ",
      round(stats$overall[5], digits), "\n")
  cat("P-Value [Acc > NIR]: ",
      format.pval(stats$overall[6], digits), "\n")
  cat("\n")

  heading("Statistics by Category")
  dfstats <- as.data.frame(stats$byClass, drop=FALSE)
  row.names(dfstats) <- names(stats$byClass)
  row.names(dfstats)[1] <- "Sensitivity"
  row.names(dfstats)[3] <- "Pos Pred Value"
  names(dfstats) <- ""
  print(dfstats[c(1,2,3,4,7),,drop=FALSE], digits=digits)
  cat("---\n")
  cat("Note: recall = sensitivity, \n")
  cat("      precision = pos pred value.\n")

  # plot results
  print(binaryPlot(stats))

 invisible(stats)
}
