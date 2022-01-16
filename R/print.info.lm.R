#' @title Print "info.lm" Object
#'
#' @description
#' \code{print} method for class \code{"info.lm"}.
#'
#' @param x an object of class \code{"info.lm"} produced by the
#' \code{"info.lm"} function.
#' @param digits number of significant digits to print.
#' @param ... parameters passed to print.
#' @return NULL
#' @export
#' @examples
#' fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#' sfit <- info(fit)
#' print(sfit, digits=5)
print.info.lm <- function(x, digits=3, ...){

  if(!inherits(x, "info.lm")){
    stop("x must  be class 'info.lm'")
  }

  heading("MULTIPLE REGRESSION SUMMARY")

  cat("Model: ", x$overall["formula"],"\n",
      "Data : ", x$overall["data"], "\n",
      "N    : ", x$overall["N"], "\n\n",
      sep="")

  heading("Fit Indices")
  print(x$fit.indices, digits=digits, row.names=FALSE)
  cat("\n")

  heading("Omnibus Test")

  cat(paste0("F(", x$F.test$numdf, ",", x$F.test$dendf, ") = ",
             round(x$F.test$value, digits),
             ", p < ", format.pval(x$F.test$p, digits),
             "  ", sigstars(x$F.test$p),
             sep = ""), "\n")


  cat("\n")

  heading("Anova Table (type III tests)")
  printAnova(x$anova.table, digits)

  cat("\n")

  heading("Regression Coefficients")
  print(x$coefficient.table, digits=digits, ...)
}
