#' @title Print Summary of a Linear Model Fit
#'
#' @description
#' \code{print} method for class \code{"summary.mreg"}.
#'
#' @param x an object of class \code{"summary.mreg"} produced by the
#' \code{"summary.mreg"} function.
#' @param digits number of significant digits to print.
#' @param ... parameters passed to print.
#' @return NULL
#' @export
#' @examples
#' fit <- mreg(mpg ~ wt + hp + disp, data=mtcars)
#' sfit <- summary(fit)
#' print(sfit, digits=5)
print.summary.mreg <- function(x, digits=3, ...){

  if(!inherits(x, "summary.mreg")){
    stop("x must  be class 'summary.mreg'")
  }

  heading("MULTIPLE REGRESSION SUMMARY")

  cat("Model: ", as.character(x$call["formula"]),"\n",
      "Data : ", as.character(x$call["data"]), "\n",
      "N    : ", x$N, "\n\n",
      sep="")

  heading("Fit Indices")
  print(x$fit.indices, digits=digits, row.names=FALSE)
  cat("\n")

  heading(paste0(x$k, "-Fold Cross Validated Fit Indices:"))
  print(x$cv.indices, digits=digits, row.names=FALSE)
  cat("\n")

  heading("Omnibus Test")

  signif <- ifelse(x$Ftest$p < .001, "***",
                   ifelse(x$Ftest$p < 0.01, "**",
                          ifelse(x$Ftest$p < 0.05, "*", " ")))

  cat(paste0("F(", x$Ftest$numdf,
             ",", x$Ftest$dendf,
             ") = ", round(x$Ftest$value, digits),
             ", p < ", format.pval(x$Ftest$p, digits),
             "  ", signif,
             sep = ""), "\n")


  cat("\n")

  heading("Anova Table (type III tests)")
  printAnova(x$anova.table, digits)

  cat("\n")

  heading("Regression Coefficients")
  print(x$coefficient.table, digits=digits, ...)
}
