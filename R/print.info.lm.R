#' @title Print Summary of a Linear Model Fit
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
#' fit <- lm(mpg ~ wt + hp + disp, data=mtcars)
#' sfit <- info(fit)
#' print(sfit, digits=5)
print.info.lm <- function(x, digits=3, ...){

  if(!inherits(x, "info.lm")){
    stop("x must  be class 'info.lm'")
  }

  heading("MULTIPLE REGRESSION SUMMARY")

  cat("Model: ", as.character(x$call["formula"]),"\n",
      "Data : ", as.character(x$call["data"]), "\n",
      "N    : ", x$N, "\n\n",
      sep="")

  heading("Fit Indices")
  # squared <- "\U00B2"
  # r2lbl <- paste("R", squared, sep="")
  # adj_r2lbl <- paste("Adj.", r2lbl, sep="")
  # names(x$fit.indices) <- c(r2lbl, adj_r2lbl, "AIC", "RMSE", "MAE")
  print(x$fit.indices, digits=digits, row.names=FALSE)
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
