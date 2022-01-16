#'@title Print "info.glm" Object
#'
#'@description \code{print} method for class \code{"info.glm"}.
#'
#' @param x an object of class \code{"info.glm"} produced by the
#' \code{"info.glm"} function.
#' @param digits number of significant digits to print.
#' @param ... parameters passed to print.
#' @return NULL
#'
#' @importFrom crayon bold
#'
#' @export
#'
#' @examples
#' fit <- glm(caesarian ~ age + bp + delivery.time, family = binomial, data = caesarian)
#' sfit <- info(fit)
#' print(sfit, digits=5)
print.info.glm <- function(x, digits=4, ...){


  heading("LOGISTIC REGRESSION SUMMARY")

  cat("Formula: ", x$overall["formula"], "\n",
      "Data   : ", x$overall["data"], "\n",
      "N      : ", x$overall["N"], "\n\n", sep="")


  cat("Predicted category: ", x$overall["target"], "\n\n", sep="")

  heading("Omnibus Test")
  otest <- x$omnibus.test

  signif <- ifelse(otest$`Pr(>Chi)`[2] < .001, "***",
                   ifelse(otest$`Pr(>Chi)`[2] < 0.01, "**",
                          ifelse(otest$`Pr(>Chi)`[2] < 0.05, "*", " ")))

  cat("Chi-square(", otest$Df[2], ") = ",
      round(otest$`Deviance`[2], digits),
      ", p = ",
      format.pval(otest$`Pr(>Chi)`[2], digits),
      " ", signif, sep="")
  cat("\n\n")

  heading("Fit Measures")

  gof <- unlist(x$fit.indices[[1]])
  cat("Stukel's GOF Test: Chi-square(", gof["Df2"], ") = ",
      round(gof["Deviance2"], digits), ", p < ",
      round(gof["Pr(>Chi)2"], digits), "\n", sep="")
  cat("Tjur's Psuedo-R.squared: " , round(x$fit.indices[[2]], digits), "\n",
      sep="")
  cat("AIC:", round(x$fit.indices[[3]], digits), "\n\n")

  heading("Anova Table (type III tests)")
  x$anova.table$sig <- sigstars(x$anova.table[[3]])
  names(x$anova.table) <- c("LR Chisq", "DF", "Pr(>Chisq)", "")
  print(x$anova.table, digits=digits)
  cat("\n", sep="")

  heading("Logistic Regression Coefficients")
  x$coefficient.table$sig <- sigstars(x$coefficient.table[[4]])
  names(x$coefficient.table) <- c("B", "SE", "z", "Pr(>|z|)", " ")
  print(x$coefficient.table, digits=digits, ...)

  cat("\n")

  heading("Odds Ratios (with 95% Confidence Intervals)")
  print(x$odds.ratios, digits=digits, ...)


}



