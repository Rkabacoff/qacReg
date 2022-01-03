#'@title Print Summary of a Logistic Regression Model Fit
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
#' fit <- glm(am ~ wt + hp + disp, family=binomial, data=mtcars)
#' sfit <- info(fit)
#' print(sfit, digits=5)
print.info.glm <- function(x, digits=4, ...){


  heading("LOGISTIC REGRESSION SUMMARY")

  cat("Formula: ", as.character(x$call["formula"]), "\n",
      "Data   : ", as.character(x$call["data"]), "\n",
      "N      : ", x$N, "\n\n", sep="")

  cat("Predicted category is ", x$target, ".\n\n", sep="")

  heading("Omnibus Test")
  otest <- x$omnibus.test

  signif <- ifelse(otest$`Pr(>Chi)`[2] < .001, "***",
                   ifelse(otest$`Pr(>Chi)`[2] < 0.01, "**",
                          ifelse(otest$`Pr(>Chi)`[2] < 0.05, "*", " ")))

  chi <- "\U03A7"
  squared <- "\U00B2"
  cat(chi, squared,	"(", otest$Df[2], ") = ",
      round(otest$`Deviance`[2], digits),
      ", p = ",
      format.pval(otest$`Pr(>Chi)`[2], digits),
      " ", signif, sep="")
  cat("\n\n")

  heading("Fit Measures")
  StukelTest <- x$fit.indices[[1]]$`Pr(>Chi)`[2]
  cat("Stukel's Test: p = ", round(StukelTest, digits), "\n", sep="")
  cat("Tjura's Psuedo-R", squared, ":" , round(x$fit.indices[[2]], digits), "\n",
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
  print(x$oddsratios, digits=digits, ...)


}



