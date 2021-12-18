#'@title Summarise an 'mreg' object
#'
#'@description This function serves as a summary method for class "mreg".
#'
#'@param x an object of class "mreg", a result of a call to \link[qacReg]{mreg} (which is a wrapper for \link[base]{lm})
#'@param digits number of significant digits to print
#'@param k number of folds for cross validated performance
#'
#'@return The function \strong{summary.mreg} computes summary statistics and results for a fitted linear model with measures
#'of robustness including:
#' \describe{
#'   \item{\strong{fit.indices}}{includes R-squared, Adjusted R-squared, Akaike's Information Criterion, Root Mean Squared Error}
#'   \item{\strong{cv.indices}}{includes a more reliable and accurate measure of R-squared, Root Mean Squared Error}
#'   \item{\strong{Ftest}}{includes F-statistic, Degrees of Freedom, p-value}
#'   \item{\strong{anova.table}}{includes a Type-III Analysis of Variance Table (outputted from the \link[car]{Anova()} function in the \strong{car} package)}
#'   \item{\strong{coefficient.table}}{includes a table of Coefficients, Standardised Coefficients, Standard Errors, t-values, p-values, Significance codes}
#' }
#'
#'
#'@importFrom caret train trainControl
#'@importFrom qacr standardize
#'@importFrom broom glance tidy
#'@importFrom car Anova
#'
#'@export
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- mreg(mpg ~ hp + wt + am, mtcars)
#'summary(fit, k=5)


summary.mreg <- function(x, k=5, digits=4){

  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")

  one <- suppressWarnings(rsqr.mreg(x))
  two <- suppressMessages(cv.mreg(x, k))
  four <- suppressWarnings(anova.mreg(x))
  five <- suppressWarnings(solution.mreg(x))

  cat("\n",
      "----------------------------------\n",
      "Multiple Linear Regression Summary\n",
      "----------------------------------\n\n",
      sep="")

  cat("Formula: ", as.character(x$call["formula"]),"\n",
      "Data   : ", as.character(x$call["data"]), "\n\n",
      sep="")

  cat("------------\n",
      "Fit Indices:\n",
      "------------\n",
      sep="")
  print(one, digits=digits)
  cat("\n")

  cat("------------------------------------\n",
      k, "-Fold Cross Validated Fit Indices:\n",
      "------------------------------------\n",
      sep="")
  print(two, digits=digits)
  cat("\n")

  cat("-------------\n",
      "Omnibus Test:\n",
      "-------------\n",
      sep="")
  three <- fstat.mreg(x, digits)
  cat("\n")

  cat("-----------------------------\n",
      "Anova Table (type III tests):\n",
      "-----------------------------\n",
      sep="")
  printAnova(four, digits)
  cat("\n")

  cat("------------------------\n",
      "Regression Coefficients:\n",
      "------------------------\n",
      sep="")
  print(five, digits=digits)

  invisible(list("fit.indices" = one,
                 "cv.indices" = two,
                 "Ftest" = three,
                 "anova.table" = four,
                 "coefficient.table" = five))

}


