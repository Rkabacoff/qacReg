#'@title Summarise an 'mreg' object
#'
#'@description This function serves as a summary method for class "mreg".
#'
#'@param x an object of class "mreg", a result of a call to \link[qacReg]{mreg} (which is a wrapper for \link[base]{lm})
#'
#'@return The function \strong{summary.mreg} computes summary statistics and results for a fitted linear model with measures
#'of robustness including:
#' \describe{
#'   \item{\strong{Fit Indices}}{includes R-squared, Adjusted R-squared, Akaike's Information Criterion, Root Mean Squared Error}
#'   \item{\strong{10-fold Cross Validated Fit Indices}}{includes a more reliable and accurate measure of R-squared, Root Mean Squared Error}
#'   \item{\strong{Omnibus Test}}{includes F-statistic, Degrees of Freedom, p-value}
#'   \item{\strong{Anova Table (Type 3 Tests)}}{includes a Type-III Analysis of Variance Table (outputted from the \link[car]{Anova()} function in the \strong{car} package)}
#'   \item{\strong{Regression Coefficients}}{includes a table of Coefficients, Standardised Coefficients, Standard Errors, t-values, p-values, Significance codes}
#' }
#'
#'
#'@importFrom caret train trainControl
#'@importFrom qacr standardize
#'@importFrom broom glance
#'
#'@export
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- mreg(mpg ~ hp + wt + am, mtcars)
#'summary(fit)


summary.mreg <- function(x){

  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")

  one <- suppressWarnings(rsqr.mreg(x))
  two <- cv.mreg(x)
  four <- anova.mreg(x)
  five <- solution.mreg(x)

  cat("\n", "Multiple Linear Regression Summary",
      "\n", "----------------------------------",
      "\n", "\n", sep="")

  cat("Formula:", as.character(x$call["formula"]),
      "\n",
      "Data   :", as.character(x$call["data"]),
      "\n", "\n", sep="")

  cat("Fit Indices:", "\n",
      "------------", "\n", sep="")
  print(one)
  cat("\n")

  cat("10-Fold Cross Validated Fit Indices:", "\n",
      "------------------------------------", "\n", sep="")
  print(two)
  cat("\n")

  cat("Omnibus Test:", "\n",
      "-------------", "\n", sep="")
  three <- fstat.mreg(x)
  cat("---", "\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
  cat("\n")


  cat("Anova Table (type III tests):", "\n",
      "-----------------------------", "\n", sep="")
  print(four[1:4])
  cat("\n")

  cat("Multiple Linear Regression Coefficients:", "\n",
      "----------------------------------------", "\n", sep="")
  print(five)
  cat("---", "\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

  invisible(list("Fit Indices" = one,
                 "10-fold CV Fit Indices" = two,
                 "Omnibus Test" = three,
                 "Type III Anova" = four,
                 "Regression Coefficients" = five))

}

