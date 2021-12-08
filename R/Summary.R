#'@title Summarise an 'mreg' object
#'
#'@description This function serves as a summary method for class "mreg".
#'
#'@param x an object of class "mreg", a result of a call to \link[qacReg]{mreg} (which is a wrapper for \link[base]{lm})
#'
#'@return The function \emph{summary.mreg} computes summary statistics and results for a fitted linear model with measures
#'of robustness including:
#' \describe{
#'   \item{fit indices}{includes R-squared, Adjusted R-squared, Akaike's Information Criterion, Root Mean Squared Error}
#'   \item{10-fold cross validated fit indices}{includes a more reliable and accurate measure of R-squared, Root Mean Squared Error}
#'   \item{omnibus test}{includes F-statistic, degrees of freedom, p-value}
#'   \item{anova table (type 3 tests)}{a type-III analysis of variance table (outputted from the \link[car]{Anova} function in the \strong{car} package)}
#'   \item{coefficients table}{includes a table of regression coefficients, standardised regression coefficients, standard errors, t-values, p-values, significance codes}
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

  cat("\n", " Multiple Linear Regression Summary",
      "\n", "______________________________", "\n", "\n")

  cat("Formula:", as.character(x$call["formula"]),
      "\n",
      "Data:", as.character(x$call["data"]),
      "\n", "\n")

  cat("Fit Indices:", "\n",
      "______________________________", "\n")
  print(one)
  cat("\n")

  cat("10-Fold Cross Validated Fit Indices:", "\n",
      "______________________________", "\n")
  print(two)
  cat("\n")

  cat("Omnibus Test:", "\n",
      "______________________________", "\n")
  three <- fstat.mreg(x)
  cat("---", "\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
  cat("\n")


  cat("Anova Table (type III tests):", "\n",
      "______________________________", "\n")
  print(four[1:4])
  cat("\n")

  cat("Multiple Linear Regression Coefficients:", "\n",
      "______________________________", "\n")
  print(five)
  cat("---", "\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

  invisible(list("Fit Indices" = one,
                 "10-fold CV Fit Indices" = two,
                 "Omnibus Test" = three,
                 "Type III Anova" = four,
                 "Regression Coefficients" = five))

}

