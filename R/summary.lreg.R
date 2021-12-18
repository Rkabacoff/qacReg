#'@title Summarise an 'lreg' object
#'
#'@description This function serves as a summary method for class "lreg".
#'
#'@param x an object of class "lreg", a result of a call to \link[qacReg]{lreg} (which is a wrapper for \link[base]{glm})
#'@param digits number of significant digits to print
#'@return The function \strong{summary.lreg} computes and prints summary statistics and results for a fitted logistic regression model with measures
#'of robustness including:
#' \describe{
#'   \item{\strong{Goodness of Fit Measures}}{includes robust goodness of fit measures such as Stukel's test, Psuedo R-squared, and Akaike Information Criterion}
#'   \item{\strong{Anova Table (Type 3 Tests)}}{includes a Type-III Analysis of Variance Table (outputted from the \link[car]{Anova} function in the \strong{car} package)}
#'   \item{\strong{Regression Coefficients}}{includes Coefficients, Standard Errors, t-values, and p-values with significan codes}
#'   \item{\strong{Odds Ratios}}{includes odd's ratios (i.e. exponentiated coefficients) with 95% confidence intervals}
#' }
#' Additionally, summary.lreg also returns tibbles outputted using \link[broom]{glance.lm} and \link[broom]{augment.lm} functions from the \strong{broom} package.
#'
#'@importFrom broom glance
#'@importFrom broom augment
#'@importFrom car Anova
#'@importFrom broom tidy
#'@importFrom car Confint
#'@importFrom crayon bold
#'@importFrom dplyr group_by summarize
#'
#'@export
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- lreg(am ~ hp + wt + mpg, mtcars)
#'summary(fit)


summary.lreg <- function(x, digits=4){

  if(!inherits(x, "lreg")) stop("x must  be class 'lreg'")

  one <- suppressWarnings(stukel.lreg(x))
  two <- pseudor2.lreg(x)
  three <- aic.lreg(x)
  four <- anova.lreg(x)
  five <- solution.lreg(x)
  six <- suppressWarnings(oddsr.lreg(x))


  cat("\n",
      "---------------------------\n",
      "Logistic Regression Summary\n",
      "---------------------------\n\n",
      sep="")

  cat("Formula: ", as.character(x$call["formula"]), "\n",
      "Data   : ", as.character(x$call["data"]), "\n\n", sep="")

  # Model Fit
  cat("-------------\n",
      "Fit Measures:\n",
      "-------------\n", sep="")

  cat("Stukel's Test: p = ", round(one$`Pr(>Chi)`[2], digits), "\n")
  cat("Tjura's Psuedo R-squared: ", round(two, digits), "\n")
  cat("AIC:", round(three, digits), "\n\n")

  # ANOVA
  cat("-----------------------------\n",
      "Anova Table (type III tests):\n",
      "-----------------------------\n", sep="")
  print(four[1:3])
  cat("\n", sep="")

  # Coefficients
  cat("---------------------------------\n",
      "Logistic Regression Coefficients:\n",
      "---------------------------------\n", sep="")
  print(five, digits=digits)

  cat("\n")

  #Odds Ratios
  cat("--------------------------------------------\n",
      "Odds Ratios (with 95% Confidence Intervals):\n",
      "--------------------------------------------\n", sep="")
  print(six, digits=digits)

  # Augmented df, Model Statistics
  class(x) <- c("lm")
  aug_df <- augment(x, type.predict="response")
  stats_df <- glance(x)

  invisible(list("GOF Measures" = list(one, two, three),
                 "Anova (Type III)" = four,
                 "Coefficients" = five[-5],
                 "Odds Ratios" = six,
                 "Augmented df" = aug_df,
                 "Model Stats" = stats_df))

}



