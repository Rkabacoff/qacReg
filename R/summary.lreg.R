#'@title Summarise an 'lreg' object
#'
#'@description This function serves as a summary method for class "lreg".
#'
#'@param x an object of class "lreg", a result of a call to \link[qacReg]{lreg} (which is a wrapper for \link[base]{glm})
#'
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
#'
#'@export
#'
#'@examples
#'mtcars$am <- factor(mtcars$am)
#'fit <- lreg(am ~ hp + wt + mpg, mtcars)
#'summary(fit)


summary.lreg <- function(x){

  if(!inherits(x, "lreg")) stop("x must  be class 'lreg'")

  one <- suppressWarnings(stukel.lreg(x))
  two <- pseudor2.lreg(x)
  three <- aic.lreg(x)
  four <- anova.lreg(x)
  five <- solution.lreg(x)
  six <- suppressWarnings(oddsr.lreg(x))


  cat("\n", "______________________________", "\n",
      crayon::bold("Logistic Regression Summary"),
      "\n", "______________________________", "\n", "\n", sep="")

  cat("Formula:", as.character(x$call["formula"]),
      "\n",
      "Data:", as.character(x$call["data"]),
      "\n", "\n", sep="")

  # Model Fit
  cat("Model Fit Measures:", "\n",
      "______________________________", "\n", sep="")
  cat("Stukel's Test:", "\n", sep="")
  print(one)
  cat("-----", "\n", sep="")
  cat("Tjura's Psuedo R-squared:", "\n", sep="")
  cat(two, "\n", sep="")
  cat("-----", "\n", sep="")
  cat("Akaike Information Criterion:", "\n", sep="")
  cat(three, "\n", sep="")
  cat("-----", "\n", sep="")
  cat("\n", sep="")

  # ANOVA
  cat("Anova Table (type III tests):", "\n",
      "______________________________", "\n", sep="")
  print(four[1:3])
  cat("\n", sep="")

  # Coefficients
  cat("Logistic Regression Coefficients:", "\n",
      "______________________________", "\n", sep="")
  print(five)
  cat("---", "\n", sep="")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n", sep="")
  cat("\n", sep="")

  #Odds Ratios
  cat("Odds Ratios (with 95% Confidence Intervals):", "\n",
      "______________________________", "\n", sep="")
  print(six)
  cat("---", "\n", sep="")

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



