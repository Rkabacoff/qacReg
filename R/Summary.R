#'@title Summarise an 'mreg' object
#'
#'@description This function serves as a summary method for class 'mreg'.
#'It computes summary statistics and results for a fitted linear model.
#'
#'@param x an object of class 'mreg', presumably an output from the mreg() function.
#'
#'@return a list containing summary statistics and results
#'
#'@details
#'
#'@importFrom caret train trainControl
#'@importFrom qacr standardize
#'@importFrom broom glance
#'
#'@export
#'
#'@examples
#'fit.mreg <- mreg(mpg ~ cyl + hp + wt, mtcars)
#'summary.mreg(fit.mreg)


summary.mreg <- function(x){

  one <- suppressWarnings(rsqr.mreg(x))
  two <- cv.mreg(x)
  three <- fstat.mreg(x)
  four <- anova.mreg(x)
  five <- solution.mreg(x)

 output <- list("Metrics" = one,
                "K-Fold Cross Validation, K=10" = two,
                "F-statistic, P-Value" = three,
                "Anova Table (Type III)" = four,
                "Multiple Linear Regression Coefficients" = five)
 return(output)
}

