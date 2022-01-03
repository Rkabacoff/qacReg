#'@title Summarise a logistic regression model
#'
#'@description
#'Summarize a logistic regression model of class \code{"glm"}.
#'
#'@param x an object of class "glm"
#'
#'@details
#'Put details here
#'
#'@return The list of class c(\code{"info.glm"}, \code{"list"}).
#' \describe{
#'   \item{omnibus.test}{omnibus chisquare test}
#'   \item{fit.indices}{goodness of fit measures including Stukel's test, Pseudo R-squared, and Akaike Information Criterion}
#'   \item{anova.table}{Type-III Analysis of Variance Table}
#'   \item{coefficient.table}{includes Coefficients, Standardized Coefficients,
#'   Standard Errors, z-values, and p-values with significant stars}
#'   \item{oddsratios}{odd's ratios with 95% confidence intervals}
#'   \item{N}{sample size}
#'   \item{target}{category predicted}
#' }
#'
#'@importFrom car Anova Confint
#'@importFrom stats AIC aggregate anova update
#'
#'@seealso \code{\link[car]{Anova}}, \code{\link[car]{Confint}}
#'
#'@export
#'
#'@examples
#'fit <- regress(am ~ hp + wt + mpg, mtcars)
#'info(fit)


info.glm <- function(x){

  checkargs <- is.logistic(x)

  N <- nrow(x$model)

  # what is the target?
  dv <- factor(x$model[[1]])
  target <- levels(dv)[2]

  # omnibus test
  omnibus.test <- stats::anova(stats::update(x, ~1), x, test="Chisq")


  # fit statistics
  stukel <- suppressWarnings(stukel.lreg(x))
  R2 <- pseudoR2(x)
  AIC <- stats::AIC(x)

  # anova table
  anova.table <- as.data.frame(car::Anova(x, type=3))

  # regression coefficients
  coefficients <- summary(x)$coefficients
  coefficient.table <- as.data.frame(coefficients)


  oddsratios <- suppressWarnings(oddsr(x))

  results <- list("omnibus.test" = omnibus.test,
                  "fit.indices" = list(stukel, R2, AIC),
                  "anova.table" = anova.table,
                  "coefficient.table" = coefficient.table,
                  "oddsratios" = oddsratios,
                  "call" = x$call,
                  "N" = N,
                  "target" = target)
  class(results) <- c("info.glm", "list")
  return(results)
}

