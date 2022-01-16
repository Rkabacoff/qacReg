#'@title Model summary for a "glm" object
#'
#'@description
#'Summarize a logistic regression model of class \code{"glm"}.
#'
#'@param x an object of class "glm"
#'@details
#'The function \code{info.glm} produces a summary for a
#'logistic regression model fitted with the \code{\link{glm}}  or
#'\code{\link{regress}} function. There are six sections.
#'
#'\describe{
#'  \item{Overall}{Model formula, data frame, and sample size (N), and predicted
#'  category.}
#'  \item{Omnibus Test}{Chi-square statistic, degrees of freedom, p-value.}
#'  \item{Fit Measures}{Stukel's GOF Test, Tjur's psuedo R squared, and
#'  Akaike's information criterion (AIC).}
#'  \item{Analysis of Variance}{ANOVA table with type III (marginal)
#'  effects.}
#'  \item{Regression Coefficients}{Regression coefficients (B),
#'        standard errors (SE), z-values, and p-values.}
#'   \item{Odds Ratios}{odds ratios with 95% confidence intervals}
#'}
#'
#'The ANOVA table is obtained from the \code{\link[car]{Anova}} function in
#'the \code{car} package. Odds ratios and confidence intervals are obtained
#'from the \code{\link[car]{Confint}} function from the \code{car} package.
#'
#'Tjur's pseudo-R.squared (2009) is defined as the mean absolute difference
#'between the mean predicted probability for the positive group
#'and the mean predicted probability for the negative group. It ranges
#'from zero to one.
#'
#'Stukel provides a goodness of fit (GOF) fit test for ungrouped
#'data, and has better properties than the traditional Hosmer-Lemeshow test
#'in simulation studies.  A significant result suggests model misspecification.
#'See Allison (2014) for details.
#'
#'@references
#'Allison, P. D. (2014). "Measures of fit for logistic regression", SAS Global Forum
#'\url{https://statisticalhorizons.com/wp-content/uploads/GOFForLogisticRegression-Paper.pdf}
#'
#'Allison, P. D. (2014). "Another goodness-of-fit test for
#'logistic regression".
#'\url{https://statisticalhorizons.com/another-goodness-of-fit-test-for-logistic-regression}
#'
#'Stukel, T. A. (1988). “Generalized Logistic Models.”
#'Journal of the American Statistical Association 83:426–431.
#'
#'Tjur, T. (2009) “Coefficients of determination in logistic regression models—A new proposal: The coefficient of
#'discrimination.” The American Statistician 63: 366-372.
#'
#'@return
#'A list of class c(\code{"info.glm"}, \code{"list"}).
#' \itemize{
#'   \item{overall}
#'   \item{omnibus.test}
#'   \item{fit.indices}
#'   \item{anova.table}
#'   \item{coefficient.table}
#'   \item{odds.ratios}
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
#' fit <- glm(caesarian ~ age + bp + delivery.time, family = binomial, data = caesarian)
#' info(fit)
info.glm <- function(x){

  checkargs <- is.logistic(x)

  # no error degrees of freedom
  if (summary(x)$df[2] == 0){
    stop("No residual degrees of freedom.")
  }


  N <- nrow(x$model)

  # what is the target?
  dv <- factor(x$model[[1]])
  target <- levels(dv)[2]

  # sample size
  N <- nrow(x$model)

  # model overview
  overall <- c(formula = as.character(x$call["formula"]),
               data = as.character(x$call["data"]),
               N = N,
               target = target)


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

  # odds ratios
  odds.ratios <- suppressWarnings(oddsr(x))

  # output
  results <- list("overall" = overall,
                  "omnibus.test" = omnibus.test,
                  "fit.indices" = list(stukel, R2, AIC),
                  "anova.table" = anova.table,
                  "coefficient.table" = coefficient.table,
                  "odds.ratios" = odds.ratios)

  class(results) <- c("info.glm", "list")
  return(results)
}

