#'@title Model Summary for an 'lm' Object
#'
#'@description
#'Extended summary information for an object of class \code{"lm"}.
#'
#'@details
#'The function \code{info.lm} produces a summary for a
#'linear model fitted with the \code{\link{lm}}  or
#'\code{\link{regress}} function. There are five sections.
#'
#'\describe{
#'  \item{Overall}{Model formula, data frame, and sample size (N).}
#'  \item{Fit Indices}{R squared, adjusted R squared, Akaike's information
#'             criterion (AIC), root mean square error (RMSE), and
#'             mean absolute deviation (MAE).}
#'  \item{Omnibus Test}{F-statistic, degrees of freedom, p-value.}
#'  \item{Analysis of Variance}{ANOVA table with type III (marginal)
#'  effects.}
#'  \item{Regression Coefficients}{Regression coefficients (B),
#'        standardized regression coefficients (B*) standard errors (SE),
#'        t-values, and p-values.}
#'}
#'
#'The ANOVA table is obtained from the \code{\link[car]{Anova}} function in
#'the \code{car} package. The standardized regression coefficients are
#'obtained for the \code{\link[lm.beta]{lm.beta}} function in the
#'\code{lm.beta} package.
#'
#'@param x an object of class \code{"lm"}
#'
#'@seealso
#'\code{\link{print.info.lm}}
#'
#'@return
#'A list of class c(\code{"info.lm"}, \code{"list"}).
#' \itemize{
#'   \item{overall}
#'   \item{fit.indices}
#'   \item{F.test}
#'   \item{anova.table}
#'   \item{coefficient.table}
#' }
#'
#'@importFrom car Anova
#'@importFrom stats AIC as.formula coef model.frame pf summary.lm
#'@importFrom lm.beta lm.beta
#'@export
#'
#'@examples
#'fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#'info(fit)
info.lm <- function(x){

  if(!inherits(x, "lm")) stop("x must  be class 'lm'")

  # fit indices
  model <- stats::summary.lm(x)

  # no error degrees of freedom
  if (summary(x)$df[2] == 0){
    stop("No residual degrees of freedom.")
  }

  # missing coefficients
  sumNA <- sum(is.na(stats::coef(x)))
  if (sumNA > 0){
    print(coef(x))
    stop("Degenerate solution: some coefficients are NA.")
  }

  # sample size
  N <- nrow(x$model)

  # model overview
  overall <- c(formula = as.character(x$call["formula"]),
               data = as.character(x$call["data"]),
               N = N)

  # fit statistics
  fit.indices <- data.frame(`R.Squared` = model$r.squared,
                            `Adj.R.Squared` = model$adj.r.squared,
                            AIC = stats::AIC(x),
                            RMSE = sqrt(mean(model$residuals^2)),
                            MAE = mean(abs(model$residuals)),
                            row.names=NULL)

  # omnibus test
  F <- model$fstatistic
  F.test <- data.frame(value=F[1],
                       numdf=F[2],
                       dendf=F[3],
                       p = stats::pf(F[1], F[2],F[3], lower.tail = FALSE),
                       row.names=NULL)

  # ANOVA table
  anova.table <- as.data.frame(car::Anova(x, type=3))

  # coefficients table
  coeff <- as.data.frame(stats::summary.lm(x)$coefficients)
  std_coeff <- lm.beta::lm.beta(x)$standardized.coefficients
  coeff <- cbind(coeff, std_coeff)
  coeff <- coeff[, c(1, 5, 2, 3, 4)]
  coeff$signif <- sigstars(coeff$`Pr(>|t|)`)
  names(coeff) <- c("B", "B*", "SE", "t", "Pr(>|t|)", "")

  # output
  results <- list("overall"= overall,
                  "fit.indices" = fit.indices,
                  "F.test" = F.test,
                  "anova.table" = anova.table,
                  "coefficient.table" = coeff,
                  "call" = x$call,
                  "N" = N)
  class(results) <- c("info.lm", "list")
  return(results)

}
