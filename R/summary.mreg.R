#'@title Summarise an 'mreg' object
#'
#'@description
#'Summarize an object of class \code{"mreg"}.
#'
#'@param x an object of class "mreg", a result
#'of a call to \link{mreg} (which is a wrapper
#'for \link{lm})
#'@param k number of folds for cross validated performance
#'
#'@return The function \strong{summary.mreg} computes summary statistics and results for a fitted linear model with measures
#'of robustness including:
#' \describe{
#'   \item{fit.indices}{includes R-squared, Adjusted R-squared, Akaike's Information Criterion, Root Mean Squared Error}
#'   \item{cv.indices}{includes a more reliable and accurate measure of R-squared, Root Mean Squared Error}
#'   \item{Ftest}{includes F-statistic, Degrees of Freedom, p-value}
#'   \item{anova.table}{includes a Type-III Analysis of Variance Table (outputted from the \link[car]{Anova()} function in the \strong{car} package)}
#'   \item{coefficient.table}{includes a table of Coefficients, Standardised Coefficients, Standard Errors, t-values, p-values, Significance codes}
#'   \item{k}{number of folds for cross-validation}
#'   \item{N}{sample size}
#' }
#'
#'@importFrom caret train trainControl
#'@importFrom car Anova
#'
#'@export
#'
#'@examples
#'fit <- mreg(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#'summary(fit)
summary.mreg <- function(x, k=5){

  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")

  # fit indices
  model <- summary.lm(x)

  # missing coefficients
  sumNA <- sum(is.na(coefficients(x)))
  if (sumNA > 0){
    print(coefficients(fit))
    stop("Degenerate solution: some coefficients are NA.")
  }

  # sample size
  N <- nrow(x$model)

  fit.indices <- data.frame(`R.Squared` = model$r.squared,
                            `Adj.R.Squared` = model$adj.r.squared,
                            AIC = AIC(x),
                            RMSE = sqrt(mean(model$residuals^2)),
                            row.names=NULL)

  # omnibus test
  F <- model$fstatistic
  Ftest <- data.frame(value=F[1],
                      numdf=F[2],
                      dendf=F[3],
                      p = pf(F[1], F[2],F[3], lower.tail = FALSE),
                      row.names=NULL)


  # cross validated fit indices

    trainCntrl <- caret::trainControl(method="cv", number=k)
    cv.results <- caret::train(as.formula(x$call),
                       data= model.frame(x),
                       method="lm",
                       trControl=trainCntrl)$results


    cv.indices <- data.frame(`R.Squared` = cv.results$Rsquared,
                             RMSE = cv.results$RMSE,
                             row.names=NULL)

  # ANOVA Table

    anova.table <- as.data.frame(car::Anova(x, type=3))

  # coefficients table
    coeff <- as.data.frame(summary.lm(x)$coefficients)

    # standardized coefficients
    stdata <- model.frame(x)
    for (i in 1:ncol(stdata)){
      if(is.numeric(stdata[[i]])){
        stdata[[i]] <- scale(stdata[[i]])
      }
    }
    std_fit <- lm(as.formula(x$call), stdata)
    std_summary <- summary.lm(std_fit)
    std_coeff <- std_summary$coefficients[,1]


    coeff <- cbind(coeff, std_coeff)
    coeff <- coeff[, c(1, 5, 2, 3, 4)]
    coeff$signif <- ifelse(coeff$`Pr(>|t|)` < .001, "***",
                           ifelse(coeff$`Pr(>|t|)` < 0.01, "**",
                                  ifelse(coeff$`Pr(>|t|)` < 0.05, "*", " ")))
    names(coeff) <- c("B", "B*", "SE", "t", "Pr(>|t|)", "")

    results <- list("fit.indices" = fit.indices,
                    "cv.indices" = cv.indices,
                    "Ftest" = Ftest,
                    "anova.table" = anova.table,
                    "coefficient.table" = coeff,
                    "k" = k,
                    "call" = x$call,
                    "N" = N)
    class(results) <- c("summary.mreg", "list")
    return(results)

}
