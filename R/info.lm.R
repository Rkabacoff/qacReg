#'@title Summarise an 'lm' object
#'
#'@description
#'Summarize an object of class \code{"lm"}.
#'
#'@details
#'The function \code{info.lm} produces a comprehensive
#'summary for a linear model fitted with \link{lm} function. In addition
#'to the standard output from the \link{summary.lm} function, the output includes
#'the sample size (N), Akaike's Information Criterion (AIC), Mean Absolute Error (MAE), ANOVA table (type III SS), and
#'standardized regression coefficients (B*).
#'
#'@param x an object of class code{"lm"}
#'
#'@return  a 6-component list
#' \describe{
#'   \item{fit.indices}{includes R-squared, Adjusted R-squared, Akaike's Information Criterion, Root Mean Squared Error, Mean Absolute Error}
#'   \item{Ftest}{includes F-statistic, Degrees of Freedom, p-value}
#'   \item{anova.table}{includes a Type-III Analysis of Variance Table (outputted from the \link[car]{Anova} function in the \strong{car} package)}
#'   \item{coefficient.table}{includes a table of Coefficients, Standardised Coefficients, Standard Errors, t-values, p-values, significance stars}
#'   \item{N}{sample size}
#' }
#'
#'@importFrom car Anova
#'@importFrom stats AIC as.formula coef model.frame pf summary.lm
#'@importFrom lm.beta lm.beta
#'
#'@export
#'
#'@examples
#'fit <- lm(mpg ~ hp + wt + accel + origin, data = auto_mpg)
#'info(fit)
info.lm <- function(x){

  if(!inherits(x, "lm")) stop("x must  be class 'lm'")

  # fit indices
  model <- stats::summary.lm(x)

  # missing coefficients
  sumNA <- sum(is.na(stats::coef(x)))
  if (sumNA > 0){
    print(coef(x))
    stop("Degenerate solution: some coefficients are NA.")
  }

  # sample size
  N <- nrow(x$model)

  fit.indices <- data.frame(`R.Squared` = model$r.squared,
                            `Adj.R.Squared` = model$adj.r.squared,
                            AIC = stats::AIC(x),
                            RMSE = sqrt(mean(model$residuals^2)),
                            MAE = mean(abs(model$residuals)),
                            row.names=NULL)

  # omnibus test
  F <- model$fstatistic
  Ftest <- data.frame(value=F[1],
                      numdf=F[2],
                      dendf=F[3],
                      p = stats::pf(F[1], F[2],F[3], lower.tail = FALSE),
                      row.names=NULL)

  # ANOVA Table

    anova.table <- as.data.frame(car::Anova(x, type=3))

  # coefficients table
    coeff <- as.data.frame(stats::summary.lm(x)$coefficients)

    #standardized coefficients
    # stdata <- stats::model.frame(x)
    # for (i in 1:ncol(stdata)){
    #   if(is.numeric(stdata[[i]])){
    #     stdata[[i]] <- scale(stdata[[i]])
    #   }
    # }
    #
    # # stick a tryCatch in here for log(mpg)
    # # look at model.matrix(fit) to get regression below
    # std_fit <- lm(stats::as.formula(x$call), stdata)
    # std_summary <- stats::summary.lm(std_fit)
    # std_coeff <- std_summary$coefficients[,1]
    #std_coeff <- c(0, stdB(x))
    std_coeff <- lm.beta::lm.beta(x)$standardized.coefficients


    coeff <- cbind(coeff, std_coeff)
    coeff <- coeff[, c(1, 5, 2, 3, 4)]
    coeff$signif <- ifelse(coeff$`Pr(>|t|)` < .001, "***",
                           ifelse(coeff$`Pr(>|t|)` < 0.01, "**",
                                  ifelse(coeff$`Pr(>|t|)` < 0.05, "*", " ")))
    names(coeff) <- c("B", "B*", "SE", "t", "Pr(>|t|)", "")

    results <- list("fit.indices" = fit.indices,
                    "Ftest" = Ftest,
                    "anova.table" = anova.table,
                    "coefficient.table" = coeff,
                    "call" = x$call,
                    "N" = N)
    class(results) <- c("info.lm", "list")
    return(results)

}
