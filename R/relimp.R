#' @title Variable Importance
#'
#' @description
#' The function \code{relimp} uses Dominance Analysis (DA) to rank
#' order the variables in a linear or logistic regression in terms
#' of relative importance.
#'
#'
#' @details
#' The function is a wrapper for the \code{\link[dominanceanalysis]{dominanceAnalysis}}
#' function in the \code{dominanceanalyis} package. For linear regression
#' DA analysis uses change in R-squared to assess the average contribution of
#' each variable to the set all 1, 2, 3, ..., p variable models. For logistic
#' regression, Estrella's Pseudo R-squared is used (see references).
#'
#' Results are graphed as horizontal dot plots.
#'
#' @importFrom dominanceanalysis dominanceAnalysis
#' @importFrom stats reorder
#'
#'
#' @param x an object of class \code{lm} or \code{glm}.
#'
#' @export
#'
#' @seealso \code{\link[dominanceanalysis]{dominanceAnalysis}}
#'
#' @return a ggplot2 graph
#'
#' @references
#' Estrella, A. (1998). A new measure of fit for equations with
#' dichotomous dependent variables. Journal of Business & Economic
#' Statistics, 16(2), 198-205.
#'
#' Azen, R., & Budescu, D. V. (2003). The dominance analysis approach
#' for comparing predictors in multiple regression. Psychological Methods,
#' 8(2), 129-148.
#'
#' Azen, R., & Traxel, N. (2009). Using Dominance Analysis to Determine
#' Predictor Importance in Logistic Regression.
#' Journal of Educational and Behavioral Statistics, 34(3), 319-347.
#' @examples
#' data(mtcars)
#'
#'#######################
#'# Multiple regression #
#'#######################
#'fit <- lm(mpg ~ ., data = auto_mpg)
#'relimp(fit)
#'
#'#######################
#'# Logistic regression #
#'#######################
#'fit2 <- glm(caesarian ~ ., family = binomial, data = caesarian)
#'relimp(fit2)
#'
relimp <- function(x){
  cat("working ...\n")

  variable <- r2 <- NULL # for CRAN

  if (class(x)[1] == c("lm")){
    class(x) <- "lm"
    da <- dominanceAnalysis(x)
    da_av<- da$contribution.average$r2
    df <- data.frame(variable = names(da_av),
                     r2 = da_av,
                     row.names=NULL)
    p <- ggplot(df, aes(y=reorder(variable, r2), x=r2)) +
      geom_segment(aes(y=reorder(variable, r2),
                       yend=reorder(variable, r2), x=0, xend=r2),
                   color="grey", linetype="solid") +
      geom_point(size=2.5, color="steelblue") +
      labs(title="Variable importance",
           subtitle = "based on dominance analysis for multiple regression",
           y = "Variable", x="R-squared") +
      theme_bw() +
      theme(legend.position="none",
            panel.grid.major.y=element_blank())
  }
  if(class(x)[1] == c("glm")){
    class(x) <- "glm"
    da <- dominanceAnalysis(x)
    da_av<- da$contribution.average$r2.e
    df <- data.frame(variable = names(da_av),
                     r2 = da_av,
                     row.names=NULL)
    p <- ggplot(df, aes(y=reorder(variable, r2), x=r2)) +
      geom_segment(aes(y=reorder(variable, r2),
                       yend=reorder(variable, r2), x=0, xend=r2),
                   color="grey", linetype="solid") +
      geom_point(size=2.5, color="steelblue") +
      labs(title="Variable importance",
           subtitle = "based on dominance analysis for logistic regression",
           y = "Variable", x="Estrella Pseudo R-squared") +
      theme_bw() +
      theme(legend.position="none",
            panel.grid.major.y=element_blank())
  }
  return(p)
}
