#' @title Variable Importance
#'
#' @description
#' The function \code{relimp} uses Dominance Analysis (DA) to rank
#' order the variables in a linear or logistic regression in terms
#' of relative importance.
#'
#' @details
#' The function is a wrapper for the \link[dominanceanalysis]{dominanceAnalysis}
#' function in the \code{dominanceanalyis} package. For linear regression
#' DA analysis uses change in R-squared to assess the average contribution of
#' each variable to the set all 1, 2, 3, ..., p variable models. For logistic
#' regression, Estrella's Pseudo R-squared is used (see reference).
#'
#' @importFrom dominanceanalysis dominanceAnalysis
#' @importFrom stats reorder
#'
#'
#' @param x an object of class \code{lm} or \code{glm}.
#'
#' @export
#'
#' @seealso \link{lm}, \link{glm}
#'
#' @return a ggplot2 graph
#'
#' @note
#' Estrella, A. (1998). A new measure of fit for equations with
#' dichotomous dependent variables. Journal of Business & Economic
#' Statistics, 16(2), 198-205.
#'
#' @examples
#' data(mtcars)
#'
#' # multiple linear regression
#' fit1 <- lm(mpg ~ ., mtcars)
#' relimp(fit1)
#'
#' # logistic regression
#' mtcars$am <- factor(mtcars$am)
#' fit2 <- glm(am ~ mpg + cyl +disp + drat + carb, family=binomial, mtcars)
#' relimp(fit2)
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
    p <- ggplot(df, aes(x=reorder(variable, r2), y=r2, fill=variable)) +
      geom_bar(stat="identity") +
      labs(title="Variable importance",
           subtitle = "based on dominance analysis for multiple regression",
           x = "Variable", y="R-squared") +
      theme_bw() +
      theme(legend.position="none",
            panel.grid.major.y=element_blank()) +
      coord_flip()
  }
  if(class(x)[1] == c("glm")){
    class(x) <- "glm"
    da <- dominanceAnalysis(x)
    da_av<- da$contribution.average$r2.e
    df <- data.frame(variable = names(da_av),
                     r2 = da_av,
                     row.names=NULL)
    p <- ggplot(df, aes(x=reorder(variable, r2), y=r2, fill=variable)) +
      geom_bar(stat="identity") +
      labs(title="Variable importance",
           subtitle = "based on dominance analysis for logistic regression",
           x = "Variable", y="Estrella Pseudo R-squared") +
      theme_bw() +
      theme(legend.position="none",
            panel.grid.major.y=element_blank()) +
      coord_flip()
  }
  return(p)
}
