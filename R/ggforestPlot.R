#' @title Forest Plot
#'
#' @description
#' Display a forest plot of regression parameters for a
#' given \code{"lm"} or \code{"glm"} model.
#'
#' @details
#' For linear models, the plot displays the standardized coefficients and
#' their confidence intervals. For logistic regression models, the plot
#' displays odds ratios and their confidence intervals. Effects are plotted
#' in the same order as they appear in the model.
#'
#' @export
#' @param x object of class "lm" or "glm"
#' @param digits number of decimal digits to display
#' @param ci confidence level (default = 0.95)
#' @return a ggplot2 graph
#' @seealso \code{\link{info.lm}}, \code{\link{info.glm}}
#' @examples
#' #######################
#' # multiple regression #
#' #######################
#' fit <- lm(mpg ~ ., data = auto_mpg)
#' ggforestPlot(fit)
#'
#' #######################
#' # logistic regression #
#' #######################
#' fit2 <- glm(caesarian ~ ., family = binomial, data = caesarian)
#' ggforestPlot(fit2)
ggforestPlot <- function(x, digits=2, ci=.95){

  dvname <- names(x$model)[1]

  if (x$call[[1]] == "lm"){

    # get data and standardize
    IV <- as.data.frame(model.matrix(x))
    IV <- as.matrix(std_df(IV)[-1])
    DV <- as.numeric(scale(x$model[1]))

    # fit model to std data
    x <- lm( DV ~ IV)
    coe <- coef(x)
    terms <- names(coe)
    terms <- gsub("^IV", "", terms)
    CL <- confint(x, level=.95)
    xlab <- "Standardized Coefficients"
    xintercept <- 0

  } else if(x$call[[1]] == "glm") {
    check <- is.logistic(x)
    coe <- exp(coef(x))
    terms <- names(coe)
    CL <- exp(confint(x, level=ci))
    xlab <- "Odds Ratios"
    xintercept <- 1
  }

  LB <- CL[,1]
  UB <- CL[,2]
  p <- summary(x)$coefficients[, 4]
  stars <- sigstars(p)
  label <- paste(format(round(coe, 2), digits=digits, trim=TRUE),
                 stars)
  color <- ifelse(p >= .05, 0, 1)
  coedf <- data.frame(terms, coe, LB, UB,
                      label, color, row.names=NULL)
  coedf <- coedf[-1,] # drop intercept
  coedf$order <- nrow(coedf):1


  p <- ggplot(coedf,
              aes(y=reorder(terms, order), x=coe, label=label)) +
    geom_point(size=2, aes(color=color)) +
    geom_linerange(aes(xmin=LB, xmax=UB, color=color)) +
    geom_text(vjust=-.6, size=3) +
    geom_vline(xintercept=xintercept, color="grey", size=1) +
    labs(y="",
         x=xlab,
         title=paste("Forest Plot:", dvname)) +
    theme_bw() + theme(legend.position="none")

  if(x$call[[1]] == "glm"){
    p <- p + scale_x_continuous(trans="log10")
  }

  return(p)
}
