#' @title Forest Plot
#'
#' @description
#' goes here
#'
#' @export
#' @param x object of class "lm" or "glm"
#' @param digits number of decimal digits to display
#' @param ci confidence level (default = 0.95)
#' @examples
#' #######################
#' # multiple regression #
#' #######################
#' fit <- lm(mpg ~ ., data = auto_mpg)
#' forestPlot(fit)
#'
#' #######################
#' # logistic regression #
#' #######################
#' fit2 <- glm(caesarian ~ ., family = binomial, data = caesarian)
#' forestPlot(fit2)
forestPlot <- function(x, digits=2, ci=.95){

  dvname <- names(x$model)[1]
  if (x$call[[1]] == "lm"){

    # get data and standardize
    IV <- as.data.frame(model.matrix(x))
    IV <- as.matrix(std_df(IV)[-1])
    DV <- as.numeric(scale(x$model[1]))


    # fit model to std data

    fitsd <- lm( DV ~ IV)
    coe <- coef(fitsd)
    terms <- names(coe)
    terms <- gsub("^IV", "", terms)
    CL <- confint(fitsd, level=.95)
    LB <- CL[,1]
    UB <- CL[,2]
    p <- summary(fitsd)$coefficients[, 4]
    stars <- sigstars(p)
    label <- paste(format(round(coe, 2), digits=digits, trim=TRUE), stars)
    color <- ifelse(p >= .05, 0, 1)
    coedf <- data.frame(terms, coe, LB, UB,
                       label, color, row.names=NULL)
    coedf <- coedf[-1,] # drop intercept
    coedf$order <- nrow(coedf):1


    p <- ggplot(coedf, aes(y=reorder(terms, order), x=coe, label=label)) +
      geom_point(size=2, aes(color=color)) +
      geom_linerange(aes(xmin=LB, xmax=UB, color=color)) +
      geom_text(vjust=-.6, size=3) +
      geom_vline(xintercept=0, color="grey", size=1) +
      labs(y="",
           x="Standardized Coefficients",
           title=paste("Forest Plot:", dvname)) +
      theme_bw() + theme(legend.position="none")

  } else if(x$call[[1]] == "glm"){

    check <- is.logistic(x)
    OR <- exp(coef(x))
    terms <- names(OR)
    CL <- exp(confint(x, level=ci))
    LB <- CL[,1]
    UB <- CL[,2]
    p <- summary(x)$coefficients[, 4]
    stars <- sigstars(p)
    label <- paste(format(OR, digits=digits, trim=TRUE), stars)
    color <- ifelse(p >= .05, 0, 1)
    ORdf <- data.frame(terms, OR, LB, UB,
                       label, color, row.names=NULL)
    ORdf <- ORdf[-1,] # drop intercept
    ORdf$order <- nrow(ORdf):1


    p <- ggplot(ORdf, aes(y=reorder(terms, order), x=OR, label=label)) +
      geom_point(size=2, aes(color=color)) +
      geom_linerange(aes(xmin=LB, xmax=UB, color=color)) +
      geom_text(vjust=-.6, size=3) +
      geom_vline(xintercept=1, color="grey", size=1) +
      labs(y="",
           x="Odds Ratio",
           title=paste("Forest Plot:", dvname))+
      scale_x_continuous(trans="log10") +
      theme_bw() + theme(legend.position="none")


  }
  return(p)
}
