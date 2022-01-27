#' @title Receiver Operating Characteristic curve
#'
#' @description Plot a receiver operating characteristic curve for a binary predictive model
#'
#' @param actual actual class.
#' @param prob predicted probability for the target class
#' @param positive label for the target class
#' @param n.cuts number of probability cut-points to plot
#' @param labelsize size of cutpoint labels
#' @param digits number of decimal digits in the cutpoint labels
#' @export
#' @import ggplot2
#' @import plotROC
#' @return a ggplot2 graph
#' @examples
#' # logistic regression
#' fit <- glm(caesarian ~ ., family=binomial, caesarian)
#' prob <- predict(fit, newdata=caesarian, type="response")
#' roc_plot(caesarian$caesarian, prob, positive="yes")
roc_plot <- function(actual, prob, positive, n.cuts=20,
                     labelsize=3, digits=2){

  outcome <- as.character(actual)
  # check positive outcome
  x <- table(outcome)
  if (length(x) != 2) stop("Outcome must have 2 levels.")
  if (missing(positive)) {
    positive <- names(x)[length(x)]
  } else if(!(positive %in% names(x))){
    stop("Specified outcome is not one of the existing levels.")
  }

  text <- paste("predicting:", positive)

  outcome <- ifelse(outcome == positive, 1, 0)

  # predicted probability
  df <- data.frame(d = outcome,
                   m = prob)

  p <- ggplot(df, aes(d=.data[["d"]], m=.data[["m"]])) +
    geom_roc(n.cuts=n.cuts, labelsize=labelsize, alpha=.5,
             labelround=digits, size=.5) +
    geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
    theme_bw() +
    labs(title="ROC Plot",
         x="1 - Specificity\n(false positive rate)",
         y="Sensitivity\n(true positive rate)") +
    scale_x_continuous(breaks=seq(0,1, .1))+
    scale_y_continuous(breaks=seq(0,1, .1))


  auc <- round(calc_auc(p)[1,3], 3)
  p + labs(subtitle=paste("Area under curve:", auc),
           caption=paste("predicting:", positive))

}
