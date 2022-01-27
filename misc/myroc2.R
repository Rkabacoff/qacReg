
roc_plot <- function(x){

  data <- eval(x$call[[4]])
  dataname <- as.character(x$call[[4]])
  dv <- as.factor(x$model[[1]])
  dvname <- names(x$model)[[1]]
  dvLevels <- levels(dv)
  prob <- stats::predict(x, data, type="response")
  auc <- pROC::auc(response=x[[dvname]], predictor=prob)
  cutoff <- seq(0, 1, .05)
  results <- data.frame(cutoff=double(),
                       x=double(),
                       y=double())
  for(i in seq_along(cutoff)){
    pred <- factor(ifelse(prob <= cutoff[i], dvLevels[1], dvLevels[2]))
    stats <- suppressWarnings(caret::confusionMatrix(pred, dv, positive=dvLevels[2]))
    y <- stats$byClass[["Sensitivity"]]
    x <- 1-stats$byClass[["Specificity"]]
    outcome <- c(cutoff = cutoff[i], x=x, y=y)
    results[i,] <- outcome
  }

  #remove duplicate rows
  index <- !duplicated(results[c("x", "y")]) | results$cutoff == .5
  labels <- results[index, ]
  library(ggplot2)
  library(ggrepel)
  ggplot(data=results, aes(x=x, y=y))  +
    geom_path(color="black") +
    geom_point(color="black") +

    #geom_step(direction="vh") +
    scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, .1)) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1, .1)) +
    geom_text_repel(data=labels, aes(x=x, y= y, label=cutoff),
                    color="steelblue", size=3) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1),
                 color="red", linetype="dashed") +
    geom_point(data=results[results$cutoff==.5,],
               aes(x=x, y), color="red", size=2) +
    labs(title=paste("ROC Plot for", dvname),
         subtitle=paste("AUC:", auc),
         x = "1-Specificity\n(False Positive Rate)",
         y = "Sensitivity\n(True Positive Rate)",
         caption=paste("Impact of cutpoints on error rates\n",
                       "Predicting:", dvLevels[2])) +
    theme_bw() +
    theme(axis.title=element_text(size=10))
}

roc_plot(fit)

