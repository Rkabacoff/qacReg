
roc_plot <- function(x){

  data <- eval(x$call[[4]])
  dataname <- as.character(x$call[[4]])
  dv <- as.factor(x$model[[1]])
  dvname <- names(x$model)[[1]]
  dvLevels <- levels(dv)
  prob <- stats::predict(x, data, type="response")
  cutoff <- seq(0, 1, .05)
  results <- data.frame(cutoff=double(),
                       x=double(),
                       y=double())
  for(i in seq_along(cutoff)){
    pred <- factor(ifelse(prob > cutoff[i], dvLevels[2], dvLevels[1]))
    stats <- caret::confusionMatrix(pred, dv, positive=dvLevels[2])
    y <- stats$byClass[["Sensitivity"]]
    x <- 1- stats$byClass[["Specificity"]]
    outcome <- c(cutoff = cutoff[i], x=x, y=y)
    results[i,] <- outcome
  }

  #remove duplicate rows
  index <- !duplicated(results[c("x", "y")]) | results$cutoff == .5
  labels <- results[index, ]
  library(ggplot2)
  library(ggrepel)
  ggplot(data=results, aes(x=x, y=y))  +
    geom_point() +
    geom_path() +
    #geom_step(direction="vh") +
    scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, .1)) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1, .1)) +
    geom_text_repel(data=labels, aes(x=x, y= y, label=cutoff),size=3) +
    geom_segment(aes(x=0, y=0, xend=1, yend=1),
                 color="red", linetype="dashed") +
    geom_point(data=results[results$cutoff==.5,],
               aes(x=x, y), color="red", size=2) +
    labs(title=paste("ROC Plot for", dvname),
         x = "1 - Specificity",
         y = "Sensitivity") +
    theme_bw()
}

roc_plot(fit)
