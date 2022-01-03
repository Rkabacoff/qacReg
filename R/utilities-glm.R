

oddsr <- function(x){
  odds <- as.data.frame(exp(car::Confint(x)))
  names(odds) <- c('Odds Ratio', '2.5%', '97.5%')
  odds <- odds[-1, , drop=FALSE]
  return(odds)
}

pseudoR2 <- function(x){

  prob <- predict(x, type="response")
  meanprob <- stats::aggregate(prob, list(x$model[[1]]), mean, na.rm=TRUE)
  r2 <- abs(meanprob$x[1] - meanprob$x[2])
  return(r2)
}


stukel.lreg <- function(x){
  formula <- x$formula
  df <- x$data
  group <- formula[[2]]
  fit <- glm(formula, df, family = binomial)

  # Stukel Test: New GOF test
  logodds <- predict(fit, df)
  df$za <- ifelse(logodds >= 0, logodds^2, 0)
  df$zb <- ifelse(logodds < 0, logodds^2, 0)
  new_form <- as.character(formula)[3]
  new_form <- paste(new_form, "+ za + zb")
  formula[[3]] <- str2lang(new_form)
  fit2 <- glm(formula, df, family = binomial)
  stukel <- stats::anova(fit, fit2, test='LR')

  return(stukel)
}

is.logistic <- function(x){
  if (class(x)[1] == "glm"){
    if (!any(grepl("binomial", x$call))){
      stop("Only linear or logistic models are allowed.", call.=FALSE)
    }
  }
}


binaryPlot <- function(x){
  if (class(x) != "confusionMatrix") stop ("Not a caret confusion matrix")
  cm <- as.data.frame(x$table)
  names(cm) <- c("pred", "truth", "Freq")
  if (nrow(cm) != 4) stop("Not a binary classification")
  pos <- x$positive
  TP <- cm[cm$pred == pos & cm$truth == pos, "Freq"]
  TN <- cm[cm$pred != pos & cm$truth != pos, "Freq"]
  FP <- cm[cm$pred == pos & cm$truth != pos, "Freq"]
  FN <- cm[cm$pred != pos & cm$truth == pos, "Freq"]
  Total <- sum(TP, TN, FP, FN)

  TPp <- TP/Total
  TNp <- TN/Total
  FPp <- FP/Total
  FNp <- FN/Total

  r1 <- paste(prettyNum(Total, big.mark=","), "cases tested", sep="\n")
  r2 <- paste(prettyNum(TP + FN, big.mark=","), "have condition", sep="\n")
  r3 <- paste(prettyNum(TN + FP, big.mark=","), "don't have condition", sep="\n")
  r4 <- paste(prettyNum(FN, big.mark=","), "false negatives", sep="\n")
  r5 <- paste(prettyNum(TP, big.mark=","), "true positives", sep="\n")
  r6 <- paste(prettyNum(FP, big.mark=","), "false positives", sep="\n")
  r7 <- paste(prettyNum(TN, big.mark=","), "true negatives", sep="\n")

  Prevalence <- (TP + FN)/Total
  Sensitivity <- TP/(TP + FN)
  Specificity <- TN/(TN + FP)

  t1 <- paste0("Prevalence", "\n", round(100*Prevalence), "%")
  t2 <- paste0(round(100*(1-Prevalence)), "%")
  t3 <- paste0(round(100*(1-Sensitivity)), "%")
  t4 <- paste0("Sensitivity", "\n", round(100*Sensitivity), "%")
  t5 <-  paste0(round(100*(1-Specificity)), "%")
  t6 <- paste0("Specificity", "\n", round(100*Specificity), "%")

  x1 <- x2 <- y1 <- y2 <- y <- r <- NULL  # for CRAN

  d <- data.frame(x1 = c( 0, 35, 35, 75, 75, 75, 75),
                  x2 = c(25, 60, 60, 100, 100, 100, 100),
                  y1 = c(20, 35, 5, 45, 30, 15, 0),
                  y2 = c(30, 45, 15, 55, 40, 25, 10),
                  r = c(r1, r2, r3, r4, r5, r6, r7))

  d2 <- data.frame(x1=c(25, 25, 60, 60, 60, 60),
                   x2=c(35, 35, 75, 75, 75, 75),
                   y1=c(25, 25, 40, 40, 10, 10),
                   y2=c(10, 40, 50, 35, 20, 5))

  d3 <- data.frame(x=c(30, 30, 68, 68, 68, 68),
                   y=c(32.5, 17.5, 45, 37.5, 15, 7.5),
                   r=c(t1, t2, t3, t4, t5, t6))

  d4 <- data.frame(x1 = c( 0, 35, 35, 75, 75, 75, 75),
                   x2 = c(25,
                          35 + Prevalence*25,
                          35 + (1-Prevalence)*25,
                          75 + FNp*25,
                          75 + TPp*25,
                          75 + FPp*25,
                          75 + TNp*25),
                   y1 = c(20, 35, 5, 45, 30, 15, 0),
                   y2 = c(30, 45, 15, 55, 40, 25, 10))

  ggplot() +
    geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
              fill=NA, color="black") +
    geom_rect(data=d4, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
              fill="grey", color=NA, alpha=.5) +
    geom_segment(data=d2, aes(x=x1, y=y1, xend=x2, yend=y2), alpha=.3) +
    geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=3.5) +
    geom_text(data=d3, aes(x=x, y=y, label=r), size=3) +
    theme(
      axis.line=element_blank(),
      axis.title=element_blank(),
      axis.ticks=element_blank(),
      axis.text=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background=element_blank(),
      plot.caption=element_text(face="plain", family="serif", size=8)) +
    labs(title = "Classification Results",
         subtitle = paste("Predicting", x$positive))
         # caption = paste("[TP] correctly predict condition eq ", x$positive,
         #                 "[FP] incorrectly predict condition eq ", x$positive, "\n",
         #                 "[TN] correctly predict condition ne ", x$positive,
         #                 "[FN] incorrectly predict condition ne ", x$positive))
}
