library(qacReg)
library(ggplot2)
sigstars <- function(x){
  ifelse(x < .001, "***",
         ifelse(x < 0.01, "**",
                ifelse(x < 0.05, "*", " ")))
}

standardize <- function (data, mean = 0, sd = 1,
                         include_dummy = TRUE) {
  if (!is.data.frame(data))
    stop("data must be a data frame")
  std <- function(x) {
    number <- is.numeric(x)
    values <- unique(x)
    values <- values[order(values)]
    cond1 <- length(values) == 2
    cond2 <- values[1] == 0
    cond3 <- values[2] == 1
    dummy <- cond1 & cond2 & cond3
    if (include_dummy) {
      doit <- number
    }
    else {
      doit <- number & !dummy
    }
    if (doit) {
      x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
      x <- x * sd + mean
    }
    return(x)
  }
  for (i in 1:ncol(data)) {
    data[, i] <- std(data[[i]])
  }
  return(data)
}
fit <- lm(mpg ~ ., auto_mpg)

    # get data and standardize
    IV <- as.data.frame(model.matrix(fit))
    IV <- as.matrix(standardize(IV)[-1])
    DV <- as.numeric(scale(fit$model[1]))


    # fit model to std data

    fitsd <- lm( DV ~ IV)
    OR <- coef(fitsd)
    terms <- names(OR)
    terms <- gsub("^IV", "", terms)
    CL <- confint(fitsd, level=.95)
    LB <- CL[,1]
    UB <- CL[,2]
    p <- summary(fitsd)$coefficients[, 4]
    stars <- sigstars(p)
    label <- paste(format(round(OR, 2), digits=2, trim=TRUE), stars)
    color <- ifelse(p >= .05, 0, 1)
    ORdf <- data.frame(terms, OR, LB, UB,
                       label, color, row.names=NULL)
    ORdf <- ORdf[-1,] # drop intercept
    ORdf$order <- nrow(ORdf):1

    p <- ggplot(ORdf, aes(y=reorder(terms, order), x=OR, label=label)) +
      geom_point(size=2, aes(color=color)) +
      geom_linerange(aes(xmin=LB, xmax=UB, color=color)) +
      geom_text(vjust=-.6, size=3) +
      geom_vline(xintercept=0, color="grey", size=1) +
      labs(y="",
           x="Standardized Coefficients",
           title="Forest Plot") +
      theme_bw() + theme(legend.position="none")

