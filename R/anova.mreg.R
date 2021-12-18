
anova.mreg <- function(x){

  aov_df <- car::Anova(x, type=3)
  aov_df <- broom::tidy(aov_df)
  return(aov_df)
}

printAnova <- function(x, digits){
  x$sumsq <- round(x$sumsq, 4)
  x$statistic <- round(x$statistic, 4)
  x$p.value <- round(x$p.value, 6)

  x$significant <- ifelse(x$p.value < .001, "***",
                          ifelse(x$p.value < 0.01, "**",
                                 ifelse(x$p.value < 0.05, "*", " ")))
  nas <- is.na(x)
  x[] <- sapply(seq_len(ncol(x)), function(i) {
    x <- as.character(x[[i]])
    x[nas[, i]] <- ""
    x
  })
  x[[5]] <- ifelse(x[[5]] == "0", "< 0.000001", x[[5]])
  x <- as.data.frame(x)
  row.names(x) <- x[[1]]
  x[1] <- NULL
  names(x) <- c("Sum Sq", "DF", "F value", "Pr(>F)", "")
  print.data.frame(x, digits=digits)
}
