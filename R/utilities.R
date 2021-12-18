# print headings
heading <- function(x){
  len <- nchar(x)
  l <- paste(rep("-", len), collapse="")
  cat(l, x, l, sep="\n")

}

# pretty print car::Anova.lm results
printAnova <- function(x, digits){
  x$`Sum Sq` <- round(x$`Sum Sq`, digits)
  x$`F value` <- round(x$`F value`, digits)
  x$`Pr(>F)` <- round(x$`Pr(>F)`, digits)
  x$significant <- ifelse(x$`Pr(>F)` < .001, "***",
                          ifelse(x$`Pr(>F)` < 0.01, "**",
                                 ifelse(x$`Pr(>F)` < 0.05, "*", " ")))

  nas <- is.na(x)
  x[] <- sapply(seq_len(ncol(x)), function(i) {
    x <- as.character(x[[i]])
    x[nas[, i]] <- ""
    x
  })
  lbound <- paste0("<0.",
                   paste(rep(0, digits-1), collapse=""),
                   "1")
  x[[4]] <- ifelse(x[[4]] == "0", lbound, x[[4]])
  names(x) <- c("Sum Sq", "DF", "F value", "Pr(>F)", "")
  print.data.frame(x, digits=digits)
}

