fstat.mreg <- function(x, digits=4){
  class(x) <- c("lm", "mreg")
  model1 <- summary.lm(x)


  df1 <- model1$fstatistic[2]
  df2 <- model1$fstatistic[3]
  f <- model1$fstatistic[1]
  p <- pf(model1$fstatistic[1],model1$fstatistic[2],model1$fstatistic[3],lower.tail=FALSE)

  signif <- ifelse(p < .001, "***",
                   ifelse(p < 0.01, "**",
                          ifelse(p < 0.05, "*",
                                 ifelse(p < 0.1, ".", " "))))

  cat(paste0("F(", df1,
      ",", df2,
      ") = ", round(f, digits),
      ", p ", format.pval(p, digits),
      "  ", signif,
      sep = ""), "\n")

  invisible(list("df1" = as.numeric(df1),
                 "df2" = as.numeric(df2),
                 "F value" = as.numeric(f),
                 "Pr(>F)" = as.numeric(p)))
}

