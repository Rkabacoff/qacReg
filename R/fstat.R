fstat.mreg <- function(x){
  class(x) <- c("lm", "mreg")
  model1 <- summary.lm(x)


  df1 <- model1$fstatistic[2]
  df2 <- model1$fstatistic[3]
  f <- model1$fstatistic[1]
  p <- pf(model1$fstatistic[1],model1$fstatistic[2],model1$fstatistic[3],lower.tail=FALSE)

  signif <- ifelse(round(p, 3) == 0, "***",
                   ifelse(round(p, 3) < 0.001, "**",
                          ifelse(round(p, 3) < 0.01, "*",
                                 ifelse(round(p, 3) < 0.05, ".",
                                        ifelse(round(p, 3) < 0.1, " ", "")))))

  cat(paste0("F(df1: ", df1,
      ", df2: ", df2,
      ") = ", round(f, 4),
      ", p < ", round(p, 4),
      "  ", signif,
      sep = ""), "\n")

  invisible(list("df1" = as.numeric(df1),
                 "df2" = as.numeric(df2),
                 "F value" = as.numeric(f),
                 "Pr(>F)" = as.numeric(p)))
}

