fstat.mreg <- function(x, digits=digits){
  class(x) <- c("lm", "mreg")
  model <- summary.lm(x)


  df1 <- model$fstatistic[2]
  df2 <- model$fstatistic[3]
  f <- model$fstatistic[1]
  p <- pf(model$fstatistic[1],
          model$fstatistic[2],
          model$fstatistic[3],
          lower.tail=FALSE)

  signif <- ifelse(p < .001, "***",
                   ifelse(p < 0.01, "**",
                          ifelse(p < 0.05, "*", " ")))

  if (p < 2.2e-16){
    psign = ""
  } else {
    psign = " < "
  }

  cat(paste0("F(", df1, ",", df2, ") = ",
             round(f, digits), ",", " p ",  psign,
             format.pval(p, digits), " ", signif), "\n")

  invisible(list("df1" = as.numeric(df1),
                 "df2" = as.numeric(df2),
                 "F value" = as.numeric(f),
                 "Pr(>F)" = as.numeric(p)))
}

