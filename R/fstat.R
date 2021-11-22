fstat.mreg <- function(x){
  class(x) <- c("lm", "mreg")
  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")
  model1 <- summary(x)

  df1 <- model1$fstatistic[2]
  df2 <- model1$fstatistic[3]
  f <- model1$fstatistic[1]
  p <- pf(model1$fstatistic[1],model1$fstatistic[2],model1$fstatistic[3],lower.tail=FALSE)

  invisible(data.frame("df1" = df1,
                       "df2" = df2,
                       "F-stat" = f,
                       "P-value" = p))
  return(paste("F(df1: ", df1,
      ", df2: ", df2,
      ") = ", round(f, 4),
      ", p < ", round(p, 4),
      sep = ""))
}

