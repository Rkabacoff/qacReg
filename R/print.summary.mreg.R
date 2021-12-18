heading <- function(x){
  len <- nchar(x)
  l <- paste(rep("-", len), collapse="")
  cat(l, x, l, sep="\n")

}


print.summary.mreg <- function(x, digits=3){

  if(!inherits(x, "summary.mreg")) stop("x must  be class 'summary.mreg'")

  heading("Multiple Linear Regression Summary")

  cat("Formula: ", as.character(x$call["formula"]),"\n",
      "Data   : ", as.character(x$call["data"]), "\n\n",
      sep="")

  heading("Fit Indices:")
  print(x$fit.indices, digits=digits, row.names=FALSE)
  cat("\n")

  heading(paste0(x$k, "-Fold Cross Validated Fit Indices:"))
  print(x$cv.indices, digits=digits, row.names=FALSE)
  cat("\n")

  heading("Omnibus Test:")

  signif <- ifelse(x$Ftest$p < .001, "***",
                   ifelse(x$Ftest$p < 0.01, "**",
                          ifelse(x$Ftest$p < 0.05, "*", " ")))

  cat(paste0("F(", x$Ftest$numdf,
             ",", x$Ftest$dendf,
             ") = ", round(x$Ftest$value, digits),
             ", p < ", format.pval(x$Ftest$p, digits),
             "  ", signif,
             sep = ""), "\n")


  cat("\n")

  heading("Anova Table (type III tests):")

  printAnova(x$anova.table, digits)

  cat("\n")

  heading("Regression Coefficients:")
  print(x$coefficient.table, digits=digits)

}


