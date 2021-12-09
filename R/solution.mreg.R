
solution.mreg <- function(x){

  coeff <- as.data.frame(summary.lm(x)$coefficients)



  std_fit <- lm(as.formula(x$call), qacr::standardize(model.frame(x)))
  std_sum <- summary.lm(std_fit)
  std_coeff <- std_sum$coefficients[,1]

  coeff <- cbind(coeff, std_coeff)
  coeff <- coeff[, c(1, 5, 2, 3, 4)]


  signif <- ifelse(coeff[[5]] < .001, "***",
                   ifelse(coeff[[5]] < 0.01, "**",
                          ifelse(coeff[[5]] < 0.05, "*",
                                 ifelse(coeff[[5]] < 0.1, ".", " "))))

  coeff <- cbind(coeff, signif)
  names(coeff) <- c("B", "B*", "SE", "t", "p-value", "")
  coeff[["p-value"]] <- format.pval(coeff[["p-value"]], 4)


  return(coeff)

}
