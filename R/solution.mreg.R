
solution.mreg <- function(x){

  coeff <- as.data.frame(summary.lm(x)$coefficients)



  std_fit <- lm(as.formula(x$call), qacr::standardize(model.frame(x)))
  std_sum <- summary.lm(std_fit)
  std_coeff <- std_sum$coefficients[,1]

  coeff <- cbind(coeff, std_coeff)
  coeff <- coeff[, c(1, 5, 2, 3, 4)]


  signif <- ifelse(round(coeff[[5]], 3) == 0, "***",
         ifelse(round(coeff[[5]], 3) < 0.001, "**",
                ifelse(round(coeff[[5]], 3) < 0.01, "*",
                       ifelse(round(coeff[[5]], 3) < 0.05, ".",
                              ifelse(round(coeff[[5]], 3) < 0.1, " ", "")))))

  coeff <- cbind(coeff, signif)
  names(coeff) <- c("B", "B*", "SE", "t", "p-value", "")


  return(coeff)

}
