
solution.mreg <- function(x){

  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")

  coeff <- as.data.frame(summary.lm(x)$coefficients)



  std_fit <- lm(as.formula(x$call), qacr::standardize(model.frame(x)))
  std_sum <- summary.lm(std_fit)
  std_coeff <- std_sum$coefficients[,1]

  coeff <- cbind(coeff, std_coeff)
  coeff <- coeff[, c(1, 5, 2, 3, 4)]

  names(coeff) <- c("B", "B*", "SE(B)", "t", "p-value")


  return(coeff)

}
