
aic.lreg <- function(x){
  formula <- x$formula
  df <- x$data
  group <- formula[[2]]
  fit <- glm(formula, df, family = binomial)

  # AIC
  AIC <- fit$aic

  return(AIC)
}
