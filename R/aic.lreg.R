
aic.lreg <- function(x){
  formula <- x$formula
  df <- x$data
  group <- formula[[2]]
  fit <- glm(formula, df, family = binomial)

  # AIC
  AIC <- stats_df$AIC

  return(AIC)
}
