
pseudor2.lreg <- function(x){
  formula <- x$formula
  df <- x$data
  group <- formula[[2]]
  fit <- glm(formula, df, family = binomial)


  aug_df <- broom::augment(fit, type.predict="response")

  # pseudo R-squared
  temp <- dplyr::group_by(aug_df, df[deparse(formula[[2]])])
  temp <- dplyr::summarize(temp, meanprob = mean(.fitted))
  R2 <- abs(temp$meanprob[1] - temp$meanprob[2])



  return(R2)
}
