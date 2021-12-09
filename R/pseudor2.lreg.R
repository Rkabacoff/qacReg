
pseudor2.lreg <- function(x){
  formula <- x$formula
  df <- x$data
  group <- formula[[2]]
  fit <- glm(formula, df, family = binomial)


  aug_df <- broom::augment(fit, type.predict="response")

  # pseudo R-squared
  R2 <- aug_df %>%
    group_by(df[deparse(formula[[2]])]) %>%
    summarize(meanprob = mean(.fitted))
  R2 <- abs(R2$meanprob[1] - R2$meanprob[2])
  R2

  return(R2)
}
