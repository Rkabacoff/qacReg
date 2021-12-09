stukel.lreg <- function(x){
  formula <- x$formula
  df <- x$data
  group <- formula[[2]]
  fit <- glm(formula, df, family = binomial)

  # Stukel Test: New GOF test
  logodds <- predict(fit, df)
  df$za <- ifelse(logodds >= 0, logodds^2, 0)
  df$zb <- ifelse(logodds < 0, logodds^2, 0)
  new_form <- as.character(formula)[3]
  new_form <- paste(new_form, "+ za + zb")
  formula[[3]] <- str2lang(new_form)
  fit2 <- glm(formula, df, family = binomial)
  stukel <- anova(fit, fit2, test='LR')

  return(stukel)
}
