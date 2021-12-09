anova.lreg <- function(x){
  class(x) <- c("glm", "lreg")
  aov_df <- car::Anova(x, type=3)
  return(aov_df[1:3])
}
