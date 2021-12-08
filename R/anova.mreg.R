
anova.mreg <- function(x){

  aov_df <- car::Anova(x, type=3)
  return(aov_df)
}
