
anova.mreg <- function(x){

  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")

  aov_df <- car::Anova(x, type=3)
  return(aov_df)
}
