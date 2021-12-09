oddsr.lreg <- function(x){
  class(x) <- c("glm", "lreg")
  odds <- as.data.frame(exp(car::Confint(x))[-1, ])
  names(odds) <- c('Odds Ratio', '2.5%', '97.5%')
  return(odds)
}
