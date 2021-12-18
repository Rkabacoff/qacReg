solution.lreg <- function(x){
   class(x) <- c("glm", "lreg")
   sol <- as.data.frame(broom::tidy(x, conf.int=FALSE))
   rownames(sol) <- sol$term
   sol <- sol[, c(2, 3, 4, 5)]

   signif <- ifelse(sol[[4]] < .001, "***",
                    ifelse(sol[[4]] < 0.01, "**",
                           ifelse(sol[[4]] < 0.05, "*", " ")))
   sol <- cbind(sol, signif)
   names(sol) <- c("B", "SE", "t", "p-value", "")

   return(sol)
}
