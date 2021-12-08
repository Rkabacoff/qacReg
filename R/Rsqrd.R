rsqr.mreg <- function(x){

  class(x) <- c("lm", "mreg")

  model1 <- summary.lm(x)
  model2 <- broom::glance(x)

  r2 <- model1$r.squared
  ar2 <- model1$adj.r.squared
  aic <- model2$AIC
  rmse <- sqrt(mean(model1$residuals^2))

  output <- data.frame()
  output <- rbind(output, data.frame(`R-Squared` = r2,
                                     `Adj R-Squared` = ar2,
                                     `AIC` = aic,
                                     `RMSE` = rmse))

    row.names(output) <- c("")
    return(output)
}

