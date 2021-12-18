
cv.mreg <- function(x, k){

  cv_ <- (caret::train(as.formula(x$call),
                         data= model.frame(x),
                         method="lm",
                         trControl=caret::trainControl(method="cv", number=k)))
  cv_res <- cv_$results

  cv <- data.frame()
  cv <- rbind(cv, data.frame(`R squared` = cv_res$Rsquared,
                             `RMSE` = cv_res$RMSE))
  row.names(cv) <- c("")


  return(cv)
}

