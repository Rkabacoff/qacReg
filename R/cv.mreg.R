
cv.mreg <- function(x){

  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")

  cv_ <- (caret::train(as.formula(x$call),
                         data= model.frame(x),
                         method="lm",
                         trControl=caret::trainControl(method="cv", number=10)))
  cv_res <- cv_$results

  cv <- data.frame()
  cv <- rbind(cv, data.frame("K-Fold CV R-squared" = cv_res$Rsquared,
                             "K-fold CV RMSE" = cv_res$RMSE))


  return(cv)
}

