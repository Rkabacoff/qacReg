
cv.mreg <- function(x){

  if(!inherits(x, "mreg")) stop("x must  be class 'mreg'")

  cv_ <- (caret::train(as.formula(x$call),
                         data= model.frame(x),
                         method="lm",
                         trControl=caret::trainControl(method="cv", number=10)))
  cv_res <- cv_$results

  cv <- data.frame()
  cv <- rbind(cv, data.frame(`R squared` = cv_res$Rsquared,
                             `RMSE` = cv_res$RMSE))
  row.names(cv) <- c("10 Fold Cross Validated Fit Indices:")


  return(cv)
}

