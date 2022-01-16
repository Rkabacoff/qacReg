#' @title Forest Plot
#'
#' @description
#' goes here
#'
#' @import sjPlot
#' @export
#' @param x object of class "lm" or "glm"

forestPlot <- function(x){
  if (x$call[[1]] == "lm"){
    stats <- sjPlot::get_model_data(x, type="std")
  } else if(x$call[[1]] == "glm"){
    check <- is.logistic(x)
    stats <- sjPlot::get_model_data(x, type="est")
  }
  invisible(stats)
}
