# model	model object produced by lm or glm.
# variable	A quoted string giving the name of a variable for the horizontal axis
# id	controls point identification; if FALSE (the default), no points are identified;
# line	TRUE to plot least-squares line.
# smooth	specifies the smoother to be used along with its arguments

Ceres.glm <- function (model, variable, id = FALSE, line = TRUE, smooth = TRUE,
          col = carPalette()[1], col.lines = carPalette()[-1], xlab,
          ylab, pch = 1, lwd = 2, grid = TRUE, ...)
{
  id <- applyDefaults(id, defaults = list(method = list(abs(residuals(model,
                                                                      type = "pearson")), "x"), n = 2, cex = 1,
                                          col = carPalette()[1], location = "lr"), type = "id")
  if (isFALSE(id)) {
    id.n <- 0
    id.method <- "none"
    labels <- id.cex <- id.col <- id.location <- NULL
  }
  else {
    labels <- id$labels
    if (is.null(labels))
      labels <- names(na.omit(residuals(model)))
    id.method <- id$method
    id.n <- if ("identify" %in% id.method)
      Inf
    else id$n
    id.cex <- id$cex
    id.col <- id$col
    id.location <- id$location
  }
  smoother.args <- applyDefaults(smooth, defaults = list(smoother = loessLine,
                                                         var = FALSE), type = "smooth")
  if (!isFALSE(smoother.args)) {
    smoother <- smoother.args$smoother
    smoother.args$smoother <- NULL
    if (is.null(smoother.args$spread))
      smoother.args$spread <- smoother.args$var
  }
  else smoother <- "none"
  expand.model.frame <- function(model, extras,
                                 envir = environment(formula(model)),
                                 na.expand = FALSE) {
    f <- formula(model)
    data <- eval(model$call$data, envir)
    ff <- foo ~ bar + baz
    if (is.call(extras))
      gg <- extras
    else gg <- parse(text = paste("~", paste(extras,
                                             collapse = "+")))[[1]]
    ff[[2]] <- f[[2]]
    ff[[3]][[2]] <- f[[3]]
    ff[[3]][[3]] <- gg[[2]]
    if (!na.expand) {
      naa <- model$call$na.action
      subset <- model$call$subset
      rval <- if (is.null(data)) {
        eval(call("model.frame", ff, data = model.frame(model),
                  subset = subset, na.action = naa), envir)
      }
      else eval(call("model.frame", ff, data = data,
                     subset = subset, na.action = naa), envir)
    }
    else {
      subset <- model$call$subset
      rval <- eval(call("model.frame", ff, data = data,
                        subset = subset, na.action = I), envir)
      oldmf <- model.frame(model)
      keep <- match(rownames(oldmf), rownames(rval))
      rval <- rval[keep, ]
      class(rval) <- "data.frame"
    }
    return(rval)
  }
  if (!is.null(class(model$na.action)) && inherits(model$na.action,
                                                   "exclude"))
    class(model$na.action) <- "omit"
  var <- if (is.character(variable) & 1 == length(variable))
    variable
  else deparse(substitute(variable))
  mod.mat <- model.matrix(model)
  obs <- names(residuals(model))
  all.obs <- if (is.null(model$call$data))
    obs
  else row.names(eval(model$call$data))
  xx <- rep(NA, length(all.obs))
  names(xx) <- all.obs
  terms <- predictor.names(model)
  if (is.na(match(var, terms)))
    stop(paste(var, "is not in the model."))
  if (!is.null(model$contrasts[[var]]))
    stop(paste(var, "is a factor."))
  terms <- terms[-match(var, terms)]
  if (any(attr(terms(model), "order") > 1)) {
    stop("ceres plot not available for models with interactions.")
  }
  .x <- xvars <- NULL
  for (xvar in terms) {
    if (is.null(model$contrasts[[xvar]])) {
      xvars <- c(xvars, xvar)
      xx[obs] <- fitted.values(loess(as.formula(paste("mod.mat[,'",
                                                      xvar, "']~mod.mat[,'", var, "']",
                                                      sep = ""))))
      .x <- cbind(.x, xx)
    }
  }
  if (is.null(xvars))
    stop("There are no covariates.")
  n.x <- length(xvars)
  mf <- na.omit(expand.model.frame(model, all.vars(formula(model))))
  rownames(.x) <- all.obs
  mf$.x <- .x[obs, ]
  aug.model <- update(model, . ~ . + .x, data = mf, subset = NULL)
  aug.mod.mat <- model.matrix(aug.model)
  coef <- coefficients(aug.model)
  k <- length(coef)
  posn <- k:(k - n.x + 1)
  partial.res <- residuals(aug.model, "partial")[, var] +
    aug.mod.mat[, posn] %*% as.matrix(coef[posn])
  xlab <- if (!missing(xlab))
    xlab
  else var
  ylab <- if (!missing(ylab))
    ylab
  else paste("CERES Residual(", responseName(model),
             ")", sep = "")
  plot(mod.mat[, var], partial.res, xlab = xlab, col = col,
       pch = pch, ylab = ylab, type = "n", ...)
  if (grid) {
    grid(lty = 1, equilogs = FALSE)
    box()
  }
  points(mod.mat[, var], partial.res, col = col, pch = pch)
  showLabels(mod.mat[, var], partial.res, labels = labels,
             method = id.method, n = id.n, cex = id.cex, col = id.col,
             location = id.location)
  if (line)
    abline(lm(partial.res ~ mod.mat[, var]), lty = 2, lwd = lwd,
           col = col.lines[1])
  if (is.function(smoother)) {
    smoother(mod.mat[, var], partial.res, col = col.lines[2],
             log.x = FALSE, log.y = FALSE, spread = smoother.args$spread,
             smoother.args = smoother.args)
  }
}
