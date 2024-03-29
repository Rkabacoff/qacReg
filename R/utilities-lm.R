##----------------------------------------------##
## Utility functions for regression - not exported
##----------------------------------------------##


# print headings ----------
heading <- function(x){
  cat(crayon::blue$bold(x), "\n")
}


# robust normality test based on DescTools --------
normality.test <- function (x, digits=4) {
  x <- rstudent(x)
  x <- na.omit(x)
  n <- length(x)
  m1 <- sum(x)/n
  m2 <- sum((x - m1)^2)/n
  m3 <- sum((x - m1)^3)/n
  m4 <- sum((x - m1)^4)/n

  J <- sqrt(pi/2) * mean(abs(x - median(x)))
  J2 <- J^2
  b1 <- (m3/(J2)^(3/2))^2
  b2 <- (m4/(J2)^2)
  vk <- 64/n
  vs <- 6/n
  ek <- 3
  statistic <- b1/vs + (b2 - ek)^2/vk

  p.value <- 1 - stats::pchisq(statistic, df = 2)

  cat("Robust Jarque Bera Test for Normality\n")
  cat("Chi-square(2) = ",
      round(statistic, digits),
      ", p < ", format.pval(p.value, digits), "\n", sep="")
}

# scatter plot from qacBase --------------------
scatter_plot <- function(x,
                    title,
                    outlier=3,
                    alpha=.4,
                    digits=3,
                    stats=TRUE,
                    point_color="black",
                    outlier_color="violetred1",
                    line_color="grey30"){

  # import parameters
  data <- x$model
  xvar <- names(data)[2]
  yvar <- names(data)[1]
  f <- stats::as.formula(paste(yvar, "~", xvar))

  # title
  if(missing(title)){
    title <- paste("Plot of", xvar, "by", yvar)
  }


  # remove missing data
  data <- na.omit(data)

  # fit model and flag outliers
  fit  <- lm(f, data)
  sfit <- summary(fit)
  b0   <- stats::coef(fit)[1]
  b1   <- stats::coef(fit)[2]
  Fvalue <- sfit$fstatistic[1]
  dfn  <- sfit$fstatistic[2]
  dfd  <- sfit$fstatistic[3]
  r    <- sqrt(sfit$r.squared)
  p    <- pf(Fvalue, dfn, dfd, lower.tail = FALSE)

  # studentized residuals
  data$stud.residuals <- rstudent(fit)


  # rmse
  # rmse <- sqrt(mean(residuals(fit)^2))

  # flag outliers
  data$outlier <- ifelse(abs(data$stud.residuals) >= outlier,
                         "outlier", "non-outlier")

  # p-value
  p_value = "p > 0.05"
  if (p < .05) p_value = "p < .05"
  if (p < .01) p_value = "p < .01"
  if (p < .001) p_value = "p < .001"


  # create informational inset
  inset <- paste0(
    "slope = ",
    format(b1, big.mark=",", digits=digits),
    # ", RMSE = ",
    # format(rmse, big.mark=",", digits=digits),
    ", r = ",
    round(r, digits),
    " (", round(r*r*100), "%), ",
    p_value)

  # create plot
  p <- ggplot(data=data,
              aes(x=.data[[xvar]],
                  y=.data[[yvar]])) +
    geom_smooth(method="lm",
                formula=y~x,
                color=line_color) +
    geom_point(alpha=alpha,
               aes(color=.data[["outlier"]])) +
    scale_color_manual(values=c(point_color, outlier_color)) +
    labs(title = title) +
    theme_bw() +
    theme(legend.position="none",
          plot.subtitle = element_text(size=8,face="plain"),
          plot.caption = element_text(size=8, face="plain"))

  if (stats){
    p <- p + labs(subtitle=inset)
  }


  # outlier caption
  if (any(abs(data$stud.residuals) > outlier & outlier != 0)){
    p <- p + labs(caption=paste("Note: studentized residuals >",
                                outlier, "are highlighted."))
  }

  # return graph
  return(p)
}

# pretty print car::Anova.lm results------------------
printAnova <- function(x, digits){
  x$`Sum Sq`  <- round(x$`Sum Sq`, digits)
  x$`F value` <- round(x$`F value`, digits)
  x$`Pr(>F)`  <- round(x$`Pr(>F)`, digits)
  x$significant <- sigstars(x$`Pr(>F)`)

  nas <- is.na(x)
  x[] <- sapply(seq_len(ncol(x)), function(i) {
    x <- as.character(x[[i]])
    x[nas[, i]] <- ""
    x
  })
  lbound <- paste0("<0.",
                   paste(rep(0, digits-1), collapse=""),
                   "1")
  x[[4]] <- ifelse(x[[4]] == "0", lbound, x[[4]])
  names(x) <- c("Sum Sq", "DF", "F value", "Pr(>F)", "")
  print.data.frame(x, digits=digits)
}


sigstars <- function(x){
  ifelse(x < .001, "***",
         ifelse(x < 0.01, "**",
                ifelse(x < 0.05, "*", " ")))
}

std_df <- function (data, mean = 0, sd = 1,
                         include_dummy = TRUE) {
  if (!is.data.frame(data))
    stop("data must be a data frame")
  std <- function(x) {
    number <- is.numeric(x)
    values <- unique(x)
    values <- values[order(values)]
    cond1 <- length(values) == 2
    cond2 <- values[1] == 0
    cond3 <- values[2] == 1
    dummy <- cond1 & cond2 & cond3
    if (include_dummy) {
      doit <- number
    }
    else {
      doit <- number & !dummy
    }
    if (doit) {
      x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
      x <- x * sd + mean
    }
    return(x)
  }
  for (i in 1:ncol(data)) {
    data[, i] <- std(data[[i]])
  }
  return(data)
}
