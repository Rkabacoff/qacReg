# testing the linearity assumption

ggResidualPlot <- function(x, n.labels=3){
  require(ggplot2)
  require(ggrepel)
  y <- rstudent(fit)
  x <- fit$fitted.values
  df <- data.frame(x, y)
  
  # which points to label
  df$absres <- abs(df$y)
  df2 <- tail(df[order(df$absres),], n.labels)
  
  ggplot(df, aes(x, y)) +
    geom_point() +
    geom_smooth(method="lm", formula=y~x, se=FALSE, linetype="dashed") +
    geom_smooth(method="loess", formula=y~x, se=FALSE, color="indianred2") +
    geom_text_repel(data=df2, aes(x=x, y=y, label=row.names(df2)), size=3) +
    labs(x="Fitted values", y="Studentized Residuals",
         subtitle="Residual plot with robust linear and loess fit lines", 
         title="Assessing Linearity",
         caption="For linear relationships, points should cluster around a straight line") + 
    theme_bw()
}

data(Duncan, package="carData")
fit <- lm(prestige ~ income + education + type, data=Duncan)
ggResidualPlot(fit)
