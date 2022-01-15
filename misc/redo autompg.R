data(auto_mpg)
dim(auto_mpg)
df$year <- as.numeric(as.character(df$year))
auto_mpg$id <- paste0(auto_mpg$carname, " (", auto_mpg$year, ")")
df <- auto_mpg[!duplicated(auto_mpg$id), ]
row.names(df) <- df$id
df <- as.data.frame(df)
row.names(df) <- df$id
df$id <- NULL
df$carname <- NULL
df$cyl <- as.numeric(as.character(df$cyl))
dim(df)
auto_mpg <- df
library(usethis)
use_data(auto_mpg, overwrite=TRUE)

> x <- visreg(fit, gg=TRUE)
> plot(x[[1]]) + ggplot2::theme_bw()
> plot(x[[2]]) + ggplot2::theme_bw()
> plot(x[[3]]) + ggplot2::theme_bw()
> plot(x[[4]]) + ggplot2::theme_bw()
