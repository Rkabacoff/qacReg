df <- read.csv("misc/caesarian.csv")
df$delivery.time <- factor(df$delivery.time,
                           levels=c(0, 1, 2),
                           labels=c('timely', 'premature', 'latecomer'))
df$bp <- factor(df$bp, levels=c(0,1,2),
                labels=c('low', 'normal', 'high'))
df$heart.problem <- factor(df$heart.problem, levels=c(0, 1),
                           labels=c('apt', 'inept'))
df$caesarian <- factor(df$caesarian, levels=c(0,1),
                       labels=c('no', 'yes'))
str(df)

caesarian  <- df
library(usethis)
use_data(caesarian)

fit <- regress(caesarian ~ ., df)
summary(fit)
info(fit)
diagnostics(fit)
plots(fit)
relimp(fit)
performance(fit)
