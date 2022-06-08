library(qacReg)
fit <- regress(caesarian ~ ., caesarian)
info(fit)

library(margins)
# average marginal effects
mod_effects <- margins(fit)
df <- summary(mod_effects)

# margins plot
library(ggplot2)
df$label <- paste0(round(df$AME*100, 1), "%")
ggplot(data = df) +
  geom_point(mapping = aes(x = factor, y = AME)) +
  geom_errorbar(mapping = aes(x = factor,
                              ymin = lower, ymax = upper,
                              width=.2)) +
  geom_hline(yintercept = 0) +
  geom_text(aes(x=factor, y=AME, label=label), size=3, vjust=-1) +
  theme_minimal() +
  labs(x="",
       y="Average Marginal Effect",
       title = "Margins Plot") +
  coord_flip()
