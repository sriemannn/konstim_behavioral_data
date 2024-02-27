library(meta)
library(metafor)

d <- read.csv("contextual_learning.csv")

metaanalysis <- metamean(
    studlab = study,
    n = n,
    mean = mean_acc,
    sd = sd_acc,
    data = d
)

print(metaanalysis)

png("./meta_intercept.png", width = 960 * 4, height = 240 * 4, res = 300)
forest.meta(metaanalysis)
dev.off()

random_effects_model <- round(metaanalysis$ TE.random[1] * 1e-2, 3)

intercept_prior <- log(
    random_effects_model / (1 - random_effects_model)
)

print(intercept_prior)
