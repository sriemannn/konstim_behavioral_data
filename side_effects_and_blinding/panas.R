library(brms)
source("../bayes/format.R")
source("plotting.R")

data <- read.csv("../data/panas.csv")

m_panas <- brm(
    formula = bf(rating ~ site * time + panas + (1 | sbj)),
    data = data_panas,
    prior = prior(normal(0, 1), class = "b"),
    sample_prior = "yes"
)

summary(m_panas)

hypothesis(m_panas, "siteIFG:time2 = 0")
hypothesis(m_panas, "siteMTG:time2 = 0")
hypothesis(m_panas, "time2 < 0")

conditional_effects(m_panas)
