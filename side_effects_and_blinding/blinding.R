library(brms)
source("../bayes/format.R")
source("plotting.R")


data_blinding <- read.csv("../data/blinding.csv")

m_blinding <- brm(
    formula = bf(blinding ~ type),
    data = data_blinding,
    family = categorical(),
    prior = c(
        prior(normal(0, 1), class = "b")
    ),
    sample_prior = "yes"
)


pp_check(
    m_blinding,
    type = "bars_grouped",
    group = "site",
    ndraws = 1e3
)

summary(m_blinding)
hypothesis(m_blinding, "mu0_siteMTG = 0")
hypothesis(m_blinding, "mu0_siteIFG = 0")
hypothesis(m_blinding, "mu1_siteMTG = 0")
hypothesis(m_blinding, "mu1_siteIFG = 0")

ce_blinding <- ce_to_ggplot(m_blinding, "site")
ce_blinding <- format_blinding(ce_blinding)
