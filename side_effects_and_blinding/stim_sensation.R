library(brms)
source("plotting.R")

side_effects_data <- read.csv("../data/side_effects.csv")

m_side_effects <- brm(
    formula = bf(rating ~ site + side_effect + (1 | sbj)),
    data = side_effects_data,
    family = cumulative("probit"),
    prior = prior(normal(0, 1.0), class = "b"),
    sample_prior = "yes",
)

pp_check(m_side_effects, type = "bars_grouped", group = "site", ndraws = 1e3)

hypothesis(m_side_effects, "siteMTG = 0")
hypothesis(m_side_effects, "siteIFG = 0")

ce_to_ggplot <- function(model, factors, cat = TRUE) {
    ce <- conditional_effects(model, factors, categorical = cat)
    plot(ce, plot = FALSE)[[1]]
}

ce_sideeffects <- ce_to_ggplot(m_side_effects, "site")
ce_sideeffects <- format_sideffects(ce_sideeffects)

ce_sideeffects
