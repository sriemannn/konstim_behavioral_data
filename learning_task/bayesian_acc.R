# setup
library(brms)
library(ggplot2)
library(tidybayes)
source("format_tables.R")
source("formatting_results.R")

acc_data <- read.csv("../data.csv")

# prior definition

get_prior(
    formula = hit ~ task * stim_time * site +
        (1 | non_word) + (1 + task | sbj),
    family = bernoulli(),
    data = acc_data
)

m_acc_prior <- c(
    prior(normal(0.0, 0.5), class = "b"),
    prior(normal(0.2, 0.5), class = "b", coef = "siteMTG"),
    prior(normal(0.3, 0.5), class = "b", coef = "taskdelayedrecall"),
    prior(normal(0.2, 0.5),
        class = "b",
        coef = "taskdelayedrecall:stim_timet2:siteIFG"
    ),
    prior(exponential(2), class = "sd"),
    prior(normal(.65, 0.5), class = "Intercept"),
    prior(lkj(2), class = "cor")
)

m_acc_prior_prediction <- brm(
    formula = bf(
        hit ~ task * stim_time * site +
            (1 | non_word) + (1 + task | sbj)
    ),
    data = acc_data,
    family = bernoulli(),
    file = "fits/m_acc_prior",
    prior = m_acc_prior,
    sample_prior = "only",
    cores = 4,
    chains = 4,
    iter = 8000,
    backend = "cmdstanr"
)

# prior predictive check


fixef_predictor <- fixef_predictor()

acc_prior_pred_fixef <- fixef_plot(m_acc_prior_prediction)
acc_prior_pred_fixef

acc_prior_pred_randf <- randf_plot(m_acc_prior_prediction)
acc_prior_pred_randf


# most important to check the multivariate nature of
ppd <- prior_predictive_distribution_plot(m_acc_prior_prediction, ndraws = 5e2)
ppd

# model definition

m_acc <- brm(
    formula = bf(
        hit ~ task * stim_time * site +
            (1 | non_word) + (1 + task | sbj)
    ),
    data = acc_data,
    family = bernoulli(),
    file = "fits/m_acc",
    prior = m_acc_prior,
    sample_prior = "yes",
    cores = 8,
    chains = 8,
    iter = 8000,
    backend = "cmdstanr"
)

# model evaluation
plot(m_acc)
pp_check(m_acc, ndraws = 1e3)
summary(m_acc)

## hypothesis test
# a priori hypotheses

mtg_stim_helps_learning <- hypothesis(
    m_acc, "siteMTG > 0"
)
ifg_stim_helps_recalling <- hypothesis(
    m_acc, paste0(
        "siteIFG",
        "+ stim_timet2",
        "+ stim_timet2:siteIFG",
        "+ taskdelayedrecall:stim_timet2",
        "+ taskdelayedrecall:siteIFG",
        "+ taskdelayedrecall:stim_timet2:siteIFG",
        "> 0"
    )
)

a_priori_hypotheses <- cbind(
    mtg_stim_helps_learning$samples,
    ifg_stim_helps_recalling$samples
)

a_priori_hypotheses_titles <- c(
    paste0(
        "MTG stimulation during the\n",
        "acquisition-phase increases\n",
        "performance during acquisition."
    ),
    paste0(
        "IFG stimulation during the\n",
        "second recall increases\n",
        "performance during recall."
    )
)

plot_a_priori <- hypotheses_plot(
    a_priori_hypotheses,
    a_priori_hypotheses_titles
)

## test model parameters which are visually striking

mcmc_plot(m_acc)
hypothesis(m_acc, "taskdelayedrecall > 0")
hypothesis(m_acc, "taskdelayedrecall:stim_timet2:siteIFG > 0")

## post-hoc comparing slopes
ifg_t1_gt_sham_t1 <- hypothesis(m_acc, "taskdelayedrecall:siteIFG > 0")

# IFG t1 > sham t2
ifg_t1_gt_sham_t2 <- hypothesis(m_acc, paste0(
    "taskdelayedrecall:siteIFG",
    "> taskdelayedrecall:stim_timet2"
))


# IFG t1 > MTG T1
ifg_t1_gt_mtg_t1 <- hypothesis(m_acc, paste0(
    "taskdelayedrecall:siteIFG >",
    "taskdelayedrecall:siteMTG"
))


# IFG t1 > MTG t2
ifg_t1_gt_mtg_t2 <- hypothesis(m_acc, paste0(
    "taskdelayedrecall:siteIFG >",
    "stim_timet2:siteMTG +",
    "taskdelayedrecall:siteMTG +",
    "taskdelayedrecall:stim_timet2 +",
    "taskdelayedrecall:stim_timet2:siteMTG"
))

# IFG t1 > IFG t2
ifg_t1_gt_ifg_t2 <- hypothesis(
    m_acc,
    "taskdelayedrecall:stim_timet2:siteIFG > 0"
)

h_tests_slope <- cbind(
    ifg_t1_gt_sham_t1$samples,
    ifg_t1_gt_sham_t2$samples,
    ifg_t1_gt_mtg_t1$samples,
    ifg_t1_gt_mtg_t2$samples,
    ifg_t1_gt_ifg_t2$samples
)

h_tests_slope_names <- c(
    "Slope IFG t1 - sham t1",
    "Slope IFG t1 - sham t2",
    "Slope IFG t1 - MTG t1",
    "Slope IFG t1 - MTG t2",
    "Slope IFG t1 - IFG t2"
)

slope_test <- hypotheses_plot(h_tests_slope, h_tests_slope_names)
slope_test

# conditional effects - Figure 7

conds <- make_conditions(
    m_acc,
    vars = c("stim_time")
)

conds$cond__ <- gsub("stim_time =", "Stimulation time:", conds$cond__)

cond_effects_plot <- conditional_effects(
    m_acc,
    effects = c("task:site"),
    conditions = conds,
    alpha = 0.5,
)
