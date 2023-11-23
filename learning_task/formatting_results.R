library(tidyverse)
library(ggplot2)
library(ggrain)
library(brms)
library(stringr)
source("format_tables.R")

capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substring(string, 1, 1))
  return(string)
}

rainclouds <- function(data, alpha) {
  ggplot(data, aes(x = site, y = hit_freq, fill = fill)) +
    geom_rain(alpha = alpha, violin.args = list(
      color = NA, alpha = alpha
    )) +
    facet_grid(task ~ stim_time, labeller = labeller(.rows = str_to_title)) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(
      size = 18, family = "Times New Roman"
    )) +
    ylab("Accuracy [% correct]") +
    xlab("") +
    scale_fill_viridis_d()
}

remove_upper_axis <- function(patchwork_plot) {
  for (i in range(1, length(patchwork_plot) - 1)) {
    patchwork_plot[[i]] <- patchwork_plot[[i]] +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  patchwork_plot
}

remove_lower_facet_grid_boxes <- function(patchwork_plot, font_size, font) {
  last_plot_index <- length(patchwork_plot)

  for (i in range(2, last_plot_index)) {
    patchwork_plot[[i]] <- patchwork_plot[[i]] +
      theme(
        strip.background.x = element_blank(),
        strip.text.x = element_blank()
      )
  }

  patchwork_plot[[last_plot_index]] <- patchwork_plot[[
    last_plot_index
  ]] +
    theme(
      axis.text.x.bottom = element_text(
        size = font_size, family = font
      )
    )

  patchwork_plot
}


fixef_predictor <- function() {
  c(
    "Intercept", "Task", "Stimulation time", "IFG", "MTG",
    "Task * Stimulation time", "Task * IFG", "Task * MTG",
    "Stimulation time * IFG", "Stimulation time * MTG",
    "Task * IFG\n* Stimulation time",
    "Task * MTG\n* Stimulation time"
  )
}

fixef_plot <- function(model) {
  acc_prior_pred_fixef <- bayesplot::mcmc_areas(as.array(model),
    regex_pars = c("^[b][_]")
  )

  acc_prior_pred_fixef <- acc_prior_pred_fixef +
    scale_y_discrete(labels = fixef_predictor) +
    geom_vline(xintercept = 0, color = "#949494", linetype = 2) +
    theme_classic() +
    theme(text = element_text(size = 20))
}

randf_plot <- function(model) {
  acc_prior_pred_randf <- bayesplot::mcmc_areas(model,
    regex_pars = c("^[s|c]")
  )

  acc_prior_pred_randf <- acc_prior_pred_randf +
    scale_y_discrete(labels = c(
      "SD non-word intercept",
      "SD subject intercept",
      "SD task slope",
      "Correlation subject\nintercept and task slope"
    )) +
    geom_vline(xintercept = 0, color = "#949494", linetype = 2) +
    theme_classic() +
    theme(text = element_text(size = 20))
}

prior_predictive_distribution_plot <- function(model, ndraws) {
  ppd <- pp_check(model, prefix = "ppd", ndraws = ndraws)

  ppd <- ppd +
    theme_classic() +
    theme(
      text = element_text(size = 20),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none",
      plot.margin = unit(rep(0.7, 4), "cm")
    )
  ppd
}

create_fe_table <- function(model) {
  title <- "Table X\n\nPopulation level effects of the accuracy model"
  footer <- "Note. Est. Error - Estimates error"

  col_names <- c(
    "Predictor",
    "Estimate",
    "Est. Error",
    "l-95% CI",
    "u-95% CI",
    "Rhat",
    "Bulk ESS",
    "Tail ESS"
  )

  fe_table <- fixef_table(m_acc,
    title = title, footer = footer, col_names = col_names,
    row_names = fixef_predictor, type = "complete"
  ) %>%
    colformat_double(j = c("Bulk ESS", "Tail ESS"), digits = 0)

  fe_table
}

hypotheses_plot <- function(tests, hypotheses_titles) {
  names(tests) <- hypotheses_titles

  bayesplot::mcmc_areas(tests) +
    geom_vline(xintercept = 0, color = "#949494", linetype = 2) +
    theme_classic() +
    theme(text = element_text(size = 20), axis.text.y = element_text(hjust = 0))
}
