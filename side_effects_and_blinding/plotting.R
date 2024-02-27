library(ggplot2)

format_sideffects <- function(ce_plot) {
    ce_plot +
        theme_classic() +
        theme(
            text = element_text(size = 20, family = "Times New Roman"),
            legend.title = element_blank(),
            legend.position = "top"
        ) +
        labs(
            y = "Probability of side effect strength",
            x = "Stimulation type"
        ) +
        scale_color_discrete(
            labels = c("none", "mild", "moderate", "severe")
        ) +
        scale_fill_discrete(
            labels = c("none", "mild", "moderate", "severe")
        )
}

format_blinding <- function(ce_plot) {
    ce_plot +
        theme_classic() +
        theme(
            text = element_text(size = 20, family = "Times New Roman"),
            legend.title = element_blank(),
            legend.position = "top"
        ) +
        labs(
            y = "Probability of answering\n'yes'/'no'/'do not know'",
            x = "Stimulation type"
        ) +
        scale_color_discrete(
            labels = c("do not know", "no", "yes")
        ) +
        scale_fill_discrete(
            labels = c("do not know", "no", "yes")
        )
}
