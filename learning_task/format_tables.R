library(flextable)
library(ftExtra)
library(tidyverse)
library(brms)
library(officer)

pub_table <- function(data, title, footer) {
    data %>%
        flextable() %>%
        theme_booktabs() %>%
        add_footer_lines(as_paragraph_md(footer)) %>%
        add_header_lines(as_paragraph_md(title)) %>%
        colformat_double(digits = 2) %>%
        autofit() %>%
        hline_bottom() %>%
        font(part = "all", fontname = "Times New Roman")
}

fixef_table <- function(data, title, footer, row_names, col_names, type = "complete") {
    if (type == "complete") {
        data <- summary(data)$ fixed
    } else if (type == "effects") {
        data <- fixef(data)
    }
    data %>%
        data.frame() %>%
        add_column(Predictor = row_names, .before = 1) %>%
        set_names(col_names) %>%
        pub_table(title = title, footer = footer)
}
