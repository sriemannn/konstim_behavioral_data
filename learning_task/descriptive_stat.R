library(tidyverse)
source("format_tables.R")

d <- read.csv("../data.csv")

acc <- d %>%
       count(sbj, task, site, stim_time, hit) %>%
       filter(hit == 1) %>%
       mutate(hit_freq = n / 40)


tab_acc <- acc %>%
       group_by(task, stim_time, site) %>%
       summarise(mean_freq = mean(hit_freq), sd_freq = sd(hit_freq)) %>%
       mutate(
              mean_freq = mean_freq * 100,
              sd_freq = sd_freq * 100
       )


save_as_docx(docx_tab, path = "tables/summary_statistics.docx")
