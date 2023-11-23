acc <- acc %>%
    unite(fill, site, stim_time, remove = FALSE) %>%
    mutate(
        task = factor(task, levels = c(
            "acquisition", "immediate recall", "delayed recall"
        )),
        site = factor(site, levels = c("sham", "IFG", "MTG")),
        stim_time = factor(stim_time, levels = c("t1", "t2"))
    )


raincloud_plots <- lapply(
    split(acc, acc$task),
    function(x) {
        rainclouds(x, alpha = 0.5)
    }
)

raincloud_patchwork <- raincloud_plots[[1]] /
    raincloud_plots[[2]] /
    raincloud_plots[[3]]


raincloud_patchwork <- remove_upper_axis(raincloud_patchwork) %>%
    remove_lower_facet_grid_boxes(., font_size = 18, font = "Times New Roman")

raincloud_patchwork
