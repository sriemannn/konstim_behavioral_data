
point_from_graph <- function(min, max, point, max_perc = 1) {
    # Convert the point from a graph to a percentage value
    # relative to the onset of the graph and the maximum value
    # shown
    max <- min - ((min - max) / max_perc)
    (min - point) / (min - max) * 100
}

ci95_to_sd <- function(min, max, mean_point, ci_point, n, ci_interval = 0.95 max_perc = 1) {
    # Use the minimal point, the maximal point, the mean and a point 
    # from the ci of a graph to get the sd
    
    m <- point_from_graph(min, max, graph_mean, max_perc)
    ci <- point_from_graph(min, max, ci_point, max_perc)

    disp <- abs(m - ci) / round(qnorm(1 - (1 - ci_interval) / 2), 2)

    disp * sqrt(n)
}

#Kurmakaeva Figure 5
max_point = 2.6
min_point = 12.1
mean_point = 6.1
ci_point = 6.9
n = 24

M <- point_from_graph(
    min = min_point,
    max = max_point,
    point = mean_point
)

print(paste0("Mean: ", M))

SD <- ci95_to_sd(
    min = min_point,
    max = max_point,
    mean_point = mean_point,
    ci_point = ci_point,
    n
)

print(paste0("SD: ", SD))
