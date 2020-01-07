
library(ergm)
library(dplyr)
library(ggplot2)

simulator_function <- function(x){
  as.data.frame(ergm::simulate_formula(network::network(15, directed = FALSE) ~ triangles + edges, coef = c(log(x/(1-x)), 0), nsim = 10000, output = "stats"))
}

x_df <- data.frame(parameter = seq(0.1, 0.5, by = 0.1))

x_df <- x_df %>%
  group_by(parameter) %>%
  do(simulator_function(.$parameter))


#g <- plotly::ggplotly(ggplot(x_df) + aes(x = triangle, y = edges, fill = factor(parameter)) + geom_tile(alpha = 0.2) + ggthemes::theme_clean())

g <- plotly::ggplotly(ggplot(x_df) + aes(x = triangle, y = edges, fill = factor(parameter)) + geom_bin2d(aes(alpha = ..count.. * 2)) + ggthemes::theme_clean() + scale_x_continuous(limits = c(0, NA), expand = expand_scale(0,0)))

htmlwidgets::saveWidget(g, "triangles-and-edges.html")
