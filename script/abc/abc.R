library(protoABC)
library(parallel)
library(ggplot2)
library(dplyr)

set.seed(1)

cl <- makeCluster(detectCores())

n <- 21
theta <- c(3.5, 0.02)

x <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ degree(6) + triangles, coef = theta, output = "stats", nsim = 1000)

x_obs <- as.numeric(apply(x, 2, mean))

distance_fun <- function(theta, inp){
  sim <- igraph::sample_pa(inp$n, m = theta, directed = FALSE)
  star6 <- sum(igraph::degree(sim) == 6)
  triangles <- length(igraph::triangles(sim))/3
  dist_out <- sqrt(sum((inp$x_obs - c(star6, triangles))^2))
  return(dist_out)
}

distance_fun2 <- function(theta, inp){
  sim <- igraph::sample_pa(inp$n, m = theta, directed = FALSE)
  star6 <- sum(igraph::degree(sim) == 6)
  triangles <- length(igraph::triangles(sim))/3
  dist_out <- sum((inp$x_obs - c(star6, triangles)) * inp$true)
  return(dist_out)
}

library(protoABC)

inp <- list(
  n = n,
  x_obs = x_obs,
  true = theta
)

prior <- function(n){data.frame(m = sample(1:10, size = n, replace = TRUE))}

abc_post_1 <- abc_start(
  prior,
  distance_fun,
  inp,
  method = "rejection",
  control = list(epsilon = 20, n = 500),
  cl = cl
)

int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0]

abc_post_1$distance <- "Euclidean"

ggsave(file = "abc_post_1.pdf")

abc_post_2 <- abc_start(
  prior,
  distance_fun2,
  inp,
  method = "rejection",
  control = list(epsilon = 35, n = 500),
  cl = cl
)

abc_post_2$distance <- "Weighted product"

abc_post <- bind_rows(abc_post_1, abc_post_2)

ggplot(abc_post) +
  aes(x = m, col = distance) +
  geom_histogram(aes(y = ..prop..), stat = "count", fill = NA, width = 0) +
  scale_x_continuous(breaks= 1:10, limits = c(1,10)) +
  scale_y_continuous(expand = expand_scale(c(0,0),c(0,0.1))) +
  ggthemes::theme_few() +
  ylab("ABC posterior")

ggsave(file = "abc_post.pdf")
