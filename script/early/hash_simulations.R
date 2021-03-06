
library(igraph)
library(purrr)
library(digest)

# Barabasi Albert model

x <- replicate(1000, sample_pa(8, directed = FALSE), simplify = FALSE)

plot(x[[1]])

y <- sapply(x, purrr::compose(digest, igraph::as_adj))

entropy_calc(y)

y_permute <- sapply(x, purrr::compose(digest, partial(round, digits = 5), sort, ~ .$values, eigen, igraph::as_adj))

entropy_calc(y_permute)

# ERGM
library(ergm)

x <- simulate(network(6, directed = FALSE) ~ edges + triangle, coef = c(0, 0.01), nsim = 5000)

entropy_calc(x, hash = TRUE)

y_permute <- sapply(x, purrr::compose(digest, partial(round, digits = 5), sort, ~ .$values, eigen, network::as.matrix.network.adjacency))

entropy_calc(y_permute)


