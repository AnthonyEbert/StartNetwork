set.seed(1)

library(StartNetwork)
library(parallel)
library(ggplot2)

n = 15
replicates = 400
datapoints = 10

mech_net_gnp = purrr::partial(igraph::sample_gnp, n = !!n, ... = , directed = FALSE, loops = FALSE)

true_value <- 0.5
theta_s <- log(true_value/(1 - true_value))
theta_p <- rep(seq(0.025, 0.975, by = 0.025), datapoints)

cl <- parallel::makeCluster(parallel::detectCores())

g <- parallel::parLapply(cl, theta_p, StartNetwork::KL_ss, theta_s = theta_s, replicates = replicates, sorted = TRUE, mech_net = mech_net_gnp , lstat = igraph::gsize, mirror = TRUE, type = c("Bianconi", "Liebenau"), ergm = FALSE)

parallel::stopCluster(cl)

df <- StartNetwork::tidy_g(g, tidy = FALSE)

saveRDS(df, "edges0.Rds")

sessionInfo()
