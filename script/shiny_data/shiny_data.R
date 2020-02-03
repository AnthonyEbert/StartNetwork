set.seed(1)

library(StartNetwork)
library(parallel)
library(ggplot2)

n = 8
replicates = 400
datapoints = 5

mech_net_triangles <- purrr::partial(mech_net_triangles_n, n = !!n)

true_value <- 0.5
theta_p <- rep(seq(0.15, 0.40, by = 0.05), 5)
theta_s <- log(true_value/(1 - true_value))

cl <- parallel::makeCluster(parallel::detectCores())

g <- parallel::parLapply(cl, theta_p, StartNetwork::KL_ss, theta_s = theta_s, replicates = replicates, sorted = TRUE, mech_net = mech_net_triangles, lstat = function(x){length(igraph::triangles(x))/3}, mirror = TRUE, type = c("Bianconi", "Liebenau"), degree_triangle = TRUE, entropy_ss = TRUE)

parallel::stopCluster(cl)

df <- StartNetwork::tidy_g(g, tidy = FALSE)

saveRDS(df, "triangle7.Rds")

sessionInfo()
