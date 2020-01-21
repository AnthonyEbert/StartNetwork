set.seed(1)

library(StartNetwork)
library(parallel)
library(ggplot2)

n = 15
replicates = 400
datapoints = 10

mech_net_altkstars <- purrr::partial(sim_ergm_altkstar, n = !!n)

theta_s <- 0.5
theta_p <- rep(seq(-1.5, 1.5, by = 0.1), datapoints)

cl <- parallel::makeCluster(parallel::detectCores())

g <- parallel::parLapply(cl, theta_p, StartNetwork::KL_ss, theta_s = theta_s, replicates = replicates, sorted = TRUE, mech_net = mech_net_altkstars, lstat = function(x){as.numeric(ergm::summary_formula(x ~ altkstar(log(10), fixed = TRUE)))}, mirror = TRUE, type = c("Bianconi", "Liebenau"), ergm = TRUE)

parallel::stopCluster(cl)

df <- StartNetwork::tidy_g(g, tidy = FALSE)

saveRDS(df, "altkstars2.Rds")

sessionInfo()
