set.seed(1)

library(StartNetwork)
library(parallel)
library(ggplot2)

n = 15
replicates = 400
datapoints = 10

sim_ergm_cycle <- function(p, n, nsim = 1, ...){
  ergg <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ cycle(4), coef = c(p), nsim = nsim, ...)
  return(ergg)
}

mech_net_cycle <- purrr::partial(sim_ergm_cycle, n = !!n)

theta_s <- 0.5
theta_p <- rep(seq(-1.5, 1.5, by = 0.5), datapoints)

cl <- parallel::makeCluster(parallel::detectCores())

g <- parallel::parLapply(cl, theta_p, StartNetwork::KL_ss, theta_s = theta_s, replicates = replicates, sorted = TRUE, mech_net = mech_net_cycle , lstat = function(x){as.numeric(ergm::summary_formula(x ~ cycle(4)))}, mirror = TRUE, type = c("Bianconi", "Liebenau"), ergm = TRUE)

parallel::stopCluster(cl)

df <- StartNetwork::tidy_g(g, tidy = FALSE)

saveRDS(df, "cycle4_0.Rds")

sessionInfo()
