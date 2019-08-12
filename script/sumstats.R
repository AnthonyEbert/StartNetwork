
library(igraph)
library(dplyr)

n <- 1000
ni <- 1000

# Erdos - Renyi "gnp"

p <- runif(ni, min = 1e-3, 1e-2)
gnp_df <- cbind(p, t(sapply(p, netw_ss_sim, n = n))) %>% as.data.frame()
gnp_lm <- lm(formula = p ~ edges + twostar + threestar + triangles + poisson_est, data = gnp_df)
summary(gnp_lm)

# Barabasi - Albert "pa"

power <- runif(ni, min = 0.8, 1.2)
pa_df <- cbind(power, t(sapply(power, netw_ss_sim, n = n, type = "pa"))) %>% as.data.frame()
pa_lm <- lm(formula = power ~ edges + twostar + threestar + triangles + poisson_est, data = pa_df)
summary(pa_lm)

# Stochastic block model "sbm"

block_p <- 10^runif(ni, -2.5, -2)
sbm_df <- cbind(block_p, t(sapply(block_p, netw_ss_sim, n = n, type = "sbm"))) %>% as.data.frame()
sbm_lm <- lm(formula = block_p ~ edges + twostar + threestar + triangles + poisson_est, data = sbm_df)
summary(sbm_lm)

# Small world network

p <- 10^runif(ni, -2, -1)
smallworld_df <- cbind(p, t(sapply(p, netw_ss_sim, n = n, type = "smallworld"))) %>% as.data.frame()
smallworld_lm <- lm(formula = p ~ edges + twostar + threestar + triangles + poisson_est, data = smallworld_df)
summary(smallworld_lm)




