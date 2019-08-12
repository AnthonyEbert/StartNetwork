
library(igraph)

n <- 1000

# Erdos - Renyi

p <- 0.01

er <- igraph::sample_gnp(n = n, p = p, directed = FALSE, loops = FALSE)
netw_ss(er)

# Barabasi - Albert

ba <- sample_pa(n = n, directed = FALSE, zero.appeal = 1e-3)
netw_ss(ba)

# Stochastic block model

pref.matrix <- cbind( c(.01, .001), c(.001, .01) )
sbm <- sample_sbm(n = n, pref.matrix = pref.matrix, block.sizes = c(n/2, n/2))
netw_ss(sbm)







