
library(StartNetwork)
library(parallel)
library(ggplot2)

n = 21
replicates = 2000

mech_net_ba = function(m, n, args){args$m = m; args$n = n; do.call(igraph::sample_pa, args)}
mech_args_ba = list(directed = FALSE)

mech_net_gnp = function(p, n, args){
  args$p = p;
  args$n = n;
  do.call(igraph::sample_gnp, args)
}

theta_m <- rep(seq(1, 10, by = 1), 5)
theta_p <- rep(seq(0.1, 0.9, by = 0.05), 5)
theta_s <- c(3.5, 0.02)
theta_coef2 <- rep(seq(-0.03, 0.04, by = 0.01), 5)

sessionInfo()

save.image(file = "init.RData")
