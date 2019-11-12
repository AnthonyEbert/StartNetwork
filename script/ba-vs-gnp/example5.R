# Example 5 -----------------
# mechanistic model: ERGM
# network model: ERGM
# integral stat: sorted degree sequence
# likelihood stat: number of triangles and number of six stars

load("init.RData")

library(StartNetwork)
library(parallel)
library(ggplot2)
library(ergm)

cl <- makeCluster(detectCores())

lstat = function(x){c(sum(I(as.numeric(igraph::degree(x)) / 2) == 6), length(igraph::triangles(x))/3)}

mech_net_ergm = function(coef2, n, args, nsim = 1){
  args$coef = c(args$coef1, coef2);
  args$n = n;
  ergg <- ergm::simulate_formula(network::network(args$n, directed = FALSE) ~ degree(6) + triangles, coef = args$coef, nsim = nsim, control = ergm::control.simulate.formula.ergm(MCMC.burnin = 400000, MCMC.interval = 1000, MCMC.prop.weights = "random"))
  return(ergg)
}

mech_args_ergm = list(coef1 = theta_s[1])

g <- parSapply(cl, theta_coef2, KL_net, theta_s = theta_s, n = n, mech_net = mech_net_ergm, mech_args = mech_args_ergm, replicates = 10000, lstat = lstat, sorted = TRUE, lapply_opt = TRUE)

df <- data.frame(parameter = theta_coef2, KL = g)

ggplot(df) + aes(x = parameter, y = KL, group = parameter) + geom_boxplot() + ggtitle("Example 5")

ggsave(filename = "example5.pdf")

save.image("example5.RData")
