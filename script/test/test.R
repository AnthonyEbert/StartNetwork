
library(StartNetwork)
library(parallel)
library(ggplot2)

n = 15
theta_s = 0.405
theta_p = rep(seq(0.1, 0.9, by = 0.1), each = 5)
replicates = 2000

lstat = function(x){igraph::gsize(x)}

mech_net_gnp = function(p, n, args){
  args$p = p;
  args$n = n;
  do.call(igraph::sample_gnp, args)
}

mech_args_gnp = list(directed = FALSE, loops = FALSE)

cl <- makeCluster(detectCores())

g <- parSapply(cl, theta_p, KL_net, theta_s = theta_s, n = n, mech_net = mech_net_gnp, mech_args = mech_args_gnp, replicates = replicates, lstat = lstat, sorted = TRUE)

df <- data.frame(parameter = theta_p, KL = g)

ggplot(df) + aes(x = theta_p, y = KL, group = theta_p) + geom_boxplot() + ggtitle("Example 5")

parallel::stopCluster(cl)
