# Example 1 -----------------
# mechanistic model: Barabàsi Albert
# network model: ERGM
# integral stat: sorted degree sequence
# likelihood stat: number of 6 stars

load("init.RData")

library(StartNetwork)
library(parallel)
library(ggplot2)

cl <- makeCluster(detectCores())

lstat = function(x){sum(igraph::degree(x) == 6)}

g <- parSapply(cl, theta_m, KL_net, theta_s = theta_s[1], n = n, mech_net = mech_net_ba, mech_args = mech_args_ba, replicates = replicates, lstat = lstat, sorted = TRUE)

df <- data.frame(parameter = theta_m, KL = g)

ggplot(df) + aes(x = parameter, y = KL, group = parameter) + geom_boxplot() + ggtitle("Example 1")

ggsave(filename = "example1.pdf")

save.image("example1.RData")
