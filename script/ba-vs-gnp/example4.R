# Example 4 -----------------
# mechanistic model: GNP
# network model: ERGM
# integral stat: sorted degree sequence
# likelihood stat: number of triangles and number of six stars

load("init.RData")

library(StartNetwork)
library(parallel)
library(ggplot2)

cl <- makeCluster(detectCores())

lstat = function(x){c(sum(igraph::degree(x) == 6), length(igraph::triangles(x))/3)}

g <- parSapply(cl, theta_p, KL_net, theta_s = theta_s, n = n, mech_net = mech_net_gnp, mech_args = mech_args_ba, replicates = replicates, lstat = lstat, sorted = TRUE)

df <- data.frame(parameter = theta_p, KL = g)

ggplot(df) + aes(x = parameter, y = KL, group = parameter) + geom_boxplot() + ggtitle("Example 4")

ggsave(filename = "example4.pdf")

save.image("example4.RData")
