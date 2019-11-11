
library(StartNetwork)
library(parallel)
library(ggplot2)

# Example 1 -----------------
# mechanistic model: Barabàsi Albert
# network model: ERGM
# integral stat: sorted degree sequence
# likelihood stat: number of 6 stars

theta_m <- rep(seq(5, 12, by = 1), 5)

cl <- makeCluster(detectCores())

theta_s = 0.5
n = 25
replicates = 2000

mech_net = function(m, n, args){args$m = m; args$n = n; do.call(igraph::sample_pa, args)}
mech_args = list(directed = FALSE)
lstat = function(x){sum(igraph::degree(x) == 6)}

g <- parSapply(cl, theta_m, KL_net, theta_s = theta_s, n = n, mech_net = mech_net, mech_args = mech_args, replicates = replicates, lstat = lstat, sorted = TRUE)

df <- data.frame(parameter = theta_m, KL = g)

ggplot(df) + aes(x = parameter, y = KL, group = parameter) + geom_boxplot()

# Example 2 -----------------
# mechanistic model: Barabàsi Albert
# network model: ERGM
# integral stat: sorted degree sequence
# likelihood stat: number of triangles

lstat = function(x){length(igraph::triangles(x))/3}

g <- parSapply(cl, theta_m, KL_net, theta_s = 0.01, n = n, mech_net = mech_net, mech_args = mech_args, replicates = replicates, lstat = lstat, sorted = TRUE)

df <- data.frame(parameter = theta_m, KL = g)

ggplot(df) + aes(x = parameter, y = KL, group = parameter) + geom_boxplot()

# Example 3 -----------------
# mechanistic model: Barabàsi Albert
# network model: ERGM
# integral stat: sorted degree sequence
# likelihood stat: number of triangles and number of six stars


lstat = function(x){c(length(igraph::triangles(x))/3, sum(igraph::degree(x) == 6))}

g <- parSapply(cl, theta_m, KL_net, theta_s = c(0.01, 0.5), n = n, mech_net = mech_net, mech_args = mech_args, replicates = replicates, lstat = lstat, sorted = TRUE)

df <- data.frame(parameter = theta_m, KL = g)

ggplot(df) + aes(x = parameter, y = KL, group = parameter) + geom_boxplot()
