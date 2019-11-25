library(ergm)
library(StartNetwork)
set.seed(2)

mech_net_ba = function(m, n, args){args$m = m; args$n = n; do.call(igraph::sample_pa, args)}
mech_args_ba = list(directed = FALSE)

n = 16
replicates = 200

theta_m <- rep(seq(1, 10, by = 1), 5)
theta_p <- rep(seq(0.1, 0.9, by = 0.05), 5)
theta_s <- c(2, -1)
theta_coef2 <- rep(seq(-2, 0, by = 1), 5)

lstat = function(x){c(sum(igraph::degree(x) == 6), length(igraph::triangles(x))/3)}

ergm_x = ergm::simulate_formula(network::network(n, directed = FALSE) ~ degree(6) + triangles, coef = theta_s)

ergm_out = ergm(ergm_x ~ degree(6) + triangle)

summary(ergm_out)

summary(ergm_x ~ degree(6) + triangle)

ergm_out

StartNetwork::KL_net(theta_m[1], n = n, theta_s = theta_s, replicates = replicates, sorted = TRUE, mech_net = mech_net_ba, mech_args = mech_args_ba, lstat = lstat)



library(StartNetwork)
library(parallel)
library(ggplot2)
library(ergm)

cl <- makeCluster(detectCores())

lstat = function(x){c(sum(I(as.numeric(igraph::degree(x)) / 2) == 6), length(igraph::triangles(x))/3)}

mech_net_ergm = function(coef2, n, args, nsim = 1){
  args$coef = c(args$coef1, coef2);
  args$n = n;
  ergg <- ergm::simulate_formula(network::network(args$n, directed = FALSE) ~ degree(6) + triangles, coef = args$coef, nsim = nsim)
  return(ergg)
}

mech_args_ergm = list(coef1 = theta_s[1])

g <- parSapply(cl, theta_coef2, KL_net, theta_s = theta_s, n = n, mech_net = mech_net_ergm, mech_args = mech_args_ergm, replicates = replicates, lstat = lstat, sorted = TRUE)

df <- data.frame(parameter = theta_coef2, KL = g)

ggplot(df) + aes(x = parameter, y = KL, group = parameter) + geom_boxplot() + ggtitle("Example 5")

StartNetwork::KL_net(theta_m[1], n = n, theta_s = theta_s, replicates = replicates, sorted = TRUE, mech_net = mech_net_ba, mech_args = mech_args_ba, lstat = lstat)


library(StartNetwork)
library(dplyr)
library(parallel)

theta_m = 2

mech_net = function(m, n, args){args$m = m; args$n = n; do.call(igraph::sample_pa, args)}
mech_args = list(directed = FALSE)

x = KL_net(
  theta_m = theta_m,
  theta_s = 1,
  n = n,
  mech_net = mech_net,
  mech_args = mech_args,
  lstat = lstat,
  ds_return = TRUE,
  replicates = 10000
)

x_hash = sapply(x, digest::digest, algo = "md5")

x_table = table(x_hash)
x_df = as.data.frame(x_table)
names(x_df) <- c("hash", "Freq_x")
x_df$Freq_x <- x_df$Freq_x / sum(x_df$Freq_x)

KL_net(theta_m = theta_s, theta_s = c(1,1), n = n, mech_net = mech_net_ergm, mech_args = mech_args_ergm, lstat = lstat, ds_return = FALSE, replicates = 1000)


