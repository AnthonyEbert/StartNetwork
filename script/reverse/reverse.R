
library(StartNetwork)
library(dplyr)
library(parallel)

theta_m = 5
theta_s = 1
n = 21

mech_net = function(m, n, args){args$m = m; args$n = n; do.call(igraph::sample_pa, args)}
mech_args = list(directed = FALSE)
lstat = function(x){sum(igraph::degree(x) == 6)}

cl <- makeCluster(detectCores())

x = KL_net(
  theta_m = theta_m,
  theta_s = theta_s,
  n = n,
  mech_net = mech_net,
  mech_args = mech_args,
  lstat = lstat,
  ds_return = TRUE,
  replicates = 100000, cl = cl
)

x_hash = sapply(x, digest::digest, algo = "md5")

x_table = table(x_hash)
x_df = as.data.frame(x_table)
names(x_df) <- c("hash", "Freq_x")
x_df$Freq_x <- x_df$Freq_x / sum(x_df$Freq_x)

y = KL_net(
  theta_m = theta_m,
  theta_s = theta_s,
  n = n,
  mech_net = mech_net,
  mech_args = mech_args,
  lstat = lstat,
  pmf = x_df,
  replicates = 1000
)


ergm_x = ergm::simulate_formula(network::network(16, directed = FALSE) ~ degree(6) + triangles, coef = c(2,-1))

ergm(ergm_x ~ degree(6) + triangle)




